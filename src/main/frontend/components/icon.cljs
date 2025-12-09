(ns frontend.components.icon
  (:require ["@emoji-mart/data" :as emoji-data]
            ["emoji-mart" :refer [SearchIndex]]
            [camel-snake-kebab.core :as csk]
            [cljs-bean.core :as bean]
            [clojure.string :as string]
            [frontend.colors :as colors]
            [frontend.config :as config]
            [frontend.db :as db]
            [frontend.db-mixins :as db-mixins]
            [frontend.db.model :as model]
            [frontend.handler.property.util :as pu]
            [frontend.search :as search]
            [frontend.state :as state]
            [frontend.storage :as storage]
            [frontend.ui :as ui]
            [frontend.util :as util]
            [goog.functions :refer [debounce]]
            [goog.object :as gobj]
            [logseq.db :as ldb]
            [logseq.shui.hooks :as hooks]
            [logseq.shui.ui :as shui]
            [medley.core :as medley]
            [promesa.core :as p]
            [rum.core :as rum]))

(defonce emojis (vals (bean/->clj (gobj/get emoji-data "emojis"))))

(declare normalize-icon)

(defn- convert-bg-color-to-rgba
  "Convert background color to rgba format with opacity ~0.314.
   Handles hex colors, CSS variables, and rgba colors."
  [backgroundColor]
  (cond
   ;; Hex color - convert to rgba with opacity
    (and (string? backgroundColor)
         (string/starts-with? backgroundColor "#")
         (= (count (string/replace backgroundColor #"^#" "")) 6))
    (let [hex (string/replace backgroundColor #"^#" "")
          r (js/parseInt (subs hex 0 2) 16)
          g (js/parseInt (subs hex 2 4) 16)
          b (js/parseInt (subs hex 4 6) 16)]
      (str "rgba(" r "," g "," b ",0.314)"))
   ;; Already rgba - update opacity to 0.314
    (and (string? backgroundColor)
         (string/includes? backgroundColor "rgba"))
    (string/replace backgroundColor #",\s*[\d.]+\)$" ",0.314)")
   ;; CSS variable - use color-mix to apply opacity
    (and (string? backgroundColor)
         (string/starts-with? backgroundColor "var("))
    (str "color-mix(in srgb, " backgroundColor " 31.4%, transparent)")
   ;; Default: use as-is (might be a color name or other format)
    :else backgroundColor))

(defn icon
  [icon' & [opts]]
  (let [normalized (or (normalize-icon icon') icon')
        color? (:color? opts)
        opts (dissoc opts :color?)
        item (cond
               ;; Unified shape format
               (and (map? normalized) (= :emoji (:type normalized)) (get-in normalized [:data :value]))
               [:span.ui__icon
                [:em-emoji (merge {:id (get-in normalized [:data :value])
                                   :style {:line-height 1}}
                                  opts)]]

               (and (map? normalized) (= :icon (:type normalized)) (get-in normalized [:data :value]))
               (ui/icon (get-in normalized [:data :value]) opts)

               (and (map? normalized) (= :text (:type normalized)) (get-in normalized [:data :value]))
               (let [text-value (get-in normalized [:data :value])
                     display-text (if (> (count text-value) 8)
                                    (subs text-value 0 8)
                                    text-value)]
                 [:span.ui__icon
                  [:span.text-sm.font-medium
                   {:style {:max-width "20px"
                            :overflow "hidden"
                            :text-overflow "clip"
                            :white-space "nowrap"}}
                   display-text]])

               (and (map? normalized) (= :avatar (:type normalized)) (get-in normalized [:data :value]))
               (let [avatar-value (get-in normalized [:data :value])
                     backgroundColor (or (get-in normalized [:data :backgroundColor])
                                         (colors/variable :indigo :09))
                     color (or (get-in normalized [:data :color])
                               (colors/variable :indigo :10 true))
                     display-text (subs avatar-value 0 (min 3 (count avatar-value)))
                     bg-color-rgba (convert-bg-color-to-rgba backgroundColor)]
                 (shui/avatar
                  {:class "w-5 h-5"}
                  (shui/avatar-fallback
                   {:style {:background-color bg-color-rgba
                            :font-size "11px"
                            :color color}}
                   display-text)))

               ;; Legacy format support (fallback if normalization failed)
               (and (map? icon') (= :emoji (:type icon')) (:id icon'))
               [:span.ui__icon
                [:em-emoji (merge {:id (:id icon')
                                   :style {:line-height 1}}
                                  opts)]]

               (and (map? icon') (= :tabler-icon (:type icon')) (:id icon'))
               (ui/icon (:id icon') opts)

               :else nil)]
    (when item
      (if color?
        [:span.inline-flex.items-center.ls-icon-color-wrap
         {:style {:color (or (get-in normalized [:data :color])
                             (some-> icon' :color)
                             "inherit")}} item]
        item))))

(defn get-node-icon
  [node-entity]
  (let [first-tag-icon (some :logseq.property/icon (sort-by :db/id (:block/tags node-entity)))]
    (or (get node-entity (pu/get-pid :logseq.property/icon))
        (let [asset-type (:logseq.property.asset/type node-entity)]
          (cond
            (some? first-tag-icon)
            first-tag-icon
            (ldb/class? node-entity)
            "hash"
            (ldb/property? node-entity)
            "letter-p"
            (ldb/whiteboard? node-entity)
            "writing"
            (ldb/page? node-entity)
            "file"
            (= asset-type "pdf")
            "book"
            :else
            "letter-n")))))

(rum/defc get-node-icon-cp < rum/reactive db-mixins/query
  [node-entity opts]
  (let [;; Get fresh entity using db/sub-block to make it reactive to property changes
        ;; Only subscribe if we have a db-id (optimization: avoid unnecessary subscriptions)
        fresh-entity (when-let [db-id (:db/id node-entity)]
                       (or (model/sub-block db-id) node-entity))
        entity (or fresh-entity node-entity)
        opts' (merge {:size 14} opts)
        node-icon (cond
                    (:own-icon? opts)
                    (get entity (pu/get-pid :logseq.property/icon))
                    (:link? opts)
                    "arrow-narrow-right"
                    :else
                    (get-node-icon entity))]
    (when-not (or (string/blank? node-icon) (and (contains? #{"letter-n" "file"} node-icon) (:not-text-or-page? opts)))
      [:div.icon-cp-container.flex.items-center
       (merge {:style {:color (or (:color node-icon) "inherit")}}
              (select-keys opts [:class]))
       (icon node-icon opts')])))

(defn- emoji-char?
  "Check if a string is a single emoji character by checking against known emojis"
  [s]
  (and (string? s)
       (not (string/blank? s))
       (<= (count s) 2) ; emojis are typically 1-2 code units
       (some #(= (:id %) s) emojis)))

(defn- guess-from-value
  "Attempt to guess icon type from map value when type is unknown"
  [m]
  (let [value (or (:value m) (:id m))]
    (when (string? value)
      (if (emoji-char? value)
        {:type :emoji
         :id (str "emoji-" value)
         :label value
         :data {:value value}}
        {:type :icon
         :id (str "icon-" value)
         :label value
         :data {:value value}}))))

(defn normalize-icon
  "Convert various icon formats to unified icon-item shape:
   {:id string, :type :emoji|:icon|:text|:avatar, :label string, :data {:value string, :color string (optional), :backgroundColor string (optional)}}"
  [v]
  (cond
    ;; Already unified shape? (has :data key)
    (and (map? v) (keyword? (:type v)) (contains? v :data)) v

    ;; Legacy map with :type
    (map? v)
    (let [type-kw (cond
                    (keyword? (:type v)) (:type v)
                    (string? (:type v)) (keyword (:type v))
                    :else nil)
          id (or (:id v) (:value v))
          value (or (:value v) (:id v))
          color (:color v)
          label (or (:name v) (:label v) value)]
      (case type-kw
        :emoji {:type :emoji
                :id (or id (str "emoji-" value))
                :label (or label value)
                :data {:value value}}
        :tabler-icon {:type :icon
                      :id (or id (str "icon-" value))
                      :label (or label value)
                      :data (cond-> {:value value}
                              color (assoc :color color))}
        :icon {:type :icon
               :id (or id (str "icon-" value))
               :label (or label value)
               :data (cond-> {:value value}
                       color (assoc :color color))}
        :text {:type :text
               :id (or id (str "text-" value))
               :label (or label value)
               :data {:value value}}
        :avatar (let [backgroundColor (or (:backgroundColor v)
                                          (colors/variable :indigo :09))
                      color (or (:color v)
                                (colors/variable :indigo :10 true))]
                  {:type :avatar
                   :id (or id (str "avatar-" value))
                   :label (or label value)
                   :data {:value value
                          :backgroundColor backgroundColor
                          :color color}})
        ;; Fallback: try to guess from value
        (or (guess-from-value v)
            {:type :icon
             :id (str "icon-" (or value "unknown"))
             :label (or label value "unknown")
             :data {:value (or value "")}})))

    ;; Plain string: detect emoji vs icon name
    (string? v)
    (if (emoji-char? v)
      {:type :emoji
       :id (str "emoji-" v)
       :label v
       :data {:value v}}
      {:type :icon
       :id (str "icon-" v)
       :label v
       :data {:value v}})

    :else nil))

(defn- search-emojis
  [q]
  (p/let [result (.search SearchIndex q)]
    (->> (bean/->clj result)
         (map (fn [emoji]
                {:type :emoji
                 :id (:id emoji)
                 :label (or (:name emoji) (:id emoji))
                 :data {:value (:id emoji)}})))))

(defonce *tabler-icons (atom nil))
(defn- get-tabler-icons
  []
  (if @*tabler-icons
    @*tabler-icons
    (let [result (->> (keys (bean/->clj js/tablerIcons))
                      (map (fn [k]
                             (-> (string/replace (csk/->Camel_Snake_Case (name k)) "_" " ")
                                 (string/replace-first "Icon " ""))))
                   ;; FIXME: somehow those icons don't work
                      (remove #{"Ab" "Ab 2" "Ab Off"}))]
      (reset! *tabler-icons result)
      result)))

(defn- search-tabler-icons
  [q]
  (->> (search/fuzzy-search (get-tabler-icons) q :limit 100)
       (map (fn [icon-name]
              {:type :icon
               :id (str "icon-" icon-name)
               :label icon-name
               :data {:value icon-name}}))))

(defn- search
  [q tab]
  (p/let [icons (when (not= tab :emoji) (search-tabler-icons q))
          emojis' (when (not= tab :icon) (search-emojis q))]
    {:icons icons
     :emojis emojis'}))

(rum/defc icons-row
  [items]
  [:div.its.icons-row items])

(rum/defc icon-cp < rum/static
  [icon-item {:keys [on-chosen hover]}]
  (let [icon-id (get-in icon-item [:data :value])
        icon-name (or (:label icon-item) icon-id)
        color (get-in icon-item [:data :color])
        icon-id' (when icon-id (cond-> icon-id (string? icon-id) (string/replace " " "")))]
    [:button.w-9.h-9.transition-opacity
     (when icon-id'
       {:key icon-id'
        :tabIndex "0"
        :title icon-name
        :on-click (fn [e]
                    (on-chosen e {:type :tabler-icon
                                  :id icon-id'
                                  :name icon-name
                                  :color color}))
        :on-mouse-over #(some-> hover
                                (reset! {:type :tabler-icon
                                         :id icon-id'
                                         :name icon-name
                                         :icon icon-id'
                                         :color color}))
        :on-mouse-out #()})
     (when icon-id'
       (ui/icon icon-id' {:size 24}))]))

(rum/defc emoji-cp < rum/static
  [icon-item {:keys [on-chosen hover]}]
  (let [emoji-id (get-in icon-item [:data :value])
        emoji-name (or (:label icon-item) emoji-id)]
    [:button.text-2xl.w-9.h-9.transition-opacity
     (cond->
      {:tabIndex "0"
       :title emoji-name
       :on-click (fn [e]
                   (on-chosen e {:type :emoji
                                 :id emoji-id
                                 :name emoji-name}))}
       (not (nil? hover))
       (assoc :on-mouse-over #(reset! hover {:type :emoji
                                             :id emoji-id
                                             :name emoji-name})
              :on-mouse-out #()))
     [:em-emoji {:id emoji-id
                 :style {:line-height 1}}]]))

(rum/defc text-cp < rum/static
  [icon-item {:keys [on-chosen hover]}]
  (let [text-value (get-in icon-item [:data :value])
        display-text (if (> (count text-value) 8)
                       (subs text-value 0 8)
                       text-value)]
    [:button.w-9.h-9.transition-opacity.text-sm.font-medium
     (cond->
      {:tabIndex "0"
       :title text-value
       :on-click (fn [e]
                   (on-chosen e {:type "text"
                                 :value text-value}))}
       (not (nil? hover))
       (assoc :on-mouse-over #(reset! hover {:type :text
                                             :value text-value})
              :on-mouse-out #()))
     display-text]))

(rum/defc avatar-cp < rum/static
  [icon-item {:keys [on-chosen hover]}]
  (let [avatar-value (get-in icon-item [:data :value])
        backgroundColor (or (get-in icon-item [:data :backgroundColor])
                            (colors/variable :indigo :09))
        color (or (get-in icon-item [:data :color])
                  (colors/variable :indigo :10 true))
        display-text (subs avatar-value 0 (min 3 (count avatar-value)))
        bg-color-rgba (convert-bg-color-to-rgba backgroundColor)]
    [:button.transition-opacity
     (cond->
      {:tabIndex "0"
       :title avatar-value
       :class "p-0 border-0 bg-transparent cursor-pointer"
       :on-click (fn [e]
                   (on-chosen e {:type "avatar"
                                 :value avatar-value
                                 :backgroundColor backgroundColor
                                 :color color}))}
       (not (nil? hover))
       (assoc :on-mouse-over #(reset! hover {:type :avatar
                                             :value avatar-value
                                             :backgroundColor backgroundColor
                                             :color color})
              :on-mouse-out #()))
     (shui/avatar
      {:class "w-5 h-5"}
      (shui/avatar-fallback
       {:style {:background-color bg-color-rgba
                :font-size "11px"
                :color color}}
       display-text))]))

(defn render-item
  "Render an icon-item based on its type"
  [icon-item opts]
  (case (:type icon-item)
    :emoji (emoji-cp icon-item opts)
    :icon (icon-cp icon-item opts)
    :text (text-cp icon-item opts)
    :avatar (avatar-cp icon-item opts)
    nil))

(defn item-render
  [item opts]
  (if (map? item)
    (render-item item opts)
    ;; Legacy support: handle raw strings/old formats
    (let [normalized (normalize-icon item)]
      (if normalized
        (render-item normalized opts)
        nil))))

(rum/defc pane-section
  [label icon-items & {:keys [searching? virtual-list? render-item-fn]
                       :or {virtual-list? true}
                       :as opts}]
  (let [*el-ref (rum/use-ref nil)
        render-fn (or render-item-fn render-item)]
    [:div.pane-section
     {:ref *el-ref
      :class (util/classnames
              [{:has-virtual-list virtual-list?
                :searching-result searching?}])}
     [:div.hd.px-1.pb-1.leading-none
      [:strong.text-xs.font-medium.text-gray-07.dark:opacity-80 label]]
     (if virtual-list?
       (let [total (count icon-items)
             step 9
             rows (quot total step)
             mods (mod total step)
             rows (if (zero? mods) rows (inc rows))
             items (vec icon-items)]
         (ui/virtualized-list
          (cond-> {:total-count rows
                   :item-content (fn [idx]
                                   (icons-row
                                    (let [last? (= (dec rows) idx)
                                          start (* idx step)
                                          end (* (inc idx) (if (and last? (not (zero? mods))) mods step))
                                          icons (try (subvec items start end)
                                                     (catch js/Error e
                                                       (js/console.error e)
                                                       nil))]
                                      (mapv #(render-fn % opts) icons))))}

            searching?
            (assoc :custom-scroll-parent (some-> (rum/deref *el-ref) (.closest ".bd-scroll"))))))
       [:div.its
        (map #(render-fn % opts) icon-items)])]))

(rum/defc emojis-cp < rum/static
  [emojis* opts]
  (let [icon-items (map (fn [emoji]
                          {:type :emoji
                           :id (:id emoji)
                           :label (or (:name emoji) (:id emoji))
                           :data {:value (:id emoji)}})
                        emojis*)]
    (pane-section
     (util/format "Emojis (%s)" (count emojis*))
     icon-items
     opts)))

(rum/defc icons-cp < rum/static
  [icons opts]
  (let [icon-items (map (fn [icon-name]
                          {:type :icon
                           :id (str "icon-" icon-name)
                           :label icon-name
                           :data {:value icon-name}})
                        icons)]
    (pane-section
     (util/format "Icons (%s)" (count icons))
     icon-items
     opts)))

(defn get-used-items
  []
  (let [v2-items (storage/get :ui/ls-icons-used-v2)]
    (if (seq v2-items)
      v2-items
      ;; Migrate from legacy format
      (let [legacy-items (storage/get :ui/ls-icons-used)]
        (if (seq legacy-items)
          (let [normalized (map normalize-icon legacy-items)]
            (storage/set :ui/ls-icons-used-v2 normalized)
            normalized)
          [])))))

(defn add-used-item!
  [m]
  (let [normalized (normalize-icon m)
        s (some->> (or (get-used-items) [])
                   (take 24)
                   (filter #(not= normalized %))
                   (cons normalized))]
    (storage/set :ui/ls-icons-used-v2 s)))

(defn- derive-initials
  "Derive initials from a page title (max 8 chars)"
  [title]
  (when title
    (let [words (string/split (string/trim title) #"\s+")
          initials (if (> (count words) 1)
                     ;; Take first letter of first two words
                     (str (subs (first words) 0 1)
                          (subs (second words) 0 1))
                     ;; Single word: take first 2 chars
                     (subs (first words) 0 (min 2 (count (first words)))))]
      (subs initials 0 (min 8 (count initials))))))

(defn- derive-avatar-initials
  "Derive initials from a page title (max 2-3 chars for avatars)"
  [title]
  (when title
    (let [words (string/split (string/trim title) #"\s+")
          initials (if (> (count words) 1)
                     ;; Take first letter of first two words
                     (str (subs (first words) 0 1)
                          (subs (second words) 0 1))
                     ;; Single word: take first 2-3 chars
                     (subs (first words) 0 (min 3 (count (first words)))))]
      (subs initials 0 (min 3 (count initials))))))

(rum/defc text-tab-cp
  [*q page-title opts]
  (let [query @*q
        text-value (if (string/blank? query)
                     ;; Use page-title or fallback to current page
                     (let [title (or page-title
                                     (some-> (state/get-current-page)
                                             (db/get-page)
                                             (:block/title)))]
                       (derive-initials title))
                     ;; Use query (max 8 chars)
                     (subs query 0 (min 8 (count query))))
        icon-item (when text-value
                    {:type :text
                     :id (str "text-" text-value)
                     :label text-value
                     :data {:value text-value}})]
    (if icon-item
      (pane-section "Text" [icon-item] (assoc opts :virtual-list? false))
      [:div.pane-section.px-2.py-4
       [:div.text-sm.text-gray-07.dark:opacity-80
        "Enter text or use page initials"]])))

(rum/defc avatar-tab-cp
  [*q page-title opts]
  (let [query @*q
        avatar-value (if (string/blank? query)
                       ;; Use page-title or fallback to current page
                       (let [title (or page-title
                                       (some-> (state/get-current-page)
                                               (db/get-page)
                                               (:block/title)))]
                         (derive-avatar-initials title))
                       ;; Use query (max 2-3 chars)
                       (subs query 0 (min 3 (count query))))
        backgroundColor (colors/variable :indigo :09)
        color (colors/variable :indigo :10 true)
        icon-item (when avatar-value
                    {:type :avatar
                     :id (str "avatar-" avatar-value)
                     :label avatar-value
                     :data {:value avatar-value
                            :backgroundColor backgroundColor
                            :color color}})]
    (if icon-item
      (pane-section "Avatar" [icon-item] (assoc opts :virtual-list? false))
      [:div.pane-section.px-2.py-4
       [:div.text-sm.text-gray-07.dark:opacity-80
        "Enter initials or use page initials"]])))

(rum/defc all-cp
  [opts]
  (let [used-items (get-used-items)
        emoji-items (->> (take 32 emojis)
                         (map (fn [emoji]
                                {:type :emoji
                                 :id (:id emoji)
                                 :label (or (:name emoji) (:id emoji))
                                 :data {:value (:id emoji)}})))
        icon-items (->> (take 48 (get-tabler-icons))
                        (map (fn [icon-name]
                               {:type :icon
                                :id (str "icon-" icon-name)
                                :label icon-name
                                :data {:value icon-name}})))
        opts (assoc opts :virtual-list? false)]
    [:div.all-pane.pb-10
     (when (seq used-items)
       (pane-section "Frequently used" used-items opts))
     (pane-section (util/format "Emojis (%s)" (count emojis))
                   emoji-items
                   opts)
     (pane-section (util/format "Icons (%s)" (count (get-tabler-icons)))
                   icon-items
                   opts)]))

(rum/defc tab-observer
  [tab {:keys [reset-q!]}]
  (hooks/use-effect!
   #(reset-q!)
   [tab])
  nil)

(rum/defc select-observer
  [*input-ref]
  (let [*el-ref (rum/use-ref nil)
        *items-ref (rum/use-ref [])
        *current-ref (rum/use-ref [-1])
        set-current! (fn [idx node] (set! (. *current-ref -current) [idx node]))
        get-cnt #(some-> (rum/deref *el-ref) (.closest ".cp__emoji-icon-picker"))
        focus! (fn [idx dir]
                 (let [items (rum/deref *items-ref)
                       ^js popup (some-> (get-cnt) (.-parentNode))
                       idx (loop [n idx]
                             (if (false? (nth items n nil))
                               (recur (+ n (if (= dir :prev) -1 1))) n))]
                   (if-let [node (nth items idx nil)]
                     (do (.focus node #js {:preventScroll true :focusVisible true})
                         (.scrollIntoView node #js {:block "center"})
                         (when popup (set! (. popup -scrollTop) 0))
                         (set-current! idx node))
                     (do (.focus (rum/deref *input-ref)) (set-current! -1 nil)))))
        down-handler!
        (hooks/use-callback
         (fn [^js e]
           (let []
             (if (= 13 (.-keyCode e))
                ;; enter
               (some-> (second (rum/deref *current-ref)) (.click))
               (let [[idx _node] (rum/deref *current-ref)]
                 (case (.-keyCode e)
                    ;;left
                   37 (focus! (dec idx) :prev)
                    ;; tab & right
                   (9 39) (focus! (inc idx) :next)
                    ;; up
                   38 (do (focus! (- idx 9) :prev) (util/stop e))
                    ;; down
                   40 (do (focus! (+ idx 9) :next) (util/stop e))
                   :dune))))) [])]

    (hooks/use-effect!
     (fn []
        ;; calculate items
       (let [^js sections (.querySelectorAll (get-cnt) ".pane-section")
             items (map #(some-> (.querySelectorAll % ".its > button") (js/Array.from) (js->clj)) sections)
             step 9
             items (map #(let [count (count %)
                               m (mod count step)]
                           (if (> m 0) (concat % (repeat (- step m) false)) %)) items)]
         (set! (. *items-ref -current) (flatten items))
         (focus! 0 :next))

        ;; handlers
       (let [^js cnt (get-cnt)]
         (.addEventListener cnt "keydown" down-handler! false)
         #(.removeEventListener cnt "keydown" down-handler!)))
     [])
    [:span.absolute.hidden {:ref *el-ref}]))

(rum/defc color-picker
  [*color on-select!]
  (let [[color, set-color!] (rum/use-state @*color)
        *el (rum/use-ref nil)
        content-fn (fn []
                     (let [colors ["#6e7b8b" "#5e69d2" "#00b5ed" "#00b55b"
                                   "#f2be00" "#e47a00" "#f38e81" "#fb434c" nil]]
                       [:div.color-picker-presets
                        (for [c colors]
                          (shui/button
                           {:on-click (fn [] (set-color! c)
                                        (some-> on-select! (apply [c]))
                                        (shui/popup-hide!))
                            :size :sm :variant :outline
                            :class "it" :style {:background-color c}}
                           (if c "" (shui/tabler-icon "minus" {:class "scale-75 opacity-70"}))))]))]
    (hooks/use-effect!
     (fn []
       (when-let [^js picker (some-> (rum/deref *el) (.closest ".cp__emoji-icon-picker"))]
         (let [color (if (string/blank? color) "inherit" color)]
           (.setProperty (.-style picker) "--ls-color-icon-preset" color)
           (storage/set :ls-icon-color-preset color)))
       (reset! *color color))
     [color])

    (shui/button {:size :sm
                  :ref *el
                  :class "color-picker"
                  :on-click (fn [^js e] (shui/popup-show! (.-target e) content-fn {:content-props {:side-offset 6}}))
                  :variant :outline}
                 [:strong {:style {:color (or color "inherit")}}
                  (shui/tabler-icon "palette")])))

(rum/defcs ^:large-vars/cleanup-todo icon-search <
  (rum/local "" ::q)
  (rum/local nil ::result)
  (rum/local false ::select-mode?)
  (rum/local :all ::tab)
  {:init (fn [s]
           (assoc s ::color (atom (storage/get :ls-icon-color-preset))))}
  [state {:keys [on-chosen del-btn? icon-value page-title] :as opts}]
  (let [*q (::q state)
        *result (::result state)
        *tab (::tab state)
        *color (::color state)
        *input-ref (rum/create-ref)
        *result-ref (rum/create-ref)
        result @*result
        normalized-icon-value (normalize-icon icon-value)
        opts (assoc opts
                    :on-chosen (fn [e m]
                                 (let [icon-item (normalize-icon m)
                                       icon? (= :icon (:type icon-item))
                                       avatar? (= :avatar (:type icon-item))
                                       m' (cond
                                            (and icon? (not (string/blank? @*color)))
                                            (assoc-in m [:data :color] @*color)
                                            avatar?
                                            ;; Ensure avatar has default colors if not provided
                                            (let [m-normalized (normalize-icon m)]
                                              (if (and (get-in m-normalized [:data :backgroundColor])
                                                       (get-in m-normalized [:data :color]))
                                                m
                                                (assoc m
                                                       :backgroundColor (get-in m-normalized [:data :backgroundColor] (colors/variable :indigo :09))
                                                       :color (get-in m-normalized [:data :color] (colors/variable :indigo :10 true)))))
                                            :else m)]
                                   (and on-chosen (on-chosen e m'))
                                   (when (:type icon-item) (add-used-item! icon-item)))))
        *select-mode? (::select-mode? state)
        reset-q! #(when-let [^js input (rum/deref *input-ref)]
                    (reset! *q "")
                    (reset! *result {})
                    (reset! *select-mode? false)
                    (set! (. input -value) "")
                    (util/schedule
                     (fn []
                       (when (not= js/document.activeElement input)
                         (.focus input))
                       (util/scroll-to (rum/deref *result-ref) 0 false))))]
    [:div.cp__emoji-icon-picker
     {:data-keep-selection true}
     ;; header
     [:div.hd.bg-popover
      (tab-observer @*tab {:reset-q! reset-q!})
      (when @*select-mode?
        (select-observer *input-ref))
      [:div.search-input
       (shui/tabler-icon "search" {:size 16})
       [(shui/input
         {:auto-focus true
          :ref *input-ref
          :placeholder (util/format "Search %s items" (string/lower-case (name @*tab)))
          :default-value ""
          :on-focus #(reset! *select-mode? false)
          :on-key-down (fn [^js e]
                         (case (.-keyCode e)
                            ;; esc
                           27 (do (util/stop e)
                                  (if (string/blank? @*q)
                                   ;(some-> (rum/deref *input-ref) (.blur))
                                    (shui/popup-hide!)
                                    (reset-q!)))
                           38 (do (util/stop e))
                           (9 40) (do
                                    (reset! *select-mode? true)
                                    (util/stop e))
                           :dune))
          :on-change (debounce
                      (fn [e]
                        (reset! *q (util/evalue e))
                        (reset! *select-mode? false)
                        (if (string/blank? @*q)
                          (reset! *result {})
                          (p/let [result (search @*q @*tab)]
                            (reset! *result result))))
                      200)})]
       (when-not (string/blank? @*q)
         [:a.x {:on-click reset-q!} (shui/tabler-icon "x" {:size 14})])]]
     ;; body
     [:div.bd.bd-scroll
      {:ref *result-ref
       :class (or (some-> @*tab (name)) "other")}
      [:div.content-pane
       (if (seq result)
         [:div.flex.flex-1.flex-col.gap-1.search-result
          (let [matched (concat (:emojis result) (:icons result))]
            (when (seq matched)
              (pane-section
               (util/format "Matched (%s)" (count matched))
               matched
               opts)))]
         [:div.flex.flex-1.flex-col.gap-1
          (case @*tab
            :emoji (emojis-cp emojis opts)
            :icon (icons-cp (get-tabler-icons) opts)
            :text (text-tab-cp *q page-title opts)
            :avatar (avatar-tab-cp *q page-title opts)
            (all-cp opts))])]]

     ;; footer
     [:div.ft
      ;; tabs
      [:<>
       [:div.flex.flex-1.flex-row.items-center.gap-2
        (let [tabs [[:all "All"] [:emoji "Emojis"] [:icon "Icons"] [:text "Text"] [:avatar "Avatar"]]]
          (for [[id label] tabs
                :let [active? (= @*tab id)]]
            (shui/button
             {:variant :ghost
              :size :sm
              :class (util/classnames [{:active active?} "tab-item"])
              :on-mouse-down (fn [e]
                               (util/stop e)
                               (reset! *tab id))}
             label)))]

       (when (and (not= :emoji @*tab) (not= :text @*tab) (not= :avatar @*tab))
         (color-picker *color (fn [c]
                                (when (= :icon (:type normalized-icon-value))
                                  (on-chosen nil (assoc-in normalized-icon-value [:data :color] c) true)))))

       ;; action buttons
       (when del-btn?
         (shui/button {:variant :outline :size :sm :data-action "del"
                       :on-click #(on-chosen nil)}
                      (shui/tabler-icon "trash" {:size 17})))]]]))

(rum/defc icon-picker
  [icon-value {:keys [empty-label disabled? initial-open? del-btn? on-chosen icon-props popup-opts button-opts page-title]}]
  (let [*trigger-ref (rum/use-ref nil)
        normalized-icon-value (normalize-icon icon-value)
        content-fn
        (if config/publishing?
          (constantly [])
          (fn [{:keys [id]}]
            (icon-search
             {:on-chosen (fn [e icon-value keep-popup?]
                           (on-chosen e icon-value)
                           (when-not (true? keep-popup?) (shui/popup-hide! id)))
              :icon-value normalized-icon-value
              :page-title page-title
              :del-btn? del-btn?})))]
    (hooks/use-effect!
     (fn []
       (when initial-open?
         (js/setTimeout #(some-> (rum/deref *trigger-ref) (.click)) 32)))
     [initial-open?])

    ;; trigger
    (let [has-icon? (some? icon-value)]
      (shui/button
       (merge
        {:ref *trigger-ref
         :variant :ghost
         :size :sm
         :class (if has-icon? "px-1 leading-none text-muted-foreground hover:text-foreground"
                    "font-normal text-sm px-[0.5px] text-muted-foreground hover:text-foreground")
         :on-click (fn [^js e]
                     (when-not disabled?
                       (shui/popup-show! (.-target e) content-fn
                                         (medley/deep-merge
                                          {:align :start
                                           :id :ls-icon-picker
                                           :content-props {:class "ls-icon-picker"
                                                           :onEscapeKeyDown #(.preventDefault %)}}
                                          popup-opts))))}
        button-opts)
       (if has-icon?
         (if (vector? icon-value) ; hiccup
           icon-value
           (icon icon-value (merge {:color? true} icon-props)))
         (or empty-label "Empty"))))))

