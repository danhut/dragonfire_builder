(ns df-front.core
  (:require
    [reagent.core :as r]
    [clojure.string :as s]
    [reagent-modals.modals :as reagent-modals]
    [secretary.core :as secretary :refer-macros [defroute]]
    [goog.events :as events]
    [goog.history.EventType :as EventType]
    [cljsjs.clipboard]
    [df-front.features :refer [features titles feature-map slot-list archetypes classes paths
                                 bonus-archetypes colour-map char-titles characters character-map packs]])
  (:import goog.History))

;; Initialise App Data
(defonce app-state (r/atom {:xp-earned 0 :xp-used 0 :xp-slots 0 :xp-features 0
                            :xp-filter false
                            :class "Fighter"
                            :archetype "Martial"
                            :archetype-bonus nil
                            :race "Human"
                            :packs (let [pax (js->clj (.getItem (.-localStorage js/window) "packs"))]
                                     (if (nil? pax) packs (s/split pax #",")))
                            :slot-cost {:slot0 0 :slot1 0 :slot2 5 :slot3 10 :slot4 15 :slot5 25 :slot6 40}
                            :name nil
                            :slot0 nil :slot1 nil :slot2 nil :slot3 nil :slot4 nil :slot5 nil :slot6 nil :fighting1 nil :fighting2 nil
                            :invocation1 nil :invocation2 nil :invocation3 nil :invocation4 nil}))

;; Builder functions
(defn get-paths
  "Gets list of features that specialise character"
  []
  (let [chosen (keep #(:name ((keyword %) @app-state)) slot-list)
        path-list (distinct (flatten (map #(second %) paths)))]
    (into [] (clojure.set/intersection (set chosen) (set path-list)))))

(defn get-archetypes
  "Gets current archetypes for characters"
  []
  (remove nil? [(:archetype @app-state) (:archetype-bonus @app-state)]))

(defn affordable?
  "Takes a list of features and removes those which cost too much XP"
  [features]
  (if (:xp-filter @app-state)
    (let [remaining (- (:xp-earned @app-state) (:xp-used @app-state))]
      (filter #(or (= "Background" (:xp %))(>= remaining (:xp %))) features))
    features))

(defn filter-features
  "Main filtering function when a feature slot button is pushed"
  [features slot]
  (let [features (filter #(some #{(:source %)} (-> @app-state :packs)) features) ;filter unused packs
        features (distinct (map #(dissoc % :source) features))  ;remove features in multiple packs
        existing (keep #((keyword %) @app-state) slot-list)
        features (into [] (clojure.set/difference (set features) (set existing)))
        background (when (= slot :slot1) (filter #(= "Background" (:xp %)) features))
        no-background (remove #(= "Background" (:xp %)) features)
        fighting (filter #(s/includes? (:name %) "Fighting Style") features)
        extra-slots-only (remove #(or (s/includes? (:name %) "Fighting Style")
                                      (s/includes? (:name %) "Patron")) features)
        patron (filter #(s/includes? (:name %) "Patron") features)
        no-patron (remove #(s/includes? (:name %) "Patron") features)
        invocations (filter #(s/includes? (:name %) "Invocation ") features)
        no-req (filter #(empty? (:requires %)) no-background)
        archetype (map #(filter (fn [arch] (s/includes? (:requires arch)
                                                        (str % " Class"))) extra-slots-only) (get-archetypes))
        path (map #(filter (fn [feature] (s/includes? (:requires feature) %)) features) (get-paths))
        no-path (remove #(s/includes? (:requires %) "&") extra-slots-only)
        class (filter #(s/includes? (:requires %) (str (:class @app-state) " Class")) no-path)
        ; hack to have Sun Elf recognised in feature requirements. Also data hacked have preceeding space
        myrace (if (s/includes? (:race @app-state) " Elf") " Elf" (:race @app-state))
        race (seq (filter #(s/includes? (:requires %) myrace) features))]

    (cond
      (or (= slot :slot0))
      (affordable? patron)

      (or (= slot :fighting1) (= slot :fighting2))
      (affordable? fighting)

      (or (= slot :invocation1) (= slot :invocation2) (= slot :invocation3) (= slot :invocation4))
      (affordable? invocations)

      :else
      (affordable? (remove #(nil? %) (flatten [race background no-req archetype class path]))))))

(defn update-used-xp
  "Updates amount of XP used by the build based on selections"
  []
  (let [slot-costs (reduce + (map #(if (% @app-state) (get-in @app-state [:slot-cost %]) 0) slot-list))
        feature-costs (reduce + (map #(let [fcost (js/parseInt (get-in @app-state [% :xp]))]
                                        (if (int? fcost) fcost 0)) slot-list))]
    (swap! app-state assoc :xp-slots slot-costs)
    (swap! app-state assoc :xp-features feature-costs)
    (swap! app-state assoc :xp-used (+ slot-costs feature-costs))))

(defn check-feature
  "Checks if a feature has been selected in the builder (by name)"
  [feature-name]
  (seq (filter #(= feature-name (get-in @app-state [% :name])) slot-list)))

(defn update-archetypes
  "Updates archetype based on applied features"
  []
  (let [mods (keys bonus-archetypes)]
    (swap! app-state assoc :archetype-bonus nil)
    (doseq [fname mods]
      (when (check-feature fname)
        (swap! app-state assoc :archetype-bonus (get-in bonus-archetypes [fname]))))))

(defn sticker-view
  "Shows the data for a given feature sticker"
  ([fmap] (sticker-view fmap nil))
  ([fmap style]
  (let [fname (s/replace (:name fmap) "Fighting Style" "")]
  [:div
   [:div.title fname]
   [:div.req (:requires fmap)]
   [:div.description (:description fmap)]
   [(keyword (str "div.xp" style)) (:xp fmap) (when-not (= "Background" (:xp fmap)) " XP")]])))

(defn sticker-select
  "Builds the view of a given feature sticker on modal"
  [fmap slot]
  [:div.tv {:on-click (fn [] (swap! app-state assoc slot fmap)
                        (reagent-modals/close-modal!)
                        (update-used-xp)
                        (update-archetypes))}
   [sticker-view fmap ".mod"]])

(defn reset-features []
  (doseq [slot slot-list]
    (swap! app-state assoc slot nil))
  (update-used-xp)
  (update-archetypes))

(defn reset-feature-btn
  "Renders a button to reset the build"
  []
  [:span [:input {:type "button" :value "Reset Features" :class "btn-primary"
                  :on-click #(reset-features)}]])

(defn save-build []
  (let [build-keys (conj slot-list :name :class :race :xp-earned :archetype)
        build (select-keys @app-state build-keys)
        old-builds (-> (.getItem (.-localStorage js/window) "builds") js/JSON.parse (js->clj :keywordize-keys true))
        name (or (:name @app-state) "Nameless")
        build-map (merge old-builds {name build})]
    (.setItem (.-localStorage js/window) "builds" (-> build-map clj->js js/JSON.stringify))))

(defn save-build-btn
  "Renders a button to save the build keyed from character name"
  []
  [:span.tab [:input {:type "button" :value "Save" :class "btn-primary"
                  :on-click #(save-build)}]])

(defn load-build [build]
  (doseq [item build]
   (swap! app-state assoc (key item) (val item))))

(defn build-select
  "Builds the view of a given build save on modal"
  [build]
  (let [inner (second build)]
    [:div.load
     {:style {:background-color (-> inner :archetype colour-map)}
      :on-click (fn [inner] (load-build (second build))
                  (reagent-modals/close-modal!)
                  (update-used-xp)
                  (update-archetypes))}
     [:div.title {:style {:color "white"}} (-> inner :name)]
     [:div.req {:style {:color "white"}} (-> inner :race) " " (-> inner :class) " - XP:" (-> inner :xp-earned)]]))

(defn get-builds []
  (-> (.getItem (.-localStorage js/window) "builds") js/JSON.parse (js->clj :keywordize-keys true)))

(defn load-build-btn
  "Renders a button to load the build keyed from character name"
  []
  [:span.tab [:input {:type "button" :value "Load" :class "btn-primary"
                      :on-click #(reagent-modals/modal! (into [:div {:style {:background "black"}}]
                                                              (for [b (get-builds)] [build-select b])))}]])

(defn delete-build []
  (let [builds (-> (.getItem (.-localStorage js/window) "builds") js/JSON.parse (js->clj :keywordize-keys true))
        name (keyword (or (:name @app-state) "Nameless"))
        build-map (dissoc builds name)]
    (.setItem (.-localStorage js/window) "builds" (-> build-map clj->js js/JSON.stringify))
    (reset-features)
    (update-used-xp)
    (update-archetypes)
    (swap! app-state assoc :name nil)))

(defn delete-build-btn
  "Renders a button to delete the build keyed from character name"
  []
  [:span.tab [:input {:type "button" :value "Delete" :class "btn-primary"
                      :on-click #(delete-build)}]])

(defn kw->str [kw]
  (s/capitalize (name kw)))

(defn build-text
  "Builds a text version of the features"
  []
  [:div {:id "build"}
   (-> @app-state :class) " : " (-> @app-state :race) " : "
   (-> @app-state :archetype) (when-let [b (-> @app-state :archetype-bonus)] [:span "/" b])
   [:br][:br]

   "Used XP: " (-> @app-state :xp-used) [:br][:br]

   (doall (for [s slot-list]
     (when-not (nil? (@app-state s))
       [:div {:key s}
        (kw->str s) " : "
        (-> @app-state s :name) " : XP - " (-> @app-state s :xp) [:br]
        (-> @app-state s :description) [:br][:br]])))])

(defn clipboard-button [label target]
  (let [clipboard-atom (atom nil)]
    (r/create-class
      {:display-name "clipboard-button"
       :component-did-mount
                     #(let [clipboard (new js/Clipboard (r/dom-node %))]
                        (reset! clipboard-atom clipboard))
       :component-will-unmount
                     #(when-not (nil? @clipboard-atom)
                        (.destroy @clipboard-atom)
                        (reset! clipboard-atom nil))
       :reagent-render (fn [] [:button.clipboard
                               {:on-click #(.removeAllRanges (.getSelection js/window))
                                :data-clipboard-target target} label])})))

(defn export-to-text
  "Renders a popup with the build as text"
  []
  [:span.tab [:input {:type "button" :value "Text" :class "btn-primary"
                      :on-click #(reagent-modals/modal!
                                   [:div {:style {:color "white"
                                                  :background-color (-> @app-state :archetype colour-map)}}
                                    [clipboard-button "Copy to Clipboard" "#build"]
                                    [build-text]])}]])

(defn print-btn
  "Renders a button to bring a print dialog"
  []
  [:span.tab [:input {:type "button" :value "Print" :class "btn-primary"
                      :on-click #(js/window.print())}]])

(defn get-races
  "Gets the valid races for the class choice"
  [class]
  (let [valid-packs (filter #(some #{(:source %)} (-> @app-state :packs)) character-map)]
    (map :race (filter #(= class (:class %)) valid-packs))))

(defn class-select []
  [:select.filter {:value (-> @app-state :class)
                   :name "class" :on-change (fn [e] (swap! app-state assoc :class (-> e .-target .-value))
                                       (swap! app-state assoc :archetype (get-in archetypes [(:class @app-state)]))
                                       (reset-features))}
   (doall (for [c classes]
     [:option {:key c} c]))])

(defn race-select []
  [:select.filter {:value (-> @app-state :race)
                   :name "race" :on-change (fn [e] (swap! app-state assoc :race (-> e .-target .-value))
                                       (reset-features))}
   (doall (for [r (-> @app-state :class get-races)]
     [:option {:key r} r]))])

(defn arch-select []
  [:select {:name "arch" :on-change (fn [e] (swap! app-state assoc :archetype-bonus (-> e .-target .-value)))}
   [:option {:key 0 :selected true :disabled true :hidden true} "Choose One"]
   (for [a (keys colour-map)]
     [:option {:key a} a])])

(defn modal-window-button
  "Renders the feature slots on screen"
  ([slot prompt]
  [:div.col-sm.tv
   {:on-click #(reagent-modals/modal! (into [:div {:style {:background-color (-> @app-state :archetype colour-map)}}]
                                            (for [f (filter-features feature-map slot)]  (sticker-select f slot))))}
    (if-let [chosen (get-in @app-state [slot])]
      [sticker-view chosen]
      prompt)]))

(defn show-feature-slots
  "Build the view of the standard 6 slots on the main screen"
  [start-slot]
  [:div
    (for [row-start [start-slot (+ 3 start-slot)]] ^{:key row-start}
      [:div.row
       (for [slot (range row-start (+ 3 row-start))] ^{:key slot}
         [modal-window-button (keyword (str "slot" slot)) "Select Feature"])])])

(defn show-fighting-styles
  "Build martial class free fighting style selection slot"
  []
  [:div
    (when (= "Martial" (:archetype @app-state))
      [:div.row
       [modal-window-button :fighting1 "Select Fighting Style"]
       ; Show second feature window if Additional Style feature has been taken
       (if (check-feature "Additional Style")
         [modal-window-button :fighting2 "Select Fighting Style"] [:div.col-sm])
       [:div.col-sm]])])

(defn show-invocations
  "Build Warlock invocation free slots if Eldritch Invocation is taken"
  []
  (let [slot #(keyword (str "invocation" %))
        prompt #(str "Select Invocation " %)]
    [:div
   (cond
     (check-feature "Eldritch Invocations")
     [:div.row
      (for [x (range 1 3)] ^{:key x}
        [modal-window-button (slot x) (prompt x)])
      [:div.col-sm]]

     (check-feature "Eldritch Invocations II")
     [:div.row
      (for [x (range 1 4)] ^{:key x}
        [modal-window-button (slot x) (prompt x)])]

     (check-feature "Eldritch Invocations III")
     [:div
      (for [y [1 3]]
        [:div.row
         (for [x (range y (+ 2 y))] ^{:key x}
           [modal-window-button (slot x) (prompt x)])
         [:div.col-sm]])])]))

(defn xp-input [value]
  "Render earned XP entry box"
  [:input {:type "number"
           :style {:line-height "0.8rem"}
           :placeholder "Earned XP"
           :max-length 4
           :id "xp-filter"
           :value (-> @app-state :xp-earned)
           :on-change #(swap! app-state assoc :xp-earned (-> % .-target .-value))}])

(defn get-xp []
  "Render earned XP entry box taking existing value from state"
  [:span.display.tab [xp-input (-> @app-state :xp-earned)]])

(defn filter-on-xp? []
  "Render a checkbox enabling filtering on earned XP"
  [:span.display.tab [:label "Filter on XP?" [:input.box
                                              {:type "checkbox"
                                               :value true
                                               :on-change #(if (:xp-filter @app-state)
                                                             (swap! app-state assoc :xp-filter nil)
                                                             (swap! app-state assoc :xp-filter
                                                                    (-> % .-target .-value)))}]]])

(defn name-input [value]
  "Render a box for character name entry"
  [:input {:type "text"
           :style {:line-height "0.8rem"}
           :placeholder "Name"
           :max-length 20
           :id "char-name"
           :value (-> @app-state :name)
           :on-change #(swap! app-state assoc :name(-> % .-target .-value))}])

; Nav Bar
(defn nav
  []
  [:div [:div.topnav {:class "dfnav"}
         [:a {:href "#/" :id "navitem"} "Build"] [:span.tab]
         [:a {:href "#/options" :id "navitem"} "Options"]] [:br][:br]])

; Main screen
(defn home
  "Builds the main view"
  []
  [:div.container
   [nav]
   [:div-row [name-input] "     " [class-select] "    " [race-select] "      "
    (when (check-feature "Circle of the Land")
      [:span.display "Circle of the Land: " [arch-select]])
    [filter-on-xp?] (when (:xp-filter @app-state) [get-xp])]

   [reagent-modals/modal-window]

   (if (= (:class @app-state) "Warlock") [show-feature-slots 0] [show-feature-slots 1])
   [show-fighting-styles]
   [show-invocations]

   [:div-row
    [:div.display "XP on Features: " (:xp-features @app-state)]
    [:div.display "XP on Slots: " (:xp-slots @app-state)]
    [:div.display "Total XP Used: " (:xp-used @app-state)]
    [:div [reset-feature-btn] [load-build-btn] [save-build-btn] [delete-build-btn] [print-btn] [export-to-text]]]
   [:br] [:div.foot "© Dungeons & Dragons, Dragonfire, Wizards of the Coast, and their respective logos are trademarks of Wizards of the Coast LLC in the U.S.A. and other countries\n
      Catalyst Game Labs and the Catalyst Game Labs logo are trademarks of InMediaRes Productions.\n"]])

; Options Screen
(defn options []
  [:div.container
   [nav]
   [:div.display {:style {:background-color "rgba(39,56,76,0.82)" :width "20rem"
                          :padding "1rem" :opacity "20%"}} "Select Packs"
    (let [save (fn [] (.setItem (.-localStorage js/window) "packs" (-> @app-state :packs clj->js)))]
      (doall (for [pack packs]
               [:div {:key pack} [:label.display pack [:input.box
                                                       {:type "checkbox" :defaultChecked (if (some #{pack} (-> @app-state :packs)) true false)
                                                        :on-change (fn [] (if (some #{pack} (-> @app-state :packs))
                                                                            (do (swap! app-state assoc :packs (remove #(= pack %) (-> @app-state :packs)))
                                                                                (save))
                                                                            (do (swap! app-state assoc :packs (-> @app-state :packs (conj pack)))
                                                                                (save))))}]]])))]])

(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      EventType/NAVIGATE
      (fn [event]
        (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

; Routes
(defn app-routes []
  (secretary/set-config! :prefix "#")
  (defroute "/" []
            (swap! app-state assoc :page :home))
  (defroute "/options" []
            (swap! app-state assoc :page :options))
  (hook-browser-navigation!))

(defmulti current-page #(@app-state :page))
(defmethod current-page :home []
  [home])
(defmethod current-page :options []
  [options])
(defmethod current-page :default []
  [:div ])

(defn mount-root []
  (app-routes)
  (r/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))