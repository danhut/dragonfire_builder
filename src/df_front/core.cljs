(ns df-front.core
    (:require
      [reagent.core :as r]
      [clojure.string :as s]
      [reagent-modals.modals :as reagent-modals]
      [df-front.features :refer [features titles feature-map slot-list archetypes classes races paths]]))

;; Initialise App Data
(defonce app-state (r/atom {:xp-earned 0 :xp-used 0 :xp-slots 0 :xp-features 0
                            :class "Fighter"
                            :archetype "Martial"
                            :race "Forest Gnome"
                            :path "Non-specialised"
                            :slot-cost {:slot0 0 :slot1 0 :slot2 5 :slot3 10 :slot4 15 :slot5 25 :slot6 40}
                            :slot0 nil :slot1 nil :slot2 nil :slot3 nil :slot4 nil :slot5 nil :slot6 nil :weapon1 nil :weapon2 nil
                            :invocation1 nil :invocation2 nil :invocation3 nil :invocation4 nil}))

;; Builder functions
(defn filter-features
  "Main filtering function when a feature slot button is pushed"
  [features slot]
  (let [existing (keep #((keyword %) @app-state) slot-list)
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
        archetype (filter #(s/includes? (:requires %) (str (:archetype @app-state) " Class")) extra-slots-only)
        path (filter #(s/includes? (:requires %) (:path @app-state)) features)
        no-path (remove #(s/includes? (:requires %) "&") extra-slots-only)
        class (filter #(s/includes? (:requires %) (str (:class @app-state) " Class")) no-path)
        ; hack to have Sun Elf recognised in feature requirements. Also data hacked have preceeding space
        myrace (if (s/includes? (:race @app-state) " Elf") " Elf" (:race @app-state))
        race (seq (filter #(s/includes? (:requires %) myrace) features))]

    (cond
      (or (= slot :slot0))
      patron

      (or (= slot :weapon1) (= slot :weapon2))
      fighting

      (or (= slot :invocation1) (= slot :invocation2) (= slot :invocation3) (= slot :invocation4))
      invocations

      (= "Warlock" (:class @app-state))
      (remove #(nil? %) (flatten [race no-req archetype class path]))

      :else
      (remove #(nil? %) (flatten [race background no-req archetype class path])))))

(defn update-used-xp
  "Updates amount of XP used by the build based on selections"
  []
  (let [slot-costs (reduce + (map #(if (% @app-state) (get-in @app-state [:slot-cost %]) 0) slot-list))
        feature-costs (reduce + (map #(let [fcost (js/parseInt (get-in @app-state [% :xp]))]
                                        (if (int? fcost) fcost 0)) slot-list))]
    (swap! app-state assoc :xp-slots slot-costs)
    (swap! app-state assoc :xp-features feature-costs)
    (swap! app-state assoc :xp-used (+ slot-costs feature-costs))))

(defn sticker-view
  "Shows the data for a given feature sticker"
  [fmap]
  [:div
   [:div.title (:name fmap)]
   [:div.req (:requires fmap)]
   [:div.description (:description fmap)]
   [:div.xp (:xp fmap) (when-not (= "Background" (:xp fmap)) " XP")]])

(defn sticker-select
  "Builds the view of a given feature sticker on main screen and modal"
  [fmap slot]
  [:div.tv {:key      (:name fmap)
            :on-click (fn [] (swap! app-state assoc slot fmap)
                        (reagent-modals/close-modal!)
                        (update-used-xp))}
   (sticker-view fmap)])

; Class, race, and specialisation selection buttons
(defn class-select []
  [:select {:name "class" :on-change (fn [e] (swap! app-state assoc :class (-> e .-target .-value))
                                       (swap! app-state assoc :archetype (get-in archetypes [(:class @app-state)])))}
   (for [c classes]
     [:option {:key c} c])])

(defn race-select []
  [:select {:name "class" :on-change (fn [e] (swap! app-state assoc :race (-> e .-target .-value)))}
   (for [r races]
     [:option {:key r} r])])

(defn path-select []
  [:select {:name "class" :on-change (fn [e] (swap! app-state assoc :path (-> e .-target .-value)))}
   (for [p (-> @app-state :class paths)]
     [:option {:key p} p])])

(defn reset-features
  "Renders a button to reset the build"
  []
  [:div
   [:input {:type "button" :value "Reset Features" :class "btn-primary"
            :on-click #(do (doseq [slot slot-list] (swap! app-state assoc slot nil))
                           (update-used-xp))}]])

(defn modal-window-button
  "Renders the feature slots on screen"
  ([slot prompt]
  [:div.col-sm.tv
   {:on-click #(reagent-modals/modal! [:div (for [f (filter-features feature-map slot)] (sticker-select f slot))])}
    (if-let [chosen (get-in @app-state [slot])]
      (sticker-view chosen)
      prompt)]))

(defn check-feature
  "Checks if a feature has been selected in the builder (by name)"
  [feature-name]
  (seq (filter #(= feature-name (get-in @app-state [% :name])) slot-list)))

(defn show-feature-slots
  "Build the view of the standard 6 slots on the main screen"
  [start-slot]
  (for [row-start [start-slot (+ 3 start-slot)]]
    [:div.row
     (for [slot (range row-start (+ 3 row-start))]
       [modal-window-button (keyword (str "slot" slot)) "Select Feature"])]))

(defn show-fighting-styles
  "Build martial class free fighting style selection slot"
  []
  (when (= "Martial" (:archetype @app-state))
    [:div.row
     [modal-window-button :weapon1 "Select Fighting Style"]
     ; Show second feature window if Additional Style feature has been taken
     (if (check-feature "Additional Style")
       [modal-window-button :weapon2 "Select Fighting Style"] [:div.col-sm])
     [:div.col-sm]]))

(defn show-invocations
  "Build Warlock invocation free slots if Eldritch Invocation is taken"
  []
  (let [slot #(keyword (str "invocation" %))
        prompt #(str "Select Invocation " %)]
   (cond
     (check-feature "Eldritch Invocations")
     [:div.row
      (for [x (range 1 3)]
        [modal-window-button (slot x) (prompt x)])
      [:div.col-sm]]

     (check-feature "Eldritch Invocations II")
     [:div.row
      (for [x (range 1 4)]
        [modal-window-button (slot x) (prompt x)])]

     (check-feature "Eldritch Invocations III")
     [:div
      (for [y [1 3]]
        [:div.row
         (for [x (range y (+ 2 y))]
           [modal-window-button (slot x) (prompt x)])
         [:div.col-sm]])])))

(defn home
  "Builds the main view"
  []
  [:div.container
   [:div-row
    (class-select) "    " (race-select) "    " (when (-> @app-state :class paths) (path-select))]

   [reagent-modals/modal-window]

   (if (= (:class @app-state) "Warlock") (show-feature-slots 0) (show-feature-slots 1))
   (show-fighting-styles)
   (show-invocations)

   [:div-row
    ;[:div.display "XP Available: " (:xp-earned @app-state)]
    [:div.display "XP on Features: " (:xp-features @app-state)]
    [:div.display "XP on Slots: " (:xp-slots @app-state)]
    [:div.display "Total XP Used: " (:xp-used @app-state)]
    [:div (reset-features)]]
   [:br] [:br] [:div.foot "© Dungeons & Dragons, Dragonfire, Wizards of the Coast, and their respective logos are trademarks of Wizards of the Coast LLC in the U.S.A. and other countries\n
      Catalyst Game Labs and the Catalyst Game Labs logo are trademarks of InMediaRes Productions.\n"]])

(defn mount-root []
  (r/render [home] (.getElementById js/document "app")))

(defn init! []
  (mount-root))