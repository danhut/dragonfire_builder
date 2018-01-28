(ns df-front.core
    (:require
      [reagent.core :as r]
      [reagent-modals.modals :as reagent-modals]
      [df-front.features :refer [features titles]]))

;; TODO
; full class banner import
; input of available xp
; handle weaponstyles - dont use feature slot
; secondary archtype for 2p/3p - is this aplpicable to skills?
; add a filter option if you spend too much xp
; fix css for model xp
; auto discover paths

;; Data
(def slot-list [:slot0 :slot1 :slot2 :slot3 :slot4 :slot5 :slot6])

(def feature-map (distinct (map #(zipmap titles %) features)))

(def archetypes {"Fighter" "Martial"
                 "Paladin" "Martial"
                 "Cleric" "Devotion"
                 "Druid" "Devotion"
                 "Wizard" "Arcane"
                 "Warlock" "Arcane"
                 "Bard" "Deception"
                 "Rogue" "Deception"
                 "Ranger" "Martial"})

(def classes (keys archetypes))

(def races
  (sort ["Shield Dwarf" "Lightfoot Halfling" "Tiefling" "Moon Elf" "Wood Elf" "Rock Gnome"
         "Gold Dwarf" "Tiefling" "Sun Elf" "Human" "Forest Gnome" "Wood Elf" "Half-Elf"]))

(def paths {"Fighter" ["Non-specialised" "Champion" "Eldritch Knight"]
            "Rogue" ["Non-specialised" "Arcane Trickster" "Thief"]
            "Wizard" ["Non-specialised" "Conjuration (Savant)" "Evocation (Savant)"]
            "Cleric" ["Non-specialised" "Life Domain" "War Domain"]
            "Druid" ["Non-specialised" "Circle of the Land" "Circle of the Moon"]
            "Ranger" ["Non-specialised" "Hunter" "Beastmaster"]
            "Bard" ["Non-specialised" "College of Lore" "College of Valor"]
            "Warlock" ["Non-specialised" "Patron (Fey)" "Patron (Fiend)"]})

;; Initialise App Data
(defonce app-state (r/atom {
                        ; handle warlock slot0 - check its name and rules
                        :xp-earned 0 :xp-used 0 :xp-slots 0 :xp-features 0
                        :class "Fighter"
                        :archetype "Martial"
                        :race "Forest Gnome"
                        :path "Non-specialised"
                        :slot-cost {:slot0 0 :slot1 0 :slot2 5 :slot3 10 :slot4 15 :slot5 25 :slot6 40}
                        :slot0 nil :slot1 nil :slot2 nil :slot3 nil :slot4 nil :slot5 nil :slot6 nil :weapon1 nil :weapon2 nil}))

; Utilities
(defn kw->str [kw]
  (clojure.string/capitalize (name kw)))

(defn str->kw [str]
  (keyword (clojure.string/lower-case str)))

(defn filter-features [features slot]
  (let [background (when (= slot :slot1) (filter #(= "Background" (:xp %)) features))
        no-background (remove #(= "Background" (:xp %)) features)
        no-req (filter #(empty? (:requires %)) no-background)
        archetype (filter #(clojure.string/includes? (:requires %) (:archetype @app-state)) features)
        path (filter #(clojure.string/includes? (:requires %) (:path @app-state)) features)
        no-path (remove #(clojure.string/includes? (:requires %) "&") features)
        class (filter #(clojure.string/includes? (:requires %) (:class @app-state)) no-path)
        ; hack to have Sun Elf recognised in feature requirements. Also data hacked have preceeding space
        myrace (if (clojure.string/includes? (:race @app-state) " Elf") " Elf" (:race @app-state))
        race (seq (filter #(clojure.string/includes? (:requires %) myrace) features))]
    (remove #(nil? %) (flatten [race background no-req archetype class path]))))

(defn update-used-xp []
  (let [slot-costs (reduce + (map #(if (% @app-state) (get-in @app-state [:slot-cost %]) 0) slot-list))
        feature-costs (reduce + (map #(let [fcost (js/parseInt (get-in @app-state [% :xp]))]
                                        (if (int? fcost) fcost 0)) slot-list))]
    (swap! app-state assoc :xp-slots slot-costs)
    (swap! app-state assoc :xp-features feature-costs)
    (swap! app-state assoc :xp-used (+ slot-costs feature-costs))))

(defn sticker-view [fmap]
  [:div
   [:div.title (:name fmap)]
   [:div.req (:requires fmap)]
   [:div.description (:description fmap)]
   [:div.xp (:xp fmap) (when-not (= "Background" (:xp fmap)) " XP")]])

(defn sticker-select [fmap slot]
  [:div.tv {:key      (:name fmap)
            :on-click (fn [] (swap! app-state assoc slot fmap)
                        (reagent-modals/close-modal!)
                        (update-used-xp))}
   (sticker-view fmap)])

(defn update-archetype []
  (swap! app-state assoc :archetype (-> @app-state :class archetypes)))

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

(defn modal-window-button
  ;([slot] (modal-window-button [slot "Select Feature"]))
  ([slot prompt]
  [:div.col-sm.tv
   {:on-click #(reagent-modals/modal! [:div (for [f (filter-features feature-map slot)] (sticker-select f slot))])}
    (if-let [chosen (get-in @app-state [slot])]
      (sticker-view chosen)
      prompt)]))

(defn home []
    [:div.container
     [:div-row
      (class-select) "    " (race-select) "    " (path-select)]

     [reagent-modals/modal-window]
     ; handle warlock slot0?
     [:div.row
      [modal-window-button :slot1 "Select Feature"]
      [modal-window-button :slot2 "Select Feature"]
      [modal-window-button :slot3 "Select Feature"]]
     [:div.row
      [modal-window-button :slot4 "Select Feature"]
      [modal-window-button :slot5 "Select Feature"]
      [modal-window-button :slot6 "Select Feature"]]


     ;need to make these in centre
     ;(when (= "Martial" (:archetype @app-state))
     ;  [:div.row
     ;   [modal-window-button :weapon1 "Select Weapon Style"]
     ;   [modal-window-button :weapon2 "Select Secondary Weapon"]])

     [:div-row
      ;[:div.display "XP Available: " (:xp-earned @app-state)]
      [:div.display "XP on Features: " (:xp-features @app-state)]
      [:div.display "XP on Slots: " (:xp-slots @app-state)]
      [:div.display "Total XP Used: " (:xp-used @app-state)]  ;if xp used bigger than avail style to red]])
      ]])

(defn mount-root []
  (r/render [home] (.getElementById js/document "app")))

(defn init! []
  (mount-root))