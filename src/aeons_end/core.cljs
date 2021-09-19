(ns aeons-end.core
  (:require
    [reagent.core :as r]
    [clojure.string :as string]
    [aeons-end.game-state :as cmd]
    [aeons-end.utils :as ut]
    [aeons-end.nemesis :as nemesis]
    [aeons-end.cards.gem :as gem]
    [aeons-end.cards.relic :as relic]
    [aeons-end.cards.spell :as spell]
    [aeons-end.mages :as mages]))

;; -------------------------
;; Views

(def random-market [{:type :gem} {:type :gem} {:type :gem}
                    {:type :relic} {:type :relic} {:type :spell}
                    {:type :spell} {:type :spell} {:type :spell}])

(def balanced-market [{:type :gem :max-cost 3} {:type :gem :min-cost 4} {:type :gem}
                      {:type :relic :max-cost 4} {:type :relic :min-cost 4} {:type :spell}
                      {:type :spell :max-cost 4} {:type :spell :min-cost 4 :max-cost 5} {:type :spell :min-cost 6}])

(def low-budget-market [{:type :gem :max-cost 3} {:type :gem :min-cost 4 :max-cost 4} {:type :gem :max-cost 5}
                        {:type :relic :max-cost 3} {:type :relic :max-cost 5} {:type :spell :max-cost 6}
                        {:type :spell :max-cost 3} {:type :spell :max-cost 4} {:type :spell :max-cost 5}])

(def prosperous-market [{:type :gem :max-cost 3} {:type :gem :min-cost 4 :max-cost 4} {:type :gem :min-cost 5}
                        {:type :relic :max-cost 4} {:type :relic :min-cost 5} {:type :spell}
                        {:type :spell :min-cost 4 :max-cost 5} {:type :spell :min-cost 5 :max-cost 6} {:type :spell :min-cost 7}])

(defonce state (r/atom {:setup-game? true
                        :game-setup  {:difficulty          :fit
                                      :nemesis             {:min-level 5 :max-level 6}
                                      :players             [{} {}]
                                      :turn-order-variant? false
                                      :supply              balanced-market}
                        :selection   []
                        :expanded?   {:market true}}))

(defn select! [option text]
  (swap! state update :selection conj {:option (or option text)
                                       :text   text}))

(defn remove-idx [coll idx]
  (let [v     (vec coll)
        start (subvec v 0 idx)
        end   (subvec v (inc idx) (count v))]
    (vec (concat start end))))

(defn deselect! [idx]
  (swap! state update :selection remove-idx idx))

(defn button-class [type & [status]]
  (case type
    :gem "gem"
    :relic "relic"
    :spell "spell"
    :attack "attack"
    :minion "minion"
    :acolyte "acolyte"
    :power "power"
    :strike "strike"
    :corruption "corruption"
    :breach (case status
              :closed "closed-breach"
              :focused "closed-breach"
              :opened "opened-breach"
              :destroyed nil)
    :aethereal (case status
                 :closed "closed-aethereal-breach"
                 :focused "closed-aethereal-breach"
                 :opened "opened-aethereal-breach")
    :sigil "sigil-breach"
    {:player-nos #{0 1}} "player-1-2"
    {:player-nos #{2 3}} "player-3-4"
    nil))

(defn button-style [& {:keys [disabled type status number-of-cards width min-width max-width]}]
  (let [inverse? (or (#{:nemesis :sigil :husks :tainted :acolyte} type)
                     (<= 2 (:player-no type))
                     (= #{2 3} (:player-nos type))
                     (#{:closed :focused} status))]
    {:color            (cond
                         inverse? (cond disabled "#ddd"
                                        :else :white)
                         (= :openable status) (case type
                                                :breach "#f8e238"
                                                :aethereal "#bfedee")
                         (= :destroyed status) "#d66"
                         (not disabled) :black
                         (= :strike type) "#444"
                         disabled "#666")
     :font-weight      :bold
     :background-color (cond
                         (= :ability type) (if (= :charged status)
                                             "#f5bb11" #_"#f9e395"
                                             "#f9e395" #_"#b4afa2")
                         (= {:player-no 0} type) "#f2f057"
                         (= {:player-no 1} type) "#ea8f23"
                         (= {:player-no 2} type) "#3aacb4"
                         (= {:player-no 3} type) "#a7326d"
                         (= :nemesis type) "#b22b2e")
     :border-color     (cond
                         (zero? number-of-cards) :red
                         (= :resolving status) :red
                         (#{:gem :attack} type) "#9d77af"
                         (#{:relic} type) "#6bb6dc"
                         (#{:spell :power} type) "#f8c44e"
                         (= :minion type) "#49c4e9"
                         (= :strike type) "#5e3628"
                         (= :husks type) "#232122"
                         (= :tainted type) "#40512c"
                         (= :corruption type) "#34971c"
                         (= :acolyte type) "#0b2013"
                         (= :destroyed status) "#ccc"
                         (= :focused status) "#f9cf23"
                         (#{:breach :sigil} type) (cond
                                                    (#{:closed :openable} status) "#434f64"
                                                    :else "#f9cf23")
                         (= :aethereal type) "#67c19c"
                         (= :ability type) "#c0895e"
                         (= {:player-no 0} type) "#9d8c42"
                         (= {:player-no 1} type) "#a1642b"
                         (= {:player-no 2} type) "#36696e"
                         (= {:player-no 3} type) "#73304f"
                         (= :nemesis type) "#782d2a"
                         :else :grey)
     :border-width     2
     :vertical-align   :top
     :width            width
     :min-width        min-width
     :max-width        max-width}))

(defn mapk [f coll]
  (->> coll
       (map (fn [e]
              (-> e f (with-meta {:key (random-uuid)}))))
       doall))

(defn keepk [f coll]
  (->> coll
       (keep (fn [e]
               (when e
                 (-> e f (with-meta {:key (random-uuid)})))))
       doall))

(defn mapk-indexed [f coll]
  (->> coll
       (map-indexed (fn [i e]
                      (with-meta (f i e) {:key (random-uuid)})))
       doall))

(defn map-tag [tag coll & [style]]
  (keepk (fn [x] [tag style x]) coll))

(defn format-title [{:keys [text cast-text]}]
  (str (if (coll? text)
         (string/join "\n" text)
         text)
       (when (and text cast-text)
         "\n")
       (when cast-text
         (str "Cast: "
              (if (coll? cast-text)
                (string/join "\n" cast-text)
                cast-text)))))

(defn view-card
  ([card]
   (view-card nil card))
  ([max {:keys [name name-ui choice-value choice-opts type cost number-of-cards breach-no interaction prepable-breaches] :as card}]
   (if (map? card)
     (let [selection       (:selection @state)
           num-selected    (->> selection (filter (comp #{name choice-value} :option)) count)
           number-of-cards (if (and (= :choosable interaction)
                                    (not (:leave-in-play-area choice-opts)))
                             (let [num (- (or number-of-cards 1) num-selected)]
                               (if (= 1 num) nil num))
                             number-of-cards)
           disabled        (or (nil? interaction)
                               (and (= :choosable interaction)
                                    (= (count selection) max))
                               (and (:unique choice-opts)
                                    (->> selection (map :option) (some #{name choice-value})))
                               (and (:similar choice-opts)
                                    (->> selection (map :option) (some (comp not #{name choice-value})))))
           prepable        (= :prepable interaction)]
       (when-not (and (= :choosable interaction)
                      (= 0 number-of-cards))
         [:div
          [:button {:style         (button-style :disabled disabled
                                                 :type type
                                                 :number-of-cards number-of-cards)
                    :class         (button-class type)
                    :title         (format-title card)
                    :disabled      disabled
                    :on-click      (when interaction
                                     (fn [] (case interaction
                                              :playable (swap! state assoc :game (cmd/play name))
                                              :choosable (select! choice-value name)
                                              :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name)))
                                              :buyable (swap! state assoc :game (cmd/buy name))
                                              :discardable (swap! state assoc :game (cmd/discard name))
                                              :prepable (swap! state assoc :game (cmd/prep-spell (first prepable-breaches) name))
                                              :castable (swap! state assoc :game (cmd/cast-spell breach-no name))
                                              :while-preppedable (swap! state assoc :game (cmd/use-while-prepped breach-no name)))))
                    :draggable     prepable
                    :on-drag-start (when prepable
                                     #(swap! state assoc :dragging {:card-name         name
                                                                    :prepable-breaches (set prepable-breaches)}))
                    :on-drag-end   (when prepable
                                     #(swap! state dissoc :dragging))}
           (str name-ui
                (when cost (str " (" (ut/format-cost cost) ")"))
                (when number-of-cards (str " x" number-of-cards)))]]))
     card)))

(defn format-text [text & [title]]
  (cond
    (coll? text) (->> text
                      (mapk-indexed (fn [idx paragraph]
                                      (format-text paragraph (when (zero? idx) title)))))
    (and (string? text)
         (re-find #"\n" text)) (format-text (string/split text #"\n") title)
    :else [:div {:style {:font-size   "0.9em"
                         :font-weight (if (= "OR" text)
                                        :bold
                                        :normal)
                         :padding-top "3px"}}
           (when title
             [:strong (str title (when text ": "))])
           text]))

(defn view-nemesis-card [{:keys [name name-ui text quote choice-value type status cost number-of-cards
                                 immediately-text to-discard-text power power-text life persistent-text blood-magic-text
                                 cast-text interaction] :as card}]
  (when card
    (let [selected? (->> (:selection @state) (some (comp #{name choice-value} :option)))
          disabled  (or (nil? interaction)
                        selected?)]
      [:button {:style    (merge (button-style :disabled disabled
                                               :type type
                                               :status status
                                               :number-of-cards number-of-cards)
                                 {:width      "150px"
                                  :min-height "170px"})
                :class    (button-class type)
                :title    quote
                :disabled disabled
                :on-click (when interaction
                            (fn [] (case interaction
                                     :discardable (swap! state assoc :game (cmd/discard-power-card name))
                                     :choosable (select! choice-value name)
                                     :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name)))
                                     :buyable (swap! state assoc :game (cmd/buy name)))))}
       [:div
        [:div {:style {:font-size "1.4em"}} (when cost (str (ut/format-cost cost) " ")) name-ui]
        [:div
         (when text
           (format-text text))
         (when immediately-text
           (format-text immediately-text "IMMEDIATELY"))
         (when to-discard-text
           (format-text to-discard-text "TO DISCARD"))
         (when power-text
           (format-text power-text (str "POWER" (when power (str " " power)))))
         (when life
           (format-text nil (str "Life: " life)))
         (when persistent-text
           (format-text persistent-text "PERSISTENT"))
         (when blood-magic-text
           (format-text blood-magic-text "BLOOD MAGIC"))
         (when cast-text
           (format-text cast-text "Cast"))]
        (when number-of-cards
          [:div {:padding-top "3px"}
           (str " x" number-of-cards)])]])))

(defn view-breach [max {:keys [breach-no name-ui status type focus-cost open-cost prepped-spells bonus-damage choice-value interaction interactions]}]
  (let [interactions (if interaction
                       #{interaction}
                       interactions)
        disabled     (empty? interactions)
        prepable     (contains? (get-in @state [:dragging :prepable-breaches]) breach-no)]
    [:tr {:style {:border :none}}
     [:td {:style {:border :none}}
      (let [focusing? (atom false)]
        [:button {:style           (button-style :disabled disabled
                                                 :type (or type :breach)
                                                 :status (or (get interactions :openable)
                                                             status)
                                                 :min-width 88)
                  :class           (button-class (or type :breach) status)
                  :disabled        disabled
                  :on-click        (when (not-empty interactions)
                                     (fn [] (cond
                                              (:openable interactions) (when (compare-and-set! focusing? false true)
                                                                         (js/setTimeout (fn [] (when @focusing?
                                                                                                 (swap! state assoc :game (cmd/focus-breach breach-no))
                                                                                                 (reset! focusing? false)))
                                                                                        250))
                                              (:focusable interactions) (swap! state assoc :game (cmd/focus-breach breach-no))
                                              (:quick-choosable interactions) (swap! state assoc :game (cmd/choose (or choice-value breach-no))))))
                  :on-double-click (when (:openable interactions)
                                     #(do (reset! focusing? false)
                                          (swap! state assoc :game (cmd/open-breach breach-no))))
                  :on-drag-over    (when prepable
                                     (fn [e] (.preventDefault e)))
                  :on-drop         (when prepable
                                     (fn []
                                       (let [{:keys [card-name]} (get @state :dragging)]
                                         (swap! state assoc :game (cmd/prep-spell breach-no card-name)))))}
         (str name-ui
              (when (= :opened status)
                (cond
                  bonus-damage (str " (+" bonus-damage " dmg)")
                  (= :aethereal type) " (+$1)"
                  :else " (open)"))
              (when (or focus-cost open-cost) (str " (" (ut/format-cost focus-cost) "/" (ut/format-cost open-cost) ")")))])]
     [:td {:style {:border         :none
                   :vertical-align :top}}
      (->> prepped-spells
           (map #(assoc % :breach-no breach-no))
           (mapk (partial view-card max)))]]))

(defn view-nemesis-breach [{:keys [breach-no name-ui text status stage cost choice-value interaction]}]
  (let [disabled (nil? interaction)]
    [:tr {:style {:border :none}}
     [:td {:style {:border :none}}
      [:button {:style    (button-style :disabled disabled
                                        :type :breach
                                        :status status
                                        :width "88px")
                :class    (button-class :breach status)
                :disabled disabled
                :on-click (when interaction
                            (fn [] (case interaction
                                     :unfocusable (swap! state assoc :game (cmd/unfocus-nemesis-breach breach-no))
                                     :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name))))))
                :title    text}
       (case status
         :closed (str name-ui " (" (ut/format-cost cost) ") " stage "/4")
         :opened (str name-ui " (open)"))]]]))

(defn view-ability [{:keys [name-ui text activation-text charges charge-cost interaction choice-value]} player-no]
  (let [disabled (nil? interaction)]
    [:div {:style {:padding-top "3px"}}
     [:button {:style    (button-style :disabled disabled
                                       :type :ability
                                       :status (when (>= charges charge-cost) :charged)
                                       :width "150px")
               :disabled disabled
               :on-click (when interaction
                           (fn [] (case interaction
                                    :quick-choosable (swap! state assoc :game (cmd/choose choice-value))
                                    :chargeable (swap! state assoc :game (cmd/charge-ability))
                                    :activatable (swap! state assoc :game (cmd/activate-ability player-no)))))}
      [:div
       [:div {:style {:font-size "1.2em"}}
        name-ui]
       [:div {:style {:font-size "1.1em"}}
        (str "Charges: " charges "/" charge-cost)]
       [:hr {:style {:margin "1px"}}]
       [:div {:style {:padding-top "3px"
                      :font-size   "0.9em"}}
        activation-text]
       (format-text text)]]]))

(defn view-player-pile [pile max]
  [:div
   (mapk (partial view-card max) (:visible-cards pile))
   (when (:number-of-cards pile)
     (str (:number-of-cards pile) " Cards"))])

(defn view-expandable-pile [key {:keys [card cards number-of-cards]} & [{:keys [deck split-after nemesis? max]}]]
  (let [expanded?    (get-in @state [:expanded? key])
        view-card-fn (if nemesis?
                       view-nemesis-card
                       (partial view-card max))]
    [:table
     [:tbody {:style {:border :none}}
      (when deck
        [:tr [:td {:col-span 2
                   :style    {:border :none}}
              deck]])
      [:tr
       [:td {:style {:border :none}}
        (if expanded?
          (if split-after
            (let [split-cards (concat (when (pos? split-after)
                                        [(take split-after cards)])
                                      (->> cards
                                           (drop split-after)
                                           (partition 5 5 [])))]
              (->> split-cards
                   (mapk-indexed (fn [idx cards]
                                   [:div
                                    (when (pos? idx)
                                      [:hr])
                                    (->> cards
                                         (mapk view-card-fn))]))))
            (->> cards
                 (mapk (partial view-card max))))
          (view-card-fn card))]
       [:td {:style {:border         :none
                     :vertical-align :top}}
        (when (> number-of-cards 1)
          [:button {:on-click (fn [] (swap! state update-in [:expanded? key] not))}
           (if expanded? "-" (str "+ (" number-of-cards ")"))])]]]]))

(defn view-kingdom-card [expanded? {:keys [card]}]
  (if expanded?
    (view-nemesis-card card)
    (view-card card)))

(defn view-row [expanded? row]
  [:tr (->> row
            (map (partial view-kingdom-card expanded?))
            (mapk (fn [card] [:td card])))])

(defn view-choice [{:keys [choice-title
                           text
                           or-text
                           options
                           interval
                           min
                           max
                           quick-choice?
                           optional?] :as choice}]
  (let [selection (:selection @state)]
    [:td {:style {:font-weight :bold
                  :padding     "20px"}}
     (when choice-title
       [:div {:style {:font-size   "2em"
                      :font-weight :bold}}
        choice-title])
     [:div (format-text text)]
     [:div (->> options
                (mapk-indexed (fn [idx {:keys [option text]}]
                                (let [disabled (and (not quick-choice?)
                                                    (or (= max (count selection))
                                                        (->> selection (map :option) set option)))]
                                  [:<>
                                   (when (pos? idx)
                                     [:div {:style {:font-weight :bold}}
                                      "OR"])
                                   [:button {:style    (button-style :disabled disabled)
                                             :disabled disabled
                                             :on-click (fn [] (if quick-choice?
                                                                (swap! state assoc :game (cmd/choose option))
                                                                (select! option text)))}
                                    text]]))))]
     (when interval
       [:div [:button {:style    (button-style)
                       :on-click (fn [] (swap! state assoc
                                               :game (cmd/choose 0)
                                               :selection []))}
              "Top"]
        (when (pos? (:to interval))
          [:span [:input {:type      :number
                          :min       1
                          :max       (dec (:to interval))
                          :on-change (fn [e] (swap! state assoc :selection [(js/parseInt (-> e .-target .-value))]))
                          :value     (or (-> @state :selection first) 0)}]
           [:button {:style    (button-style)
                     :on-click (fn [] (swap! state assoc
                                             :game (cmd/choose (:to interval))
                                             :selection []))}
            "Bottom"]])])
     (when (or (not quick-choice?) interval)
       [:div
        (when (and (not-empty selection)
                   (< 1 max))
          [:div "Selected: " (->> selection
                                  (mapk-indexed (fn [idx {:keys [text]}]
                                                  [:button {:style    (button-style)
                                                            :on-click (fn [] (deselect! idx))}
                                                   (ut/format-name text)])))])
        (when (and (empty? selection)
                   or-text)
          [:div {:style {:font-weight :bold}}
           "OR"])
        (let [disabled (and min (< (count selection) min)
                            (not (and optional? (empty? selection))))]
          [:button {:style    (button-style :disabled disabled)
                    :disabled disabled
                    :on-click (fn [] (swap! state assoc
                                            :game (cmd/choose (map :option selection))
                                            :selection []))}
           (or (and (empty? selection)
                    or-text)
               (if (and (zero? min)
                        (empty? selection))
                 "Decline"
                 "Done"))])])]))

(defn set-selector []
  (fn [sets set-name]
    [:div
     (ut/format-name set-name)
     [:input {:type      :checkbox
              :checked   (contains? sets set-name)
              :on-change #(if (contains? sets set-name)
                            (swap! state update :sets disj set-name)
                            (swap! state update :sets conj set-name))}]]))

(defn option [_ & _]
  (fn [value & [diff-mod]]
    [:option {:value value} (str (ut/format-name value)
                                 (when diff-mod
                                   (str " ("
                                        (when (pos? diff-mod)
                                          "+")
                                        diff-mod
                                        ")")))]))

(defn mage->string [{:keys [name set]}]
  (when name
    (cond-> (clojure.core/name name)
            set (str "/" (clojure.core/name set)))))

(defn string->mage [mage-name]
  (let [[name set] (string/split mage-name #"/")]
    (when (not-empty name)
      (medley.core/assoc-some {:name (keyword name)}
                              :set (keyword set)))))

(defn setup-game []
  (fn []
    (let [{:keys [nemesis difficulty
                  players turn-order-variant?
                  supply] :as game-setup} (:game-setup @state)]
      [:div [:h2 "Aeon's End"]
       [:div
        [:button {:style    (button-style)
                  :on-click #(swap! state assoc
                                    :game (cmd/start-game game-setup)
                                    :setup-game? false)}
         "Start game"]]

       [:table
        [:tbody
         [:tr
          [:td {:col-span 2}
           (let [{:keys [name min-level]} nemesis]
             [:div
              "Nemesis: "
              [:select {:value     (or name
                                       min-level
                                       :random)
                        :on-change (fn [event]
                                     (let [value          (.. event -target -value)
                                           nemesis-setup  (cond
                                                            (re-matches #"\d" value) (let [min-level (js/parseInt value)]
                                                                                       {:min-level min-level
                                                                                        :max-level (inc min-level)})
                                                            (= "random" value) {}
                                                            :else {:name (keyword value)})
                                           new-difficulty (cond
                                                            (and (nil? (:min-level nemesis-setup))
                                                                 (= :fit difficulty)) :normal
                                                            (and (:min-level nemesis-setup)
                                                                 (nil? min-level)) :fit
                                                            :else difficulty)]
                                       (swap! state update :game-setup merge {:nemesis    nemesis-setup
                                                                              :difficulty new-difficulty})))}
               [:<>
                [:option {:value :random} "Any nemesis"]
                [:hr]
                (->> (range 1 8 2)
                     (mapk (fn [n]
                             [:option {:value n} (str "Level " n "-" (inc n))])))
                [:hr]
                (->> nemesis/nemeses
                     (sort-by (juxt :level :name))
                     (mapk (fn [{:keys [name level]}]
                             [:option {:value name} (str (ut/format-name name) " (" level ")")])))]]
              [:div
               "Difficulty: "
               [:select {:value     difficulty
                         :on-change #(swap! state assoc-in [:game-setup :difficulty] (keyword (.. % -target -value)))}
                [:<>
                 (when (:min-level nemesis)
                   [:<>
                    [option :fit]
                    [:hr]])
                 [option :beginner -2]
                 [option :normal]
                 [option :expert +2]
                 [option :extinction +4]]]]])]]
         [:tr
          [:td
           (let [selected-players (->> players
                                       (keep :name)
                                       set)]
             [:div
              [:div "Players: " [:select {:style     {:width              "20px"
                                                      :padding            "4px"
                                                      :-webkit-appearance :none
                                                      :-moz-appearance    :none
                                                      :text-indent        "1px"
                                                      :text-overflow      ""}
                                          :value     (count players)
                                          :on-change (fn [event]
                                                       (let [value (js/parseInt (.. event -target -value))
                                                             diff  (- value (count players))]
                                                         (swap! state assoc-in [:game-setup :players] (vec (if (pos? diff)
                                                                                                             (concat players (repeat diff {}))
                                                                                                             (take value players))))))}
                                 [:<> (->> (range 1 5)
                                           (mapk (fn [n]
                                                   [:option {:value n} n])))]]]
              (when (= 4 (count players))
                [:div "4p variant "
                 [:input {:type     :checkbox
                          :checked  turn-order-variant?
                          :on-click #(swap! state update-in [:game-setup :turn-order-variant?] not)}]])
              (->> players
                   (mapk-indexed (fn [player-no {:keys [name] :as mage}]
                                   [:div [:select {:value     (or (mage->string mage) "random")
                                                   :on-change (fn [event]
                                                                (let [value (.. event -target -value)]
                                                                  (swap! state assoc-in [:game-setup :players player-no] (if (= "random" value)
                                                                                                                           {}
                                                                                                                           (string->mage value)))))}
                                          [:<>
                                           [:option {:value "random"} "Any mage"]
                                           [:hr]
                                           (->> mages/mages
                                                (remove (comp (clojure.set/difference selected-players #{name}) :name))
                                                (sort-by :name)
                                                (mapk (fn [mage]
                                                        (let [{:keys [name set]} mage]
                                                          [:option {:value (mage->string mage)}
                                                           (cond-> (ut/format-name name)
                                                                   set (str " (" (ut/format-name-short set) ")"))]))))]]])))])]
          [:td
           (let [value          (cond
                                  (= supply random-market) :random
                                  (= supply balanced-market) :balanced
                                  (= supply low-budget-market) :low-budget
                                  (= supply prosperous-market) :prosperous
                                  :else :custom)
                 selected-cards (->> supply
                                     (keep :card-name)
                                     set)]
             [:div
              [:strong "Market: "] [:select {:value     value
                                             :on-change (fn [event]
                                                          (let [value (keyword (.. event -target -value))]
                                                            (case value
                                                              :random (swap! state assoc-in [:game-setup :supply] random-market)
                                                              :balanced (swap! state assoc-in [:game-setup :supply] balanced-market)
                                                              :low-budget (swap! state assoc-in [:game-setup :supply] low-budget-market)
                                                              :prosperous (swap! state assoc-in [:game-setup :supply] prosperous-market)
                                                              :custom nil)))}
                                    [:<>
                                     [:option {:value :random} "All random"]
                                     [:option {:value :balanced} "Balanced"]
                                     [:option {:value :low-budget} "Low budget"]
                                     [:option {:value :prosperous} "Prosperous"]
                                     (when (= :custom value)
                                       [:option {:value :custom} "Custom"])]]
              [:table
               [:tbody
                (for [row (range 3)]
                  [:tr {:key (str "row" row)}
                   (for [col (range 3)]
                     (let [idx   (+ col (* 3 row))
                           {:keys [type min-cost max-cost card-name]} (get supply idx)
                           cards (case type
                                   :gem gem/cards
                                   :relic relic/cards
                                   :spell spell/cards)]
                       [:td {:key (str "cell" idx)}
                        [:select {:style     (merge (button-style :type type
                                                                  :width 40)
                                                    {:-webkit-appearance :none
                                                     :-moz-appearance    :none
                                                     :text-indent        "1px"
                                                     :text-overflow      ""})
                                  :value     (or min-cost "-")
                                  :disabled  card-name
                                  :on-change (fn [event]
                                               (let [value (.. event -target -value)]
                                                 (if (= "-" value)
                                                   (swap! state update-in [:game-setup :supply idx] dissoc :min-cost)
                                                   (swap! state assoc-in [:game-setup :supply idx :min-cost] (js/parseInt value)))))}
                         [:<>
                          [:option {:value "-"}]
                          (->> (range 3 (or (when max-cost (inc max-cost)) 9))
                               (mapk (fn [n]
                                       [:option {:value n} (str "$" n (if (= n max-cost) " =" " <="))])))]]
                        [:select {:style     (button-style :type type
                                                           :width 160)
                                  :class     (button-class type)
                                  :value     (or card-name
                                                 :random)
                                  :on-change (fn [event]
                                               (let [value (keyword (.. event -target -value))]
                                                 (swap! state update-in [:game-setup :supply idx] (fn [setup]
                                                                                                    (if (= :random value)
                                                                                                      (dissoc setup :card-name)
                                                                                                      (-> setup
                                                                                                          (assoc :card-name value)
                                                                                                          (dissoc :min-cost
                                                                                                                  :max-cost)))))))}
                         [:<>
                          [:option {:value :random} (str "Any " (ut/format-name type))]
                          [:hr]
                          (->> cards
                               (remove (comp (clojure.set/difference selected-cards #{card-name}) :name))
                               (sort-by (juxt :cost :name))
                               (mapk (fn [{:keys [cost name]}]
                                       [:option {:value name} (str "$" cost " - " (ut/format-name name))])))]]
                        [:select {:style     (merge (button-style :type type
                                                                  :width 40)
                                                    {:-webkit-appearance :none
                                                     :-moz-appearance    :none
                                                     :text-indent        "1px"
                                                     :text-overflow      ""})
                                  :value     (or max-cost "-")
                                  :disabled  card-name
                                  :on-change (fn [event]
                                               (let [value (.. event -target -value)]
                                                 (if (= "-" value)
                                                   (swap! state update-in [:game-setup :supply idx] dissoc :max-cost)
                                                   (swap! state assoc-in [:game-setup :supply idx :max-cost] (js/parseInt value)))))}
                         [:<>
                          [:option {:value "-"}]
                          (->> (range (or min-cost 3) 9)
                               (mapk (fn [n]
                                       [:option {:value n} (str (if (= n min-cost) "= " "<= ") "$" n)])))]]]))])]]])]]]]])))

(defn home-page []
  (fn []
    (let [{:keys [setup-game? game selection expanded?]} @state
          started? (-> @cmd/game-state :game count (> 1))]
      (if setup-game?
        [setup-game]
        [:div [:h2 "Aeon's End"]
         [:div
          (if (or (:game-over game)
                  (not started?))
            [:button {:style    (button-style)
                      :on-click #(swap! state assoc
                                        :game nil
                                        :setup-game? true)}
             "New game"]
            [:button {:style    (button-style)
                      :on-click (fn [] (if (js/confirm "Are you sure you want to give up the current game? All progress will be lost.")
                                         (swap! state assoc
                                                :game nil
                                                :setup-game? true)))}
             "Resign"])
          (let [disabled (not started?)]
            [:button {:style    (button-style :disabled disabled)
                      :disabled disabled
                      :on-click (fn [] (if (js/confirm "Are you sure you want to restart the current game? All progress will be lost.")
                                         (swap! state assoc :game (cmd/restart) :selection [])))}
             "Retry"])]

         [:table
          [:tbody
           [:tr
            [:td {:col-span 2}
             (let [{:keys [name name-ui tier unleash-text additional-rules life deck play-area discard active?
                           cloaks fury husks tainted-jades tainted-track breaches corruptions devoured
                           acolytes acolytes-in-play
                           interaction choice-value]} (:nemesis game)]
               [:div [:table
                      [:tbody
                       [:tr (map-tag :th [(str "Nemesis - Tier " tier)
                                          (when breaches "Breaches")
                                          (when acolytes-in-play "Acolytes")
                                          "Play area"
                                          (str "Deck" (when deck (str " (" (:number-of-cards deck) ")")))
                                          "Discard"
                                          (when devoured "Devoured")])]
                       [:tr {:class (when active?
                                      "active-nemesis")}
                        [:td
                         [:div
                          (let [disabled (nil? interaction)]
                            [:button {:style    (merge (button-style :type :nemesis
                                                                     :disabled disabled)
                                                       {:width      "150px"
                                                        :min-height "170px"})
                                      :title    additional-rules
                                      :disabled disabled
                                      :on-click (when interaction
                                                  (fn [] (case interaction
                                                           :choosable (select! choice-value name)
                                                           :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name))))))}
                             [:div
                              [:div {:style {:font-size "1.4em"}} name-ui]
                              [:div {:style {:font-size   "0.8em"
                                             :font-weight :normal}}
                               "Difficulty: " (ut/format-name (:difficulty game))]
                              [:div {:style {:font-size   "1.1em"
                                             :padding-top "3px"}}
                               "Life: " life]
                              (when cloaks
                                [:div {:style {:font-size   "1.1em"
                                               :padding-top "3px"}}
                                 "Cloaks: " cloaks])
                              (when fury
                                [:div {:style {:font-size   "1.1em"
                                               :padding-top "3px"}}
                                 "Fury: " fury])
                              (when tainted-jades
                                [:div {:style {:font-size   "1.1em"
                                               :padding-top "3px"}}
                                 "Tainted Jades: " tainted-jades])
                              (when corruptions
                                [:div {:style {:font-size   "1.1em"
                                               :padding-top "3px"}}
                                 "Corruptions: " corruptions])
                              (when acolytes
                                [:div {:style {:font-size   "1.1em"
                                               :padding-top "3px"}}
                                 "Acolytes: " acolytes])
                              [:hr]
                              (format-text unleash-text "UNLEASH")]])
                          (when tainted-track
                            (let [{:keys [tainted-level title next-tainted-level next-tainted-text interaction choice-value status]} tainted-track
                                  disabled (nil? interaction)]
                              [:button {:style    (merge (button-style :type :tainted
                                                                       :status status
                                                                       :disabled disabled)
                                                         {:width      "150px"
                                                          :min-height "170px"})
                                        :class    "tainted"
                                        :title    (format-title {:text title})
                                        :disabled disabled
                                        :on-click (when interaction
                                                    (fn [] (case interaction
                                                             :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value :tainted-track))))))}
                               [:div
                                [:div {:style {:font-size "1.4em"}}
                                 "Tainted Track"]
                                [:div {:style {:font-size "1.1em"}}
                                 (format-text tainted-level "Current space")]
                                [:div {:style {:font-size "1.1em"}}
                                 (format-text next-tainted-text (str "Space " next-tainted-level))]]]))]]
                        (when breaches
                          [:td [:table
                                [:tbody {:style {:border :none}}
                                 (mapk view-nemesis-breach breaches)]]])
                        (when acolytes-in-play
                          [:td (mapk view-nemesis-card acolytes-in-play)])
                        [:td [:div
                              (when husks
                                (let [{:keys [number-of-husks title swarm-text swarm-interval interaction choice-value]} husks
                                      disabled (nil? interaction)]
                                  [:button {:style    (merge (button-style :type :husks
                                                                           :disabled disabled)
                                                             {:width      "150px"
                                                              :min-height "170px"})
                                            :class    "husks"
                                            :title    (format-title {:text title})
                                            :disabled disabled
                                            :on-click (when interaction
                                                        (fn [] (case interaction
                                                                 :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value :husks))))))}
                                   [:div
                                    [:div {:style {:font-size "1.4em"}} (str number-of-husks " husk" (when (not= 1 number-of-husks) "s"))]
                                    [:div (format-text swarm-text (str "SWARM " swarm-interval))]]]))
                              (mapk view-nemesis-card play-area)]]
                        [:td [:div
                              (when (:visible-cards deck)
                                (->> (:visible-cards deck)
                                     (mapk view-nemesis-card)))]]
                        [:td [view-expandable-pile :discard/nemesis discard
                              {:nemesis? true}]]
                        (when devoured
                          [:td [view-expandable-pile :devoured/nemesis devoured
                                {:nemesis? true}]])]]]])]]
           [:tr
            [:td
             (when-let [{:keys [deck discard]} (:turn-order game)]
               [:div
                [:table {:style {:width "100%"}}
                 [:tbody
                  [:tr (map-tag :th ["Gravehold" "Turn order deck"])]
                  [:tr
                   [:td
                    (let [{:keys [life pillars]} (:gravehold game)]
                      [:div
                       [:div "Life: " life]
                       (when pillars
                         [:div "Pillars: " pillars])])]
                   [:td
                    (let [number-of-cards (:number-of-cards deck)]
                      [:div
                       [view-expandable-pile :turn-order-discard discard
                        {:deck [:button {:style    (button-style :disabled true)
                                         :disabled true}
                                (if (pos? number-of-cards)
                                  (str "Turn order x" number-of-cards)
                                  "(empty)")]}]
                       (when (:visible-cards deck)
                         (->> (reduce (fn [cards {:keys [option]}]
                                        (let [pre  (take-while (comp not #{option} :name) cards)
                                              post (drop (inc (count pre)) cards)]
                                          (vec (concat pre post))))
                                      (:visible-cards deck)
                                      selection)
                              (mapk (fn [{:keys [name name-ui type interaction choice-value]}]
                                      (let [disabled (nil? interaction)]
                                        [:div
                                         "[ "
                                         [:button {:style    (button-style :disabled disabled
                                                                           :type type)
                                                   :class    (button-class type)
                                                   :disabled disabled
                                                   :on-click (when interaction
                                                               (fn [] (case interaction
                                                                        :choosable (select! choice-value name)
                                                                        :quick-choosable (swap! state assoc :game (cmd/choose name)))))}
                                          name-ui]
                                         " ]"])))))])]
                   (when-let [{:keys [conclusion text]} (:game-over game)]
                     [:td
                      [:div {:style {:text-align :center}}
                       [:div {:style {:font-size   "3em"
                                      :font-weight :bold
                                      :padding     "20px"
                                      :color       (case conclusion
                                                     :defeat :red
                                                     :victory :green)}}
                        (ut/format-name conclusion)]
                       [:div
                        (format-text text)]]])
                   (when (:choice game)
                     (view-choice (:choice game)))
                   [:td {:style {:max-width "100%"
                                 :float     :right}}
                    [:div
                     (let [disabled (not (get-in game [:commands :can-undo?]))]
                       [:button {:style    (button-style :disabled disabled)
                                 :disabled disabled
                                 :on-click (fn [] (swap! state assoc
                                                         :game (cmd/undo)
                                                         :selection []))}
                        [:div {:style {:font-size "2em"
                                       :padding   "10px"}}
                         "Undo"]])]]]]]])

             (let [show-purchased? (expanded? :purchased)
                   {:keys [can-play-all-gems? can-discard-all? can-end-turn? confirm-end-turn]} (:commands game)]
               [:div
                [:table
                 [:tbody
                  [:tr (map-tag :th ["Breach Mage" "Breaches" "Hand" "Play area" "Deck" "Discard" (when show-purchased? "Purchased")])]
                  (->> (:players game)
                       (mapk-indexed (fn [player-no {:keys [name name-ui turn-order-token title type life ability aether breaches
                                                            hand play-area deck discard purchased trophies active? choice-value interaction]}]
                                       (let [{:keys [max repeatable?]} (:choice game)]
                                         [:tr {:class (when active?
                                                        (str "active-player-" (inc player-no)))}
                                          [:td
                                           [:div
                                            (let [disabled (or (nil? interaction)
                                                               (and (= :choosable interaction)
                                                                    (= (count selection) max))
                                                               (and (not repeatable?)
                                                                    (->> selection (map :option) (some #{name choice-value}))))]
                                              [:button {:style    (button-style :type type
                                                                                :disabled disabled
                                                                                :width "150px")
                                                        :disabled disabled
                                                        :on-click (when interaction
                                                                    (fn [] (case interaction
                                                                             :choosable (select! choice-value name)
                                                                             :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name))))))}
                                               [:div
                                                [:div {:style {:font-size "1.4em"}}
                                                 (when turn-order-token
                                                   [:div {:style {:float        :left
                                                                  :border-width "1px"
                                                                  :width        "16px"
                                                                  :height       "16px"}}])
                                                 name-ui
                                                 (when turn-order-token
                                                   [:div {:style {:float        :right
                                                                  :border-style :groove
                                                                  :border-color "#666"
                                                                  :border-width "1px"
                                                                  :width        "16px"
                                                                  :height       "16px"}
                                                          :class turn-order-token}])]
                                                [:div {:style {:font-size   "0.9em"
                                                               :font-weight :normal
                                                               :padding-top "1px"}}
                                                 title]
                                                [:div {:style {:font-size   "1.1em"
                                                               :padding-top "3px"}}
                                                 "Life: " life]
                                                (when trophies
                                                  [:div {:style {:font-size   "1.1em"
                                                                 :padding-top "3px"}}
                                                   "Trophies: " trophies])]])]
                                           (view-ability ability player-no)
                                           (when active?
                                             [:div {:style {:padding-top "3px"
                                                            :font-size   "1.2em"
                                                            :font-weight :bold
                                                            :text-align  :center}}
                                              "Aether: " aether])]
                                          [:td [:table
                                                [:tbody {:style {:border :none}}
                                                 (mapk (partial view-breach max) breaches)]]]
                                          [:td [:div
                                                (when (and active?
                                                           can-play-all-gems?)
                                                  [:div [:button {:style    (button-style)
                                                                  :on-click (fn [] (swap! state assoc :game (cmd/play-all-gems)))}
                                                         "Play all Gems"]
                                                   [:hr]])
                                                (mapk (partial view-card max) hand)]]
                                          [:td [:div
                                                (when (and active?
                                                           can-discard-all?)
                                                  [:div
                                                   [:button {:style    (button-style)
                                                             :on-click (fn [] (swap! state assoc :game (cmd/discard-all)))}
                                                    "Discard all"]
                                                   [:hr]])
                                                (mapk (partial view-card max) play-area)]]
                                          [:td [:div
                                                (when (and active?
                                                           can-end-turn?)
                                                  [:div
                                                   (let [confirm-text confirm-end-turn]
                                                     [:button {:style    (button-style)
                                                               :on-click (fn [] (if (or (not confirm-text)
                                                                                        (js/confirm (str confirm-text
                                                                                                         "\nAre you sure you want to end your turn?")))
                                                                                  (swap! state assoc :game (cmd/end-turn))))}
                                                      "End Turn"])
                                                   [:hr]])
                                                (view-player-pile deck max)]]
                                          [:td [view-expandable-pile (keyword "discard" (cljs.core/name name)) discard
                                                {:split-after (-> (- 5 (:number-of-cards deck)) (mod 5))
                                                 :max         max}]]
                                          (when show-purchased?
                                            [:td {:style {:background-color "#888"}}
                                             (->> purchased
                                                  (mapk (partial view-card)))])]))))]]])]
            [:td
             (let [supply           (:supply game)
                   market-expanded? (expanded? :market)]
               [:div {:style {:font-weight :bold}}
                "Market " [:button {:on-click (fn [] (swap! state update-in [:expanded? :market] not))}
                           (if market-expanded? "-" "+")]
                (let [[row1 supply] (split-at 3 supply)
                      [row2 row3] (split-at 3 supply)]
                  [:table
                   [:tbody
                    (view-row market-expanded? row1)
                    (view-row market-expanded? row2)
                    (view-row market-expanded? row3)]])])]]]]

         (let [{:keys [number-of-cards] :as trash} (:trash game)]
           (when (pos? number-of-cards)
             [:div (str "Destroyed")
              [view-expandable-pile :trash trash]]))

         (let [show-purchased? (expanded? :purchased)]
           [:div
            "Mode: "
            [:button {:style    (button-style)
                      :on-click (fn [] (swap! state assoc :game (cmd/switch-mode)))}
             (ut/format-name (:mode game))]
            " Purchased: "
            [:button {:on-click (fn [] (swap! state update-in [:expanded? :purchased] not))}
             (if show-purchased? "Shown" "Hidden")]])]))))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
