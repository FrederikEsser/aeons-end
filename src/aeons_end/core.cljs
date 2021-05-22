(ns aeons-end.core
  (:require
    [reagent.core :as r]
    [clojure.string :as string]
    [aeons-end.game-state :as cmd]
    [aeons-end.utils :as ut]))

;; -------------------------
;; Views

(defonce state (r/atom {:setup-game? true
                        :selection   []
                        :num-players 2}))

(defn select! [option]
  (swap! state update :selection conj option))

(defn remove-idx [coll idx]
  (let [v     (vec coll)
        start (subvec v 0 idx)
        end   (subvec v (inc idx) (count v))]
    (vec (concat start end))))

(defn deselect! [idx]
  (swap! state update :selection remove-idx idx))

(defn button-style [& {:keys [disabled type status number-of-cards]}]
  (let [inverse? (#{:closed :focused :openable} status)]
    {:color            (if inverse?
                         (cond disabled "#cccccc"
                               (= :openable status) "#f8e238"
                               :else :white)
                         (cond disabled :grey
                               :else :black))
     :font-weight      :bold
     :background-color (cond
                         (#{:gem :attack} type) "#cfbede"
                         (#{:relic} type) "#c7dff5"
                         (#{:spell :power} type) "#f7e2b5"
                         (= :minion type) "#aadfef"
                         (= :breach type) (if (= :opened status)
                                            "#f8e238"
                                            "#506f9a")
                         (= :ability type) (if (= :charged status)
                                             "#f5bb11" #_"#f9e395"
                                             "#f9e395" #_"#b4afa2"))
     :border-color     (cond
                         (zero? number-of-cards) :red
                         (#{:gem :attack} type) "#9d77af"
                         (#{:relic} type) "#6bb6dc"
                         (#{:spell :power} type) "#f8c44e"
                         (= :minion type) "#49c4e9"
                         (= :breach type) (if (#{:closed :openable} status)
                                            "#434f64"
                                            "#f9cf23")
                         (= :ability type) "#c0895e"
                         :else :grey)
     :border-width     2}))

(defn mapk [f coll]
  (->> coll
       (map (fn [e]
              (-> e f (with-meta {:key (random-uuid)}))))
       doall))

(defn mapk-indexed [f coll]
  (->> coll
       (map-indexed (fn [i e]
                      (with-meta (f i e) {:key (random-uuid)})))
       doall))

(defn map-tag [tag coll & [style]]
  (mapk (fn [x] [tag style x]) coll))

(defn view-card
  ([card]
   (view-card nil card))
  ([max {:keys [name name-ui text choice-value choice-opts type cost number-of-cards breach-no interaction] :as card}]
   (if (map? card)
     (let [selection       (:selection @state)
           num-selected    (->> selection (filter #{name choice-value}) count)
           number-of-cards (if (and (= :choosable interaction)
                                    (not (:leave-in-play-area choice-opts)))
                             (let [num (- (or number-of-cards 1) num-selected)]
                               (if (= 1 num) nil num))
                             number-of-cards)
           disabled        (or (nil? interaction)
                               (and (= :choosable interaction)
                                    (= (count selection) max))
                               (and (:unique choice-opts)
                                    (some #{name choice-value} selection))
                               (and (:similar choice-opts)
                                    (some (comp not #{name choice-value}) selection)))]
       (when-not (and (= :choosable interaction)
                      (= 0 number-of-cards))
         [:div
          [:button {:style    (button-style :disabled disabled
                                            :type type
                                            :number-of-cards number-of-cards)
                    :title    text
                    :disabled disabled
                    :on-click (when interaction
                                (fn [] (case interaction
                                         :playable (swap! state assoc :game (cmd/play name))
                                         :choosable (select! (or choice-value name))
                                         :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name)))
                                         :buyable (swap! state assoc :game (cmd/buy name))
                                         :discardable (swap! state assoc :game (cmd/discard name))
                                         :prepable (swap! state assoc :game (cmd/prep-spell name breach-no))
                                         :castable (swap! state assoc :game (cmd/cast-spell name breach-no)))))}
           (str name-ui
                (when cost (str " (" (ut/format-cost cost) ")"))
                (when number-of-cards (str " x" number-of-cards)))]]))
     card)))

(defn view-breach [{:keys [breach-no name-ui status focus-cost open-cost prepped-spells bonus-damage choice-value choice-opts interactions]}]
  (let [disabled (empty? interactions)]
    [:tr {:style {:border :none}}
     [:td {:style {:border :none}}
      [:button {:style    (button-style :disabled disabled
                                        :type :breach
                                        :status (or (get interactions :openable)
                                                    status))
                :disabled disabled
                :on-click (when (not-empty interactions)
                            (fn [] (cond
                                     (:openable interactions) (swap! state assoc :game (cmd/open-breach breach-no))
                                     (:focusable interactions) (swap! state assoc :game (cmd/focus-breach breach-no)))))}
       (str name-ui
            (when (= :opened status)
              (if bonus-damage
                (str " (+" bonus-damage " dmg)")
                " (open)"))
            (when (or focus-cost open-cost) (str " (" (ut/format-cost focus-cost) "/" (ut/format-cost open-cost) ")")))]]
     [:td {:style {:border         :none
                   :vertical-align :top}}
      (->> prepped-spells
           (map #(assoc % :breach-no breach-no))
           (mapk view-card))]]))

(defn view-ability [{:keys [name-ui text type charges charge-cost interaction]}]
  (let [disabled (nil? interaction)]
    [:div
     [:button {:style    (button-style :disabled disabled
                                       :type :ability
                                       :status (when (>= charges charge-cost) :charged))
               :title    text
               :disabled disabled
               :on-click (when interaction
                           (fn [] (case interaction
                                    :chargeable (swap! state assoc :game (cmd/charge-ability))
                                    :activatable (swap! state assoc :game (cmd/activate-ability)))))}
      (str name-ui
           " (" charges "/" charge-cost ")")]]))

(defn view-player-pile [pile max]
  [:div
   (mapk (partial view-card max) (:visible-cards pile))
   (when (:number-of-cards pile)
     (str (:number-of-cards pile) " Cards"))])

(defn view-expandable-pile [key {:keys [card cards]}]
  (let [expanded? (get-in @state [:expanded? key])]
    [:table
     [:tbody {:style {:border :none}}
      [:tr {:style {:border :none}}
       [:td {:style {:border :none}}
        (if expanded?
          (mapk view-card cards)
          (view-card card))]
       [:td {:style {:border         :none
                     :vertical-align :top}}
        [:button {:on-click (fn [] (swap! state update-in [:expanded? key] not))}
         (if expanded? "-" "+")]]]]]))

(defn view-kingdom-card [{:keys [card]}]
  (view-card card))

(defn view-row [row]
  [:tr (->> row
            (map view-kingdom-card)
            (mapk (fn [card] [:td card])))])

(defn view-nemesis-card
  [{:keys [name name-ui text choice-value type interaction] :as card}]
  (let [disabled (nil? interaction)]
    [:button {:style    (button-style :disabled disabled
                                      :type type)
              :title    text
              :disabled disabled
              :on-click (when interaction
                          (fn [] (case interaction
                                   :discardable (swap! state assoc :game (cmd/discard name)))))}
     [:div
      [:div name-ui]
      [:div text]]]))

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
    [:td
     (when choice-title
       [:div {:style {:font-weight :bold}}
        choice-title])
     [:div text]
     [:div (mapk-indexed (fn [idx {:keys [option text]}]
                           (let [disabled (and (not quick-choice?)
                                               (or (= max (count selection))
                                                   (-> selection set option)))]
                             [:<>
                              (when (pos? idx)
                                [:div {:style {:font-weight :bold}}
                                 "OR"])
                              [:button {:style    (button-style :disabled disabled)
                                        :disabled disabled
                                        :on-click (fn [] (if quick-choice?
                                                           (swap! state assoc :game (cmd/choose option))
                                                           (select! option)))}
                               text]])) options)]
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
        (when (< 1 max)
          [:div "Selected: " (mapk-indexed (fn [idx selected]
                                             [:button {:style    (button-style)
                                                       :on-click (fn [] (deselect! idx))}
                                              (ut/format-name selected)]) selection)])
        (when or-text
          [:div {:style {:font-weight :bold}}
           "OR"])
        (let [disabled (and min (< (count selection) min)
                            (not (and optional? (empty? selection))))]
          [:button {:style    (button-style :disabled disabled)
                    :disabled disabled
                    :on-click (fn [] (swap! state assoc
                                            :game (cmd/choose selection)
                                            :selection []))}
           (or or-text "Done")])])]))

(defn set-selector []
  (fn [sets set-name]
    [:div
     (ut/format-name set-name)
     [:input {:type      :checkbox
              :checked   (contains? sets set-name)
              :on-change #(if (contains? sets set-name)
                            (swap! state update :sets disj set-name)
                            (swap! state update :sets conj set-name))}]]))

(defn create-game []
  (fn []
    [:div
     [:button {:style    (button-style)
               :on-click (fn [] (swap! state assoc
                                       :game (cmd/start-game)
                                       :setup-game? false))}
      "Create game"]]))

(defn home-page []
  (fn []
    (let [{:keys [setup-game?]} @state]
      [:div [:h2 "Aeon's End"]

       (when setup-game?
         [create-game])

       [:div [:button {:style    (button-style)
                       :on-click (fn [] (swap! state update :setup-game? not))}
              "Game setup"]
        [:button {:style    (button-style)
                  :on-click (fn [] (if (js/confirm "Are you sure you want to restart the current game? All progress will be lost.")
                                     (swap! state assoc :game (cmd/restart) :selection [])))}
         "Restart"]
        (let [disabled (-> @state :game :commands :can-undo? not)]
          [:button {:style    (button-style :disabled disabled)
                    :disabled disabled
                    :on-click (fn [] (swap! state assoc :game (cmd/undo) :selection []))}
           "Undo"])]

       (when-let [{:keys [name name-ui life tokens deck play-area discard interaction choice-value choice]} (-> @state :game :nemesis)]
         [:div [:table
                [:tbody
                 [:tr (map-tag :th ["Nemesis" "Play area" "Deck" "Discard"])]
                 [:tr
                  [:td
                   [:div
                    (let [disabled (nil? interaction)]
                      [:button {:title    "Nemesis"
                                :style    (button-style :disabled disabled)
                                :disabled disabled
                                :on-click (when interaction
                                            (fn [] (case interaction
                                                     :choosable (select! (or choice-value name))
                                                     :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name))))))}
                       name-ui])]
                   [:div "Life: " life]
                   [:div "Tokens: " tokens]]
                  [:td [:div
                        (mapk view-nemesis-card play-area)]]
                  [:td [:div
                        (str (:number-of-cards deck) " Cards")]]
                  [:td (when discard
                         (view-nemesis-card discard))]
                  (when choice
                    (view-choice choice))]]]])
       [:div "Gravehold Life: " (-> @state :game :gravehold :life)]
       [:div
        [:table
         [:tbody
          [:tr (map-tag :th ["Breach Mage" "Breaches" "Hand" "Play area" "Deck" "Discard"])]
          (->> (get-in @state [:game :players])
               (mapk (fn [{:keys                    [name-ui title life ability aether breaches hand play-area deck discard active? choice-value interaction]
                           {:keys [max] :as choice} :choice}]
                       (let [breach-no     (->> breaches
                                                (remove (comp #{:closed} :status))
                                                (filter (comp empty? :prepped-spells))
                                                (sort-by (juxt :bonus-damage :status))
                                                last
                                                :breach-no)
                             add-breach-no (fn [{:keys [type] :as card}]
                                             (cond-> card
                                                     (= :spell type) (assoc :breach-no breach-no)))]
                         [:tr
                          [:td
                           [:div
                            (let [disabled (nil? interaction)]
                              [:button {:title    title
                                        :style    (button-style :disabled disabled)
                                        :disabled disabled
                                        :on-click (when interaction
                                                    (fn [] (case interaction
                                                             :choosable (select! (or choice-value name))
                                                             :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name))))))}
                               [:div
                                [:div name-ui]
                                [:div title]]])]
                           (view-ability ability)
                           [:div "Life: " life]
                           [:div "Aether: " aether]]
                          [:td [:table
                                [:tbody {:style {:border :none}}
                                 (mapk view-breach breaches)]]]
                          [:td [:div
                                (when (and active? (-> @state :game :commands :can-play-all-gems?))
                                  [:div [:button {:style    (button-style)
                                                  :on-click (fn [] (swap! state assoc :game (cmd/play-all-gems)))}
                                         "Play all Gems"]
                                   [:hr]])
                                (mapk (partial view-card max) (map add-breach-no hand))]]
                          [:td [:div
                                (when (and active? (get-in @state [:game :commands :can-discard-all?]))
                                  [:div
                                   [:button {:style    (button-style)
                                             :on-click (fn [] (swap! state assoc :game (cmd/discard-all)))}
                                    "Discard all"]
                                   [:hr]])
                                (mapk (partial view-card max) play-area)]]
                          [:td [:div
                                (when (and active? (get-in @state [:game :commands :can-end-turn?]))
                                  [:div
                                   (let [confirm-text (-> @state :game :commands :confirm-end-turn)]
                                     [:button {:style    (button-style)
                                               :on-click (fn [] (if (or (not confirm-text)
                                                                        (js/confirm (str confirm-text
                                                                                         "\nAre you sure you want to end your turn?")))
                                                                  (swap! state assoc :game (cmd/end-turn))))}
                                      "End Turn"])
                                   [:hr]])
                                (view-player-pile deck max)]]
                          [:td (view-player-pile discard max)]
                          (when choice
                            (view-choice choice))]))))]]]

       [:div "Supply"
        (let [supply (-> (:game @state) :supply)
              [row1 supply] (split-at 3 supply)
              [row2 row3] (split-at 3 supply)]
          [:table
           [:tbody
            (view-row row1)
            (view-row row2)
            (view-row row3)]])]
       [:div (str "Destroyed (" (get-in @state [:game :trash :number-of-cards]) " cards)")
        [view-expandable-pile :trash (get-in @state [:game :trash])]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
