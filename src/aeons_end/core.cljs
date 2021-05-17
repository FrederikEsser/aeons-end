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

(defn button-style [& [disabled types number-of-cards]]
  {:color            (cond disabled :grey
                           :else :black)
   :font-weight      :bold
   :background-color (cond
                       (:gem types) "#cfbede"
                       (:relic types) "#c7dff5"
                       (:spell types) "#f7e2b5"
                       (:minion types) "#aadfef"
                       (:closed types) "#506f9a"
                       (:focused types) "#506f9a"
                       (:opened types) "#f8e238")
   :border-color     (cond
                       (zero? number-of-cards) :red
                       (:gem types) "#9d77af"
                       (:relic types) "#6bb6dc"
                       (:spell types) "#f8c44e"
                       (:minion types) "#49c4e9"
                       (:closed types) "#434f64"
                       (:focused types) "#f9cf23"
                       (:opened types) "#f9cf23"
                       :else :grey)
   :border-width     2})

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
  ([max {:keys [name name-ui text choice-value choice-opts type cost number-of-cards interaction] :as card}]
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
          [:button {:style    (button-style disabled #{type} number-of-cards)
                    :title    text
                    :disabled disabled
                    :on-click (when interaction
                                (fn [] (case interaction
                                         :playable (swap! state assoc :game (cmd/play name))
                                         :choosable (select! (or choice-value name))
                                         :quick-choosable (swap! state assoc :game (cmd/choose (or choice-value name)))
                                         :buyable (swap! state assoc :game (cmd/buy name)))))}
           (str name-ui
                (when cost (str " (" (ut/format-cost cost) ")"))
                (when number-of-cards (str " x" number-of-cards)))]]))
     card)))

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
    (let [{:keys [setup-game? selection]} @state]
      [:div [:h2 "Aeon's End"]

       (when setup-game?
         [create-game])

       [:div [:button {:style    (button-style)
                       :on-click (fn [] (swap! state update :setup-game? not))}
              "Game setup"]
        [:button {:style    (button-style false)
                  :on-click (fn [] (if (js/confirm "Are you sure you want to restart the current game? All progress will be lost.")
                                     (swap! state assoc :game (cmd/restart) :selection [])))}
         "Restart"]
        (let [disabled (-> @state :game :commands :can-undo? not)]
          [:button {:style    (button-style disabled)
                    :disabled disabled
                    :on-click (fn [] (swap! state assoc :game (cmd/undo) :selection []))}
           "Undo"])]

       [:div "Nemesis" " Life: " (-> @state :game :nemesis :life)]
       [:div "Players"
        [:table
         [:tbody
          [:tr (map-tag :th ["Name" "Hand" "Play area" "Deck" "Discard"])]
          (->> (get-in @state [:game :players])
               (mapk (fn [{:keys               [name-ui title hand play-area deck discard aether active?]
                           {:keys [text
                                   options
                                   interval
                                   min
                                   max
                                   quick-choice?
                                   optional?]} :choice}]
                       [:tr
                        [:td
                         [:div name-ui]
                         (when title
                           [:div title])]
                        [:td (mapk (partial view-card max) hand)]
                        [:td (mapk (partial view-card max) play-area)]
                        [:td (view-player-pile deck max)]
                        [:td (view-player-pile discard max)]
                        [:td [:div
                              [:div "Aether: " aether]]]
                        (if text
                          [:td text
                           [:div (mapk (fn [{:keys [option text]}]
                                         (let [disabled (and (not quick-choice?)
                                                             (or (= max (count selection))
                                                                 (-> selection set option)))]
                                           [:button {:style    (button-style disabled)
                                                     :disabled disabled
                                                     :on-click (fn [] (if quick-choice?
                                                                        (swap! state assoc :game (cmd/choose option))
                                                                        (select! option)))}
                                            text])) options)]
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
                              (let [disabled (and min (< (count selection) min)
                                                  (not (and optional? (empty? selection))))]
                                [:button {:style    (button-style disabled)
                                          :disabled disabled
                                          :on-click (fn [] (swap! state assoc
                                                                  :game (cmd/choose selection)
                                                                  :selection []))}
                                 "Done"])])]
                          [:td
                           (when active?
                             [:div
                              [:div (let [disabled (-> @state :game :commands :can-play-all-gems? not)]
                                      [:button {:style    (button-style disabled)
                                                :disabled disabled
                                                :on-click (fn [] (swap! state assoc :game (cmd/play-all-gems)))}
                                       "Play all Gems"])]
                              (when (get-in @state [:game :commands :can-discard-all?])
                                [:div
                                 [:button {:style    (button-style false)
                                           :on-click (fn [] (swap! state assoc :game (cmd/discard-all)))}
                                  "Discard all"]])
                              [:div (let [disabled     (-> @state :game :commands :can-end-turn? not)
                                          confirm-text (-> @state :game :commands :confirm-end-turn)]
                                      [:button {:style    (button-style disabled)
                                                :disabled disabled
                                                :on-click (fn [] (if (or (not confirm-text)
                                                                         (js/confirm (str confirm-text
                                                                                          "\nAre you sure you want to end your turn?")))
                                                                   (swap! state assoc :game (cmd/end-turn))))}
                                       "End Turn"])]])])])))]]]

       [:div "Supply"
        (let [supply (-> (:game @state) :supply)
              [row1 supply] (split-at 3 supply)
              [row2 row3] (split-at 3 supply)]
          [:table
           [:tbody
            (view-row row1)
            (view-row row2)
            (view-row row3)]])]
       #_[:div (str "Trash (" (get-in @state [:game :trash :number-of-cards]) " cards)")
          [view-expandable-pile :trash (get-in @state [:game :trash])]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
