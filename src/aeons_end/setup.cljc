(ns aeons-end.setup
  (:require [aeons-end.operations :refer [push-effect-stack check-stack]]
            [aeons-end.cards.common :refer [player-starting-life gravehold-starting-life]]
            [aeons-end.nemesis :as nemesis]
            [aeons-end.cards.gem :as gem]
            [aeons-end.cards.relic :as relic]
            [aeons-end.cards.spell :as spell]
            [aeons-end.cards.attack :as attack]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.power :as power]
            [aeons-end.mages :as mages]
            [aeons-end.turn-order :as turn-order]
            [aeons-end.utils :as ut]))

(def basic-nemesis-cards {1 {1 1
                             2 3
                             3 7}
                          2 {1 3
                             2 5
                             3 7}
                          3 {1 5
                             2 6
                             3 7}
                          4 {1 8
                             2 7
                             3 7}})

(defn create-nemesis [{:keys [life cards] :as nemesis} & {:keys [number-of-players difficulty]}]
  (let [life (case difficulty
               :beginner (- life 10)
               :extinction (+ life 10)
               life)]
    (-> nemesis
        (assoc :life life
               :max-life life
               :deck (->> (range 1 4)
                          (mapcat (fn [tier]
                                    (->> nemesis/basic-cards
                                         (filter (comp #{tier} :tier))
                                         (group-by :type)
                                         (mapcat (comp #(take-nth 2 %) shuffle second))
                                         shuffle
                                         (take (get-in basic-nemesis-cards [number-of-players tier]))
                                         (concat (->> cards
                                                      (filter (comp #{tier} :tier))))
                                         shuffle)))
                          (concat [])
                          (mapv ut/give-id!))
               :phase :out-of-turn)
        (dissoc :cards))))

(defn select-nemesis [{:keys [name min-level max-level]} difficulty]
  (let [fit?             (= :fit difficulty)
        possible-nemeses (cond->> nemesis/nemeses
                                  name (filter (comp #{name} :name))
                                  min-level (filter (comp #(<= (cond-> min-level fit? (- 4)) %) :level))
                                  max-level (filter (comp #(>= (cond-> max-level fit? (+ 2)) %) :level)))
        {:keys [level] :as nemesis} (->> possible-nemeses
                                         shuffle
                                         first)]
    {:nemesis    nemesis
     :difficulty (if fit?
                   (cond
                     (and max-level
                          (< max-level level)) :beginner
                     (or (nil? min-level)
                         (<= min-level level)) :normal
                     (<= (- min-level 2) level) :expert
                     (<= (- min-level 4) level) :extinction)
                   (or difficulty
                       :normal))}))

(def supply-cards (concat gem/cards
                          relic/cards
                          spell/cards))

(defn create-supply [supply]
  (->> supply
       (map (fn [{:keys [type card-name cost min-cost max-cost]}]
              {:possible-cards (cond->> supply-cards
                                        type (filter (comp #{type} :type))
                                        card-name (filter (comp #{card-name} :name))
                                        cost (filter (comp #{cost} :cost))
                                        min-cost (filter (comp #(<= min-cost %) :cost))
                                        max-cost (filter (comp #(<= % max-cost) :cost)))}))
       (sort-by (comp count :possible-cards))
       (reduce (fn [supply {:keys [possible-cards]}]
                 (let [chosen-card-names (->> supply
                                              (map :card)
                                              (map :name)
                                              set)
                       {:keys [type] :as card} (->> possible-cards
                                                    (remove (comp chosen-card-names :name))
                                                    shuffle
                                                    first)]
                   (conj supply {:card      card
                                 :pile-size (if (= :gem type) 7 5)}))) [])
       (sort-by (comp (juxt :type :cost :name) :card))
       vec))

(defn create-player [{:keys [breaches ability] :as mage} & {:keys [difficulty]}]
  (-> mage
      (merge {:breaches       (->> breaches
                                   (mapv (fn [breach1 {:keys [status] :as breach2}]
                                           (if (#{:opened :destroyed} status)
                                             breach2
                                             (merge breach1 breach2)))
                                         [{:status :opened}
                                          {:status     :closed
                                           :focus-cost 2
                                           :open-costs [5 4 3 2]}
                                          {:status       :closed
                                           :focus-cost   3
                                           :open-costs   [9 7 5 3]
                                           :bonus-damage 1}
                                          {:status       :closed
                                           :focus-cost   4
                                           :open-costs   [13 10 7 4]
                                           :bonus-damage 1}]))
              :ability        (merge ability
                                     {:charges 0})
              :life           (player-starting-life difficulty)
              :phase          :out-of-turn
              :revealed-cards 5})
      (update :hand #(map ut/give-id! %))
      (update :deck #(map ut/give-id! %))))

(defn select-mages [players]
  (->> players
       (map (fn [{:keys [name]}]
              {:possible-mages (cond->> mages/mages
                                        name (filter (comp #{name} :name)))}))
       (sort-by (comp count :possible-mages))
       (reduce (fn [mages {:keys [possible-mages]}]
                 (let [chosen-mage-names (->> mages
                                              (map :name)
                                              set)
                       mage              (->> possible-mages
                                              (remove (comp chosen-mage-names :name))
                                              shuffle
                                              first)]
                   (conj mages mage))) [])
       (sort-by :name)
       vec))

(defn setup-turn-order [number-of-players]
  {:deck (->> (concat [turn-order/nemesis
                       turn-order/nemesis
                       turn-order/player-1]
                      (if (>= number-of-players 2)
                        [turn-order/player-2]
                        [turn-order/player-1])
                      (if (>= number-of-players 3)
                        [turn-order/player-3]
                        [turn-order/player-1])
                      (case number-of-players
                        1 nil
                        2 [turn-order/player-2]
                        3 [turn-order/wild]
                        4 [turn-order/player-4]))
              shuffle)})

(defn create-game [{:keys [difficulty nemesis players supply]}]
  (let [{:keys [difficulty nemesis]} (select-nemesis nemesis difficulty)
        {:keys [setup]} nemesis]
    (cond-> {:mode       :slow
             :real-game? true
             :difficulty difficulty
             :nemesis    (create-nemesis nemesis
                                         :number-of-players (count players)
                                         :difficulty difficulty)
             :gravehold  {:life (gravehold-starting-life difficulty)}
             :supply     (create-supply supply)
             :players    (->> (select-mages players)
                              (mapv #(create-player % :difficulty difficulty)))
             :turn-order (setup-turn-order (count players))}
            setup (-> (push-effect-stack {:effects setup})
                      check-stack))))
