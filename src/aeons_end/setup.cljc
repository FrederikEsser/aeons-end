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
            [aeons-end.nemeses.umbra-titan :refer [umbra-titan]]
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
  (-> nemesis
      (assoc :life (case difficulty
                     :beginner (- life 10)
                     :extinction (+ life 10)
                     life))
      (dissoc :cards)
      (merge
        {:deck (->> (range 1 4)
                    (mapcat (fn [tier]
                              (->> nemesis/basic-cards
                                   (filter (comp #{tier} :tier))
                                   shuffle
                                   (take (get-in basic-nemesis-cards [number-of-players tier]))
                                   (concat (->> cards
                                                (filter (comp #{tier} :tier))))
                                   shuffle)))
                    (concat [])
                    vec)})))

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
      (merge {:breaches (->> breaches
                             (mapv (fn [breach1 {:keys [status] :as breach2}]
                                     (if (= :opened status)
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
              :ability  (merge ability
                               {:charges 0})
              :life     (player-starting-life difficulty)
              :phase    :out-of-turn})
      (update :hand #(map ut/give-id! %))
      (update :deck #(map ut/give-id! %))))

(defn create-game [{:keys [difficulty supply]}]
  (let [{:keys [setup] :as nemesis} umbra-titan]
    (cond-> {:mode       :swift
             :real-game? true
             :difficulty (or difficulty :normal)
             :nemesis    (create-nemesis nemesis
                                         :number-of-players 2
                                         :difficulty difficulty)
             :gravehold  {:life (gravehold-starting-life difficulty)}
             :supply     (create-supply supply)
             :players    [(create-player mages/brama :difficulty difficulty)
                          (create-player mages/jian :difficulty difficulty)]
             :turn-order {:deck (->> [turn-order/player-0
                                      turn-order/player-0
                                      turn-order/player-1
                                      turn-order/player-1
                                      turn-order/nemesis
                                      turn-order/nemesis]
                                     shuffle)}}
            setup (-> (push-effect-stack {:effects setup})
                      check-stack))))
