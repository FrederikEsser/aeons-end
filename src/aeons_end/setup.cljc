(ns aeons-end.setup
  (:require [aeons-end.cards.nemesis :as nemesis-cards]
            [aeons-end.cards.gems :as gems]
            [aeons-end.cards.relics :as relics]
            [aeons-end.cards.spells :as spells]
            [aeons-end.mages :as mages]
            [aeons-end.nemeses :as nemeses]
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

(defn create-nemesis [{:keys [cards] :as nemesis} number-of-players]
  (-> nemesis
      (dissoc :cards)
      (merge
        {:deck (->> (range 1 4)
                    (mapcat (fn [tier]
                              (->> nemesis-cards/basic-cards
                                   (filter (comp #{tier} :tier))
                                   shuffle
                                   (take (get-in basic-nemesis-cards [number-of-players tier]))
                                   (concat (->> cards
                                                (filter (comp #{tier} :tier))))
                                   shuffle)))
                    vec)})))

(defn create-player [{:keys [breaches ability] :as mage}]
  (-> mage
      (merge {:breaches (->> breaches
                             (mapv merge
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
                                     :bonus-damage 1}]
                                   ))
              :ability  (merge ability
                               {:charges 0})
              :life     ut/player-starting-life
              :phase    :out-of-turn})
      (update :hand #(map ut/give-id! %))
      (update :deck #(map ut/give-id! %))))

(defn create-game []
  {:mode       :swift
   :nemesis    (create-nemesis nemeses/umbra-titan 2)
   :gravehold  {:life ut/gravehold-starting-life}
   :supply     [{:card gems/jade :pile-size 7}
                {:card gems/alien-element :pile-size 7}
                {:card gems/pain-stone :pile-size 7}
                {:card relics/unstable-prism :pile-size 5}
                {:card relics/vortex-gauntlet :pile-size 5}
                {:card spells/amplify-vision :pile-size 5}
                {:card spells/ignite :pile-size 5}
                {:card spells/dark-fire :pile-size 5}
                {:card spells/radiance :pile-size 5}]
   :players    [(create-player mages/brama)
                (create-player mages/mist)]
   :turn-order {:deck (->> [turn-order/player-0
                            turn-order/player-0
                            turn-order/player-1
                            turn-order/player-1
                            turn-order/nemesis
                            turn-order/nemesis]
                           shuffle)}})
