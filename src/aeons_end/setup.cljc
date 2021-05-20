(ns aeons-end.setup
  (:require [aeons-end.cards.gems :as gems]
            [aeons-end.cards.relics :as relics]
            [aeons-end.cards.spells :as spells]
            [aeons-end.mages :as mages]
            [aeons-end.turn-order :as turn-order]))

(defn create-player [{:keys [breaches ability] :as mage}]
  (merge mage
         {:breaches (->> breaches
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
          :life     1
          :phase    :out-of-turn}))

(defn create-game []
  {:mode       :swift
   :nemesis    {:life 50}
   :supply     [{:card gems/jade :pile-size 7}
                {:card gems/alien-element :pile-size 7}
                {:card gems/pain-stone :pile-size 7}
                {:card relics/unstable-prism :pile-size 5}
                {:card spells/amplify-vision :pile-size 5}
                {:card spells/ignite :pile-size 5}
                {:card spells/dark-fire :pile-size 5}
                {:card spells/radiance :pile-size 5}]
   :players    [(create-player mages/brama)
                (create-player mages/mist)]
   :turn-order {:deck (->> [turn-order/player-0
                            turn-order/player-0
                            turn-order/player-1
                            turn-order/player-1]
                           shuffle)}})
