(ns aeons-end.setup
  (:require [aeons-end.cards.gems :as gems]
            [aeons-end.mages :as mages]
            [aeons-end.turn-order :as turn-order]))

(defn create-player [{:keys [breaches] :as mage}]
  (merge mage
         {:breaches (mapv merge
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
                          breaches)
          :life     10
          :charges  0
          :aether   0
          :phase    :out-of-turn}))

(defn create-game []
  {:nemesis    {:life 50}
   :supply     [{:card gems/jade :pile-size 7}]
   :players    [(create-player mages/brama)
                (create-player mages/mist)]
   :turn-order {:deck (->> [turn-order/player-0
                            turn-order/player-0
                            turn-order/player-1
                            turn-order/player-1]
                           shuffle)}})
