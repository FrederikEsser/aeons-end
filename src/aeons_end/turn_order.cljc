(ns aeons-end.turn-order
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(def player-0 {:name    "Player 1"
               :type    {:player-no 0}
               :effects [[:set-current-player {:player-no 0}]]})

(def player-1 {:name    "Player 2"
               :type    {:player-no 1}
               :effects [[:set-current-player {:player-no 1}]]})

(def player-2 {:name    "Player 3"
               :type    {:player-no 2}
               :effects [[:set-current-player {:player-no 2}]]})

(def player-3 {:name    "Player 4"
               :type    {:player-no 3}
               :effects [[:set-current-player {:player-no 3}]]})

(defn draw-nemesis-card [game _]
  (let [{:keys [name type immediately]} (get-in game [:nemesis :deck 0])]
    (push-effect-stack game {:effects (concat [[:move-card {:from          :deck
                                                            :from-position :top
                                                            :to            :play-area}]]
                                              (when immediately
                                                immediately)
                                              (when (= :attack type)
                                                [[:move-card {:card-name name
                                                              :from      :play-area
                                                              :to        :discard}]]))})))

(effects/register {:draw-nemesis-card draw-nemesis-card})

(def nemesis {:name    "Nemesis"
              :type    :nemesis
              :effects [[:draw-nemesis-card]
                        [:next-turn]]})
