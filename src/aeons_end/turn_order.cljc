(ns aeons-end.turn-order
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(def player-0 {:text    "Player 1"
               :actor   {:player-no 0}
               :effects [[:set-current-player {:player-no 0}]]})

(def player-1 {:text    "Player 2"
               :actor   {:player-no 1}
               :effects [[:set-current-player {:player-no 1}]]})

(def player-2 {:text    "Player 3"
               :actor   {:player-no 2}
               :effects [[:set-current-player {:player-no 2}]]})

(def player-3 {:text    "Player 4"
               :actor   {:player-no 3}
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

(def nemesis {:text    "Nemesis"
              :actor   :nemesis
              :effects [[:draw-nemesis-card]
                        [:next-turn]]})
