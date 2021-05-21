(ns aeons-end.turn-order)

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

(def nemesis {:text    "Nemesis"
              :actor   :nemesis
              :effects [[:unleash]
                        [:next-turn]]})
