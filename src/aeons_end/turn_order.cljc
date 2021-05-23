(ns aeons-end.turn-order
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

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

(defn resolve-power-card [game {:keys [card-name]}]
  (let [{:keys [idx card]} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
        {{:keys [power effects]} :power} card]
    (-> game
        (update-in [:nemesis :play-area idx :power :power] dec)
        (cond->
          (= 1 power) (push-effect-stack {:effects (concat effects
                                                           [[:discard-nemesis-card {:card-name card-name}]])})))))

(defn resolve-minion-card [game {:keys [card-name]}]
  (let [{:keys [effects]} (-> (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
                              :card
                              :persistent)]
    (push-effect-stack game {:effects effects})))

(defn resolve-nemesis-cards-in-play [{:keys [nemesis] :as game} _]
  (push-effect-stack game {:effects (->> (:play-area nemesis)
                                         (map (fn [{:keys [type name]}]
                                                (case type
                                                  :power [:resolve-power-card {:card-name name}]
                                                  :minion [:resolve-minion-card {:card-name name}]))))}))

(effects/register {:resolve-power-card            resolve-power-card
                   :resolve-minion-card           resolve-minion-card
                   :resolve-nemesis-cards-in-play resolve-nemesis-cards-in-play})

(defn draw-nemesis-card [game _]
  (let [{:keys [name type immediately]} (get-in game [:nemesis :deck 0])]
    (push-effect-stack game {:effects (concat [[:move-card {:from          :deck
                                                            :from-position :top
                                                            :to            :play-area}]]
                                              (when immediately
                                                immediately)
                                              (when (= :attack type)
                                                [[:discard-nemesis-card {:card-name name}]]))})))

(effects/register {:draw-nemesis-card draw-nemesis-card})

(def nemesis {:name    "Nemesis"
              :type    :nemesis
              :effects [[:resolve-nemesis-cards-in-play]
                        [:draw-nemesis-card]
                        [:next-turn]]})
