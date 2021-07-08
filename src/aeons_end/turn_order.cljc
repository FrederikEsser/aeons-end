(ns aeons-end.turn-order
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(def player-1 {:name    :player-1
               :type    {:player-no 0}
               :effects [[:set-current-player {:player-no 0}]]})

(def player-2 {:name    :player-2
               :type    {:player-no 1}
               :effects [[:set-current-player {:player-no 1}]]})

(def player-3 {:name    :player-3
               :type    {:player-no 2}
               :effects [[:set-current-player {:player-no 2}]]})

(def player-4 {:name    :player-4
               :type    {:player-no 3}
               :effects [[:set-current-player {:player-no 3}]]})

(def wild {:name    :wild
           :type    :wild
           :effects [[:give-choice {:title   :turn-order
                                    :text    "Any player takes a turn."
                                    :choice  :set-current-player
                                    :options [:players]
                                    :min     1
                                    :max     1}]]})

(def nemesis {:name    :nemesis
              :type    :nemesis
              :effects [[:resolve-nemesis-cards-in-play]
                        [:draw-nemesis-card]
                        [:after-effects]
                        [:next-turn]]})

(defn draw-turn-order [{{:keys [deck discard]} :turn-order :as game} _]
  (if (empty? deck)
    (let [[card & new-deck] (shuffle discard)]
      (assoc game :turn-order {:deck    (vec new-deck)
                               :discard [card]}))
    (let [[card & new-deck] deck]
      (assoc game :turn-order {:deck    (vec new-deck)
                               :discard (concat discard [card])}))))

(defn start-turn [{{:keys [discard]} :turn-order :as game} _]
  (let [{:keys [effects]} (last discard)]
    (push-effect-stack game {:effects effects})))

(defn next-turn [{:keys [turn-order] :as game} _]
  (if turn-order
    (push-effect-stack game {:effects [[:draw-turn-order]
                                       [:start-turn]]})
    game))

(effects/register {:draw-turn-order draw-turn-order
                   :start-turn      start-turn
                   :next-turn       next-turn})

(defn reveal-top-turn-order [{{:keys [deck discard]} :turn-order :as game} _]
  (cond-> game
          (empty? deck) (assoc :turn-order {:deck (shuffle discard)})
          :always (assoc-in [:turn-order :revealed-cards] 1)))

(effects/register {:reveal-top-turn-order reveal-top-turn-order})

(defn put-turn-order-top-to-bottom [game _]
  (let [[top & deck] (get-in game [:turn-order :deck])]
    (-> game
        (cond-> top (assoc-in [:turn-order :deck] (concat deck [top])))
        (update :turn-order dissoc :revealed-cards))))

(effects/register {:put-turn-order-top-to-bottom put-turn-order-top-to-bottom})

(defn shuffle-into-turn-order-deck [{:keys [turn-order] :as game} {:keys [card-name]}]
  (let [{:keys [card idx]} (ut/get-card-idx game [:turn-order :discard] {:name card-name})
        {:keys [deck discard]} turn-order]
    (cond-> game
            card (assoc :turn-order (merge {:deck (->> (conj deck card)
                                                       shuffle
                                                       vec)}
                                           (when (> (count discard) 1)
                                             {:discard (ut/vec-remove discard idx)}))))))

(effects/register {:shuffle-into-turn-order-deck shuffle-into-turn-order-deck})
