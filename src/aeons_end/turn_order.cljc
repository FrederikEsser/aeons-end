(ns aeons-end.turn-order
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]
            [aeons-end.nemesis :as nemesis]))

(def nemesis {:name    :nemesis
              :type    :nemesis
              :effects [[::nemesis/at-start-turn]
                        [:resolve-nemesis-cards-in-play]
                        [:draw-nemesis-card]
                        [::nemesis/at-end-turn]
                        [:next-turn]]})

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

(defn cloose-player [{:keys [players] :as game} {:keys [player-nos]}]
  (let [player-with-token (->> players
                               (keep-indexed (fn [player-no {:keys [turn-order-token]}]
                                               (when (and (contains? player-nos player-no)
                                                          turn-order-token)
                                                 player-no)))
                               first)]
    (if player-with-token
      (let [next-player (-> player-nos
                            (clojure.set/difference #{player-with-token})
                            first)]
        (-> game
            (update-in [:players player-with-token] dissoc :turn-order-token)
            (push-effect-stack {:effects [[:set-current-player {:player-no next-player}]]})))
      (push-effect-stack game {:effects [[:give-choice {:title   :turn-order
                                                        :text    (str "Player "
                                                                      (case player-nos
                                                                        #{0 1} "1 or 2"
                                                                        #{2 3} "3 or 4")
                                                                      " takes a turn.")
                                                        :choice  [:set-current-player {:turn-order-token (case player-nos
                                                                                                           #{0 1} :player-1-2
                                                                                                           #{2 3} :player-3-4)}]
                                                        :options [:players {:player-nos player-nos}]
                                                        :min     1
                                                        :max     1}]]}))))

(effects/register {::cloose-player cloose-player})

(def player-1-2 {:name    "Player 1 / 2"
                 :type    {:player-nos #{0 1}}
                 :effects [[::cloose-player {:player-nos #{0 1}}]]})

(def player-3-4 {:name    "Player 3 / 4"
                 :type    {:player-nos #{2 3}}
                 :effects [[::cloose-player {:player-nos #{2 3}}]]})

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

(defn reveal-turn-order-deck [{{:keys [deck]} :turn-order :as game} _]
  (-> game
      (assoc-in [:turn-order :revealed] deck)
      (update :turn-order dissoc :deck :revealed-cards)))

(effects/register {:reveal-turn-order-deck reveal-turn-order-deck})

(defn topdeck-revealed-turn-order-cards [game {:keys [card-name card-names]}]
  (let [card-names (or card-names
                       (when card-name
                         [card-name]))
        {:keys [revealed deck]} (get game :turn-order)
        new-deck   (->> card-names
                        (map (fn [card-name]
                               (->> revealed
                                    (filter (comp #{card-name} :name))
                                    first)))
                        (concat deck))]
    (assert (= (sort card-names) (->> revealed (map :name) sort)))
    (-> game
        (assoc-in [:turn-order :revealed-cards] (count revealed))
        (assoc-in [:turn-order :deck] new-deck)
        (update :turn-order dissoc :revealed))))

(effects/register {:topdeck-revealed-turn-order-cards topdeck-revealed-turn-order-cards})

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
