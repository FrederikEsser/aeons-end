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
           :type    {:player-no -1}
           :effects [[:give-choice {:title   :turn-order
                                    :text    "Any player takes a turn."
                                    :choice  :set-current-player
                                    :options [:players]
                                    :min     1
                                    :max     1}]]})

(defn set-resolving [game {:keys [card-name]}]
  (assoc game :resolving card-name))

(defn clear-resolving [game _]
  (dissoc game :resolving))

(effects/register {:set-resolving   set-resolving
                   :clear-resolving clear-resolving})

(defn resolve-power-card [game {:keys [card-name]}]
  (let [{:keys [idx card]} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
        {{:keys [power effects]} :power} card]
    (-> game
        (update-in [:nemesis :play-area idx :power :power] dec)
        (cond->
          (= 1 power) (push-effect-stack {:effects (concat [[:set-resolving {:card-name card-name}]]
                                                           effects
                                                           [[:clear-resolving]]
                                                           [[:discard-nemesis-card {:card-name card-name}]])})))))

(defn resolve-minion-card [game {:keys [card-name]}]
  (let [{:keys [effects]} (-> (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
                              :card
                              :persistent)]
    (push-effect-stack game {:effects (concat [[:set-resolving {:card-name card-name}]]
                                              effects
                                              [[:clear-resolving]])})))

(defn resolve-nemesis-cards-in-play [{:keys [nemesis] :as game} _]
  (push-effect-stack game {:effects (->> (:play-area nemesis)
                                         (map (fn [{:keys [type name]}]
                                                (case type
                                                  :power [:resolve-power-card {:card-name name}]
                                                  :minion [:resolve-minion-card {:card-name name}]))))}))

(effects/register {:resolve-power-card            resolve-power-card
                   :resolve-minion-card           resolve-minion-card
                   :resolve-nemesis-cards-in-play resolve-nemesis-cards-in-play})

(defn set-minion-max-life [game {:keys [card-name life]}]
  (ut/update-in-vec game [:nemesis :play-area] {:name card-name} assoc :max-life life))

(defn draw-nemesis-card [game _]
  (let [{:keys [name type life effects]} (get-in game [:nemesis :deck 0])]
    (push-effect-stack game {:effects (concat [[:move-card {:from          :deck
                                                            :from-position :top
                                                            :to            :play-area}]]
                                              (when (= :minion type)
                                                [[:set-minion-max-life {:card-name name
                                                                        :life      life}]])
                                              (when (= :attack type)
                                                (concat
                                                  [[:set-resolving {:card-name name}]]
                                                  effects
                                                  [[:clear-resolving]]
                                                  [[:discard-nemesis-card {:card-name name}]])))})))

(effects/register {:set-minion-max-life set-minion-max-life
                   :draw-nemesis-card   draw-nemesis-card})

(def nemesis {:name    :nemesis
              :type    :nemesis
              :effects [[:resolve-nemesis-cards-in-play]
                        [:draw-nemesis-card]
                        [:next-turn]]})
