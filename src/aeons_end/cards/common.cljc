(ns aeons-end.cards.common
  (:require [aeons-end.operations :refer [move-cards push-effect-stack]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]))

(defn gain-aether [{:keys [current-player] :as game} {:keys [player-no arg]}]
  (let [current-player? (or (nil? current-player)
                            (= current-player player-no))]
    (cond-> game
            (and (pos? arg)
                 current-player?) (update-in [:players player-no :aether] ut/plus arg))))

(effects/register {:gain-aether gain-aether})

(defn heal [game {:keys [player-no life]}]
  (let [amount (min life
                    (- ut/player-starting-life (get-in game [:players player-no :life])))]
    (-> game
        (update-in [:players player-no :life] ut/plus amount))))

(effects/register {:heal heal})

(defn discard-from-hand [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (move-cards (merge args {:from :hand
                                                             :to   :discard}))))

(effects/register {:discard-from-hand discard-from-hand})

(defn take-from-discard [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (move-cards (merge args {:from :discard
                                                             :to   :hand}))))

(effects/register {:take-from-discard take-from-discard})

(defn discard-prepped-spells [game {:keys [player-no spells] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   (if spells
                                        (->> spells
                                             (map (fn [args]
                                                    [:move-card (merge args
                                                                       {:from :breach
                                                                        :to   :discard})])))
                                        [[:move-card (merge args
                                                            {:from :breach
                                                             :to   :discard})]])}))

(effects/register {:discard-prepped-spells discard-prepped-spells})

(defn play-twice [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:move-card {:card-name card-name
                                                              :from      :hand
                                                              :to        :play-area}]
                                                 [:card-effect {:card card}]
                                                 [:card-effect {:card card}]]}))))

(effects/register {:play-twice play-twice})

