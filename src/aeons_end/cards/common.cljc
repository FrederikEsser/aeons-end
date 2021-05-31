(ns aeons-end.cards.common
  (:require [aeons-end.operations :refer [move-cards push-effect-stack]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [medley.core :as medley]))

(defn player-starting-life [difficulty]
  (case difficulty
    :beginner 12
    :extinction 8
    10))

(defn gravehold-starting-life [difficulty]
  (case difficulty
    :beginner 35
    :extinction 25
    30))

(defn gain-aether [{:keys [current-player] :as game} {:keys [player-no arg earmark]}]
  (let [current-player? (or (nil? current-player)
                            (= current-player player-no))]
    (if earmark
      (cond-> game
              (and (pos? arg)
                   current-player?) (update-in [:players player-no :earmarked-aether earmark] ut/plus arg))
      (cond-> game
              (and (pos? arg)
                   current-player?) (update-in [:players player-no :aether] ut/plus arg)))))

(effects/register {:gain-aether gain-aether})

(defn heal [{:keys [difficulty] :as game} {:keys [player-no life]}]
  (let [player-starting-life (player-starting-life difficulty)
        current-life         (get-in game [:players player-no :life])]
    (assert (pos? current-life) "Heal error: Exhausted player cannot be healed.")
    (-> game
        (assoc-in [:players player-no :life] (min (+ current-life life)
                                                  player-starting-life)))))

(effects/register {:heal heal})

(defn discard-from-hand [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (move-cards (merge args {:from :hand
                                                             :to   :discard}))))

(effects/register {:discard-from-hand discard-from-hand})

(defn collective-discard-from-hand [game {:keys [player-card-name player-card-names]}]
  (->> (or player-card-names
           (when player-card-name
             [player-card-name]))
       (group-by :player-no)
       (medley/map-vals #(map :card-name %))
       (reduce (fn [game [player-no card-names]]
                 (push-effect-stack game {:player-no player-no
                                          :effects   [[:discard-from-hand {:card-names card-names}]]}))
               game)))

(effects/register {:collective-discard-from-hand collective-discard-from-hand})

(defn collective-discard-prepped-spells [game {:keys [spells] :as args}]
  (->> spells
       (group-by :player-no)
       (reduce (fn [game [player-no spells]]
                 (push-effect-stack game {:player-no player-no
                                          :args      args
                                          :effects   [[:discard-prepped-spells {:spells spells}]]}))
               game)))

(effects/register {:collective-discard-prepped-spells collective-discard-prepped-spells})

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

(defn destroy-prepped-spells [game {:keys [spells] :as args}]
  (push-effect-stack game {:effects (if spells
                                      (->> spells
                                           (map (fn [spell]
                                                  [:move-card (merge spell
                                                                     {:from :breach
                                                                      :to   :trash})])))
                                      [[:move-card (merge args
                                                          {:from :breach
                                                           :to   :trash})]])}))

(effects/register {:destroy-prepped-spells destroy-prepped-spells})

(defn destroy-breach [game {:keys [player-no breach-no]}]
  (let [{:keys [prepped-spells]} (get-in game [:players player-no :breaches breach-no])]
    (-> game
        (assoc-in [:players player-no :breaches breach-no] {:status :destroyed})
        (cond-> (not-empty prepped-spells) (update-in [:players player-no :discard] concat prepped-spells)))))

(effects/register {:destroy-breach destroy-breach})

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

