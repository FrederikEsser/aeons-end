(ns aeons-end.nemeses
  (:require [aeons-end.operations :refer [push-effect-stack give-choice move-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.nemesis :as cards]))

(defn unleash [game _]
  (let [effects (get-in game [:nemesis :unleash])]
    (push-effect-stack game {:effects effects})))

(effects/register {:unleash unleash})

(defn discard-nemesis-card [game {:keys [card-name]}]
  (move-card game {:card-name card-name
                   :from      :play-area
                   :to        :discard}))

(effects/register {:discard-nemesis-card discard-nemesis-card})

(defn deal-damage-to-nemesis [game {:keys [damage]}]
  (let [life (get-in game [:nemesis :life])]
    (assoc-in game [:nemesis :life] (max (- life damage) 0))))

(defn deal-damage-to-minion [game {:keys [card-name damage]}]
  (let [{:keys [card idx]} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
        {:keys [life]} card]
    (-> game
        (assoc-in [:nemesis :play-area idx :life] (max (- life damage) 0))
        (cond-> (<= life damage) (discard-nemesis-card {:card-name card-name})))))

(defn deal-damage-to-target [game {:keys [damage choice]}]
  (let [{:keys [area card-name]} choice]
    (push-effect-stack game {:effects (case area
                                        :nemesis [[:deal-damage-to-nemesis {:damage damage}]]
                                        :minions [[:deal-damage-to-minion {:card-name card-name :damage damage}]])})))

(defn deal-damage [game {:keys [arg]}]
  (let [minions (->> (get-in game [:nemesis :play-area])
                     (filter (comp #{:minion} :type)))]
    (push-effect-stack game {:effects (if (not-empty minions)
                                        [[:give-choice {:text    "Deal damage to Nemesis or a Minion."
                                                        :choice  [:deal-damage-to-target {:damage arg}]
                                                        :options [:mixed
                                                                  [:nemesis]
                                                                  [:minions]]
                                                        :min     1
                                                        :max     1}]]
                                        [[:deal-damage-to-nemesis {:damage arg}]])})))

(effects/register {:deal-damage-to-nemesis deal-damage-to-nemesis
                   :deal-damage-to-minion  deal-damage-to-minion
                   :deal-damage-to-target  deal-damage-to-target
                   :deal-damage            deal-damage})

(defn lose-nemesis-tokens [game {:keys [arg]}]
  (update-in game [:nemesis :tokens] - arg))

(effects/register {:lose-nemesis-tokens lose-nemesis-tokens})

(defn damage-gravehold [game {:keys [arg]}]
  (update-in game [:gravehold :life] - arg))

(effects/register {:damage-gravehold damage-gravehold})

(defn damage-player [game {:keys [player-no arg]}]
  (update-in game [:players player-no :life] - arg))

(effects/register {:damage-player damage-player})

(defn umbra-titan-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :damage [[:damage-gravehold 2]]
                                      :token [[:lose-nemesis-tokens 1]])}))

(defn umbra-titan-unleash [game _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (assert (<= 1 discarded-nemesis-cards 2) (str "Turn order error: There are " discarded-nemesis-cards " nemesis turn order cards in the turn order discard pile."))
    (give-choice game (case discarded-nemesis-cards
                        1 {:title     :unleash
                           :text      "Any player suffers 2 damage."
                           :choice    [:damage-player {:arg 2}]
                           :or-choice {:text    "Umbra titan loses one nemesis token"
                                       :effects [[:lose-nemesis-tokens 1]]}
                           :options   [:players]
                           :max       1}
                        2 {:title   :unleash
                           :choice  ::umbra-titan-choice
                           :options [:special
                                     {:option :damage :text "Gravehold suffers 2 damage"}
                                     {:option :token :text "Umbra titan loses one nemesis token"}]
                           :min     1
                           :max     1}))))

(effects/register {::umbra-titan-choice  umbra-titan-choice
                   ::umbra-titan-unleash umbra-titan-unleash})

(def cryptid {:name       :cryptid
              :type       :minion
              :tier       1
              :life       6
              :persistent {:text    "The player with the most expensive prepped spell discards that spell.\nOR\nUmbra Titan loses one nemesis token."
                           :effects [[:give-choice {:title     :cryptid
                                                    :text      "The player with the most expensive prepped spell discards that spell."
                                                    :choice    :discard-prepped-spells
                                                    :or-choice {:text    "Umbra titan loses one nemesis token"
                                                                :effects [[:lose-nemesis-tokens 1]]}
                                                    :options   [:prepped-spells {:most-expensive true}]
                                                    :max       1}]]}
              :quote      "'The beasts of this cave seem to revere the Titan as though it were some ancient god.' Mazhaedron, Henge Mystic"})

(def umbra-titan {:name       :umbra-titan
                  :difficulty 3
                  :life       70
                  :tokens     8
                  :unleash    [[::umbra-titan-unleash]]
                  :cards      [cryptid cards/unleash-1 cards/unleash-1
                               cards/unleash-2 cards/unleash-2 cards/unleash-2
                               cards/unleash-3 cards/unleash-3 cards/unleash-3]})
