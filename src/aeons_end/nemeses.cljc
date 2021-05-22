(ns aeons-end.nemeses
  (:require [aeons-end.operations :refer [push-effect-stack give-choice]]
            [aeons-end.effects :as effects]
            [aeons-end.cards.nemesis :as cards]))

(defn unleash [game _]
  (let [effects (get-in game [:nemesis :unleash])]
    (push-effect-stack game {:effects effects})))

(effects/register {:unleash unleash})

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
                                     (filter (comp #{:nemesis} :actor))
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

(def umbra-titan {:name       :umbra-titan
                  :difficulty 3
                  :life       80
                  :tokens     5
                  :unleash    [[::umbra-titan-unleash]]
                  :cards      [cards/unleash-1 cards/unleash-1 cards/unleash-1
                               cards/unleash-2 cards/unleash-2 cards/unleash-2
                               cards/unleash-3 cards/unleash-3 cards/unleash-3]})
