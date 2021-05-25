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

(defn deal-damage [{:keys [nemesis] :as game} {:keys [arg]}]
  (let [{:keys [name play-area]} nemesis
        minions (->> play-area
                     (filter (comp #{:minion} :type)))]
    (push-effect-stack game {:effects (if (not-empty minions)
                                        [[:give-choice {:text    (str "Deal " arg " damage to " (ut/format-name (or name :nemesis)) " or a Minion.")
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

(defn grubber-persistent [game _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (push-effect-stack game {:effects (if (= 2 discarded-nemesis-cards)
                                        [[:lose-nemesis-tokens 1]]
                                        [[:damage-gravehold 2]])})))

(effects/register {::grubber-persistent grubber-persistent})

(def grubber {:name       :grubber
              :type       :minion
              :tier       1
              :life       5
              :persistent {:text    "If the nemesis has two turn order cards in the turn order discard pile, Umbra Titan loses one nemesis token.\nOtherwise, Gravehold suffers 2 damage."
                           :effects [[::grubber-persistent]]}
              :quote      "'Kick it, stab it, burn it... the thing just keeps grinning.' Sparrow, Breach Mage Soldier"})

(defn seismic-roar-can-discard? [game {:keys [player-no]}]
  (let [aether (or (get-in game [:players player-no :aether]) 0)]
    (>= aether 6)))

(effects/register-predicates {::seismic-roar-can-discard? seismic-roar-can-discard?})

(def seismic-roar {:name       :seismic-roar
                   :type       :power
                   :tier       1
                   :to-discard {:text      "Spend 6 Aether."
                                :predicate ::seismic-roar-can-discard?
                                :effects   [[:pay 6]]}
                   :power      {:power   3
                                :text    "Umbra Titan loses two nemesis tokens."
                                :effects [[:lose-nemesis-tokens 2]]}
                   :quote      "'I roared as it bore through the rock. And Gravehold shuddered like those within its walls.' Nerva, Survivor"})

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

(def umbra-titan {:name       :umbra-titan
                  :difficulty 3
                  :life       70
                  :tokens     8
                  :unleash    [[::umbra-titan-unleash]]
                  :cards      [cryptid grubber seismic-roar
                               cards/aphotic-sun cards/null-scion cards/smite
                               cards/apocalypse-ritual cards/unleash-3 cards/throttle]})

(def generic-nemesis {:name       :generic
                      :difficulty 3
                      :life       70
                      :unleash    [[:damage-gravehold 2]]
                      :cards      [cryptid grubber seismic-roar
                                   cards/howling-spinners cards/nix cards/planar-collision
                                   cards/aphotic-sun cards/null-scion cards/smite
                                   cards/apocalypse-ritual cards/unleash-3 cards/throttle]})
