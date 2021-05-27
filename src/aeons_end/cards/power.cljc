(ns aeons-end.cards.power
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(defn apocalypse-ritual-can-discard? [game {:keys [player-no]}]
  (let [aether (or (get-in game [:players player-no :aether]) 0)]
    (>= aether 8)))

(effects/register-predicates {::apocalypse-ritual-can-discard? apocalypse-ritual-can-discard?})

(defn apocalypse-ritual-damage [game _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (push-effect-stack game {:effects [[:damage-gravehold (* 5 discarded-nemesis-cards)]]})))

(effects/register {::apocalypse-ritual-damage apocalypse-ritual-damage})

(def apocalypse-ritual {:name       :apocalypse-ritual
                        :type       :power
                        :tier       3
                        :to-discard {:text      "Spend 8 Aether."
                                     :predicate ::apocalypse-ritual-can-discard?
                                     :effects   [[:pay 8]]}
                        :power      {:power   2
                                     :text    "Gravehold suffers 5 damage for each nemesis turn order card in the turn order discard pile."
                                     :effects [[::apocalypse-ritual-damage]]}})

(defn aphotic-sun-can-discard? [game {:keys [player-no]}]
  (let [aether (or (get-in game [:players player-no :aether]) 0)]
    (>= aether 7)))

(effects/register-predicates {::aphotic-sun-can-discard? aphotic-sun-can-discard?})

(defn aphotic-sun-damage [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:spend-charges]]}))

(effects/register {::aphotic-sun-damage aphotic-sun-damage})

(def aphotic-sun {:name       :aphotic-sun
                  :type       :power
                  :tier       2
                  :to-discard {:text      "Spend 7 Aether."
                               :predicate ::aphotic-sun-can-discard?
                               :effects   [[:pay 7]]}
                  :power      {:power   2
                               :text    "Unleash. The player with the most charges suffers 3 damage and loses all of their charges."
                               :effects [[:unleash]
                                         [:give-choice {:title   :aphotic-sun
                                                        :text    "The player with the most charges suffers 3 damage and loses all of their charges."
                                                        :choice  ::aphotic-sun-damage
                                                        :options [:players {:most-charges true}]
                                                        :min     1
                                                        :max     1}]]}
                  :quote      "'The harsh light of the dead star burned away the dark as it crept through the breach. And around it I saw ravaged worlds The Nameless had already claimed.' ― Indira, Breach Mage Apprentice"})

(defn heart-of-nothing-can-discard? [game {:keys [player-no]}]
  (let [cards-in-hand (->> (get-in game [:players player-no :hand])
                           count)]
    (>= cards-in-hand 4)))

(effects/register-predicates {::heart-of-nothing-can-discard? heart-of-nothing-can-discard?})

(def heart-of-nothing {:name       :heart-of-nothing
                       :type       :power
                       :tier       1
                       :to-discard {:text      "Discard four cards in hand."
                                    :predicate ::heart-of-nothing-can-discard?
                                    :effects   [[:give-choice {:title   :heart-of-nothing
                                                               :text    "Discard four cards in hand."
                                                               :choice  :discard-from-hand
                                                               :options [:player :hand]
                                                               :min     4
                                                               :max     4}]]}
                       :power      {:power   2
                                    :text    ["Any player suffers 4 damage."
                                              "OR"
                                              "Unleash twice."]
                                    :effects [[:give-choice {:title     :heart-of-nothing
                                                             :text      "Any player suffers 4 damage."
                                                             :choice    [:damage-player {:arg 4}]
                                                             :or-choice {:text    "Unleash twice"
                                                                         :effects [[:unleash]
                                                                                   [:unleash]]}
                                                             :options   [:players]
                                                             :max       1}]]}
                       :quote      "'Beyond our world is a vast nothing. At the center of this lies The Nameless.' Mist, Voidwalker"})

(def morbid-gyre {:name       :morbid-gyre
                  :type       :power
                  :tier       2
                  :to-discard {:text      "Spend 7 Aether."
                               :predicate ::aphotic-sun-can-discard?
                               :effects   [[:pay 7]]}
                  :power      {:power   1
                               :text    ["Unleash twice."
                                         "The players collectively discard three cards in hand."]
                               :effects [[:unleash]
                                         [:unleash]
                                         [:give-choice {:title   :morbid-gyre
                                                        :text    "The players collectively discard three cards in hand."
                                                        :choice  :collective-discard-from-hand
                                                        :options [:collective-hands]
                                                        :min     3
                                                        :max     3}]]}
                  :quote      "'It churned and rolled , a maelstrom of malign power. The void was upon me and yet I felt only the throes of freedom from my prison of sleep.' ― Yan Magda, Enlightened Exile"})

(defn night-unending-damage [{:keys [players] :as game} _]
  (let [most-prepped-spells (->> players
                                 (map (fn [{:keys [breaches]}]
                                        (->> breaches
                                             (mapcat :prepped-spells)
                                             count)))
                                 (apply max))]
    (push-effect-stack game {:effects [[:damage-gravehold (* 2 most-prepped-spells)]]})))

(effects/register {::night-unending-damage night-unending-damage})

(def night-unending {:name  :night-unending
                     :type  :power
                     :tier  1
                     :power {:power   3
                             :text    "Gravehold suffers 2 damage for each spell prepped by the player with the most prepped spells."
                             :effects [[::night-unending-damage]]}
                     :quote "'Here beneath The World That Was there is no day, no time really, just night unending.' Xaxos, Voidbringer"})

(defn planar-collision-can-discard? [game {:keys [player-no]}]
  (let [prepped-spells (->> (get-in game [:players player-no :breaches])
                            (remove (comp #{:closed} :status))
                            (mapcat :prepped-spells)
                            count)]
    (>= prepped-spells 2)))

(effects/register-predicates {::planar-collision-can-discard? planar-collision-can-discard?})

(def planar-collision {:name       :planar-collision
                       :type       :power
                       :tier       1
                       :to-discard {:text      "Discard two prepped spells."
                                    :predicate ::planar-collision-can-discard?
                                    :effects   [[:give-choice {:title   :planar-collision
                                                               :text    "Discard two prepped spells."
                                                               :choice  :discard-prepped-spells
                                                               :options [:prepped-spells {:own true}]
                                                               :min     2
                                                               :max     2}]]}
                       :power      {:power   2
                                    :text    "Unleash twice."
                                    :effects [[:unleash]
                                              [:unleash]]}
                       :quote      "'None remembered the true name of The World That Was until Yan Magda spoke it aloud: Khasad Vol.'"})
