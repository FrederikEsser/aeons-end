(ns aeons-end.cards.power
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn can-afford? [game {:keys [player-no amount]}]
  (let [player (get-in game [:players player-no])]
    (ut/can-afford? player amount :discard-power-card)))

(defn cards-in-hand? [game {:keys [player-no amount]}]
  (let [cards-in-hand (->> (get-in game [:players player-no :hand])
                           count)]
    (>= cards-in-hand amount)))


(effects/register-predicates {::can-afford?    can-afford?
                              ::cards-in-hand? cards-in-hand?})

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
                                     :predicate [::can-afford? {:amount 8}]
                                     :effects   [[:pay {:amount 8
                                                        :type   :discard-power-card}]]}
                        :power      {:power   2
                                     :text    "Gravehold suffers 5 damage for each nemesis turn order card in the turn order discard pile."
                                     :effects [[::apocalypse-ritual-damage]]}})

(defn aphotic-sun-damage [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:spend-charges]]}))

(effects/register {::aphotic-sun-damage aphotic-sun-damage})

(def aphotic-sun {:name       :aphotic-sun
                  :type       :power
                  :tier       2
                  :to-discard {:text      "Spend 7 Aether."
                               :predicate [::can-afford? {:amount 7}]
                               :effects   [[:pay {:amount 7
                                                  :type   :discard-power-card}]]}
                  :power      {:power   2
                               :text    "Unleash. The player with the most charges suffers 3 damage and loses all of their charges."
                               :effects [[:unleash]
                                         [:give-choice {:title   :aphotic-sun
                                                        :text    "The player with the most charges suffers 3 damage and loses all of their charges."
                                                        :choice  ::aphotic-sun-damage
                                                        :options [:players {:most-charges true}]
                                                        :min     1
                                                        :max     1}]]}
                  :quote      "'The harsh light of the dead star burned away the dark as it crept through the breach. And around it I saw ravaged worlds The Nameless had already claimed.' Indira, Breach Mage Apprentice"})

(defn cataclysmic-fate-can-discard? [game {:keys [player-no]}]
  (let [prepped-spells (->> (get-in game [:players player-no :breaches])
                            (mapcat :prepped-spells))]
    (some (comp #(>= % 5) :cost) prepped-spells)))

(effects/register-predicates {::cataclysmic-fate-can-discard? cataclysmic-fate-can-discard?})

(defn cataclysmic-fate-destroy-breach [game {:keys [player-no breach-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:destroy-prepped-spells {:breach-no breach-no
                                                                 :card-name card-name}]
                                       [:destroy-breach {:breach-no             breach-no
                                                         :put-prepped-spells-in :discard}]]}))

(effects/register {::cataclysmic-fate-destroy-breach cataclysmic-fate-destroy-breach})

(def cataclysmic-fate {:name       :cataclysmic-fate
                       :type       :power
                       :tier       3
                       :to-discard {:text      "Destroy a prepped spell that costs 5 Aether or more and a breach in which it was prepped."
                                    :predicate ::cataclysmic-fate-can-discard?
                                    :effects   [[:give-choice {:title   :cataclysmic-fate
                                                               :text    "Destroy a prepped spell that costs 5 Aether or more and a breach in which it was prepped."
                                                               :choice  ::cataclysmic-fate-destroy-breach
                                                               :options [:player :prepped-spells {:min-cost 5}]
                                                               :min     1
                                                               :max     1}]]}
                       :power      {:power   1
                                    :text    "The player with the lowest life suffers 4 damage."
                                    :effects [[:give-choice {:title   :cataclysmic-fate
                                                             :text    "The player with the lowest life suffers 4 damage."
                                                             :choice  [:damage-player {:arg 4}]
                                                             :options [:players {:least-life true}]
                                                             :min     1
                                                             :max     1}]]}})

(defn chaos-flail-destroy [game {:keys [player-no]}]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   [[:shuffle-discard-into-deck]
                                      [:reveal-from-deck 2]
                                      [:give-choice {:title   :chaos-flail
                                                     :text    "Destroy the most expensive card revealed."
                                                     :choice  :trash-from-revealed
                                                     :options [:player :revealed {:most-expensive true}]
                                                     :min     1
                                                     :max     1}]
                                      [:topdeck-all-revealed]]})))

(effects/register {::chaos-flail-destroy chaos-flail-destroy})

(def chaos-flail {:name       :chaos-flail
                  :type       :power
                  :tier       2
                  :to-discard {:text      "Spend 7 Aether."
                               :predicate [::can-afford? {:amount 7}]
                               :effects   [[:pay {:amount 7
                                                  :type   :discard-power-card}]]}
                  :power      {:power   2
                               :text    ["Unleash twice."
                                         "Any player places their discard pile on top of their deck and shuffles it."
                                         "Then, that player reveals the top two cards of their deck and destroys the most expensive card revealed."]
                               :effects [[:unleash]
                                         [:unleash]
                                         [:give-choice {:title   :chaos-flail
                                                        :text    "Any player places their discard pile on top of their deck and shuffles it.\nThen, that player reveals the top two cards of their deck and destroys the most expensive card revealed."
                                                        :choice  ::chaos-flail-destroy
                                                        :options [:players {:min-deck+discard 1}]
                                                        :min     1
                                                        :max     1}]]}})

(def dire-abbatoir {:name       :dire-abbatoir
                    :type       :power
                    :tier       3
                    :to-discard {:text      "Spend 8 Aether."
                                 :predicate [::can-afford? {:amount 8}]
                                 :effects   [[:pay {:amount 8
                                                    :type   :discard-power-card}]]}
                    :power      {:power   2
                                 :text    "The player with the most life suffers damage equal to their current life."
                                 :effects [[:give-choice {:title   :dire-abbatoir
                                                          :text    "The player with the most life suffers damage equal to their current life."
                                                          :choice  :kill-player
                                                          :options [:players {:most-life true}]
                                                          :min     1
                                                          :max     1}]]}})

(defn heart-of-nothing-can-discard? [game {:keys [player-no]}]
  (let [cards-in-hand (->> (get-in game [:players player-no :hand])
                           count)]
    (>= cards-in-hand 4)))

(effects/register-predicates {::heart-of-nothing-can-discard? heart-of-nothing-can-discard?})

(def heart-of-nothing {:name       :heart-of-nothing
                       :type       :power
                       :tier       1
                       :to-discard {:text      "Discard four cards in hand."
                                    :predicate [::cards-in-hand? {:amount 4}]
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
                               :predicate [::can-afford? {:amount 7}]
                               :effects   [[:pay {:amount 7
                                                  :type   :discard-power-card}]]}
                  :power      {:power   1
                               :text    ["Unleash twice."
                                         "The players collectively discard three cards in hand."]
                               :effects [[:unleash]
                                         [:unleash]
                                         [:give-choice {:title   :morbid-gyre
                                                        :text    "The players collectively discard three cards in hand."
                                                        :choice  :collective-discard-from-hand
                                                        :options [:players :hand]
                                                        :min     3
                                                        :max     3}]]}
                  :quote      "'It churned and rolled, a maelstrom of malign power. The void was upon me and yet I felt only the throes of freedom from my prison of sleep.' Yan Magda, Enlightened Exile"})

(defn night-unending-damage [{:keys [players] :as game} _]
  (let [most-prepped-spells (->> players
                                 (map ut/count-prepped-spells)
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
                            (remove (comp #{:closed} :status)) ; spells prepped in closed breaches have to be cast before entering the Main phase
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
                                                               :options [:player :prepped-spells]
                                                               :min     2
                                                               :max     2}]]}
                       :power      {:power   2
                                    :text    "Unleash twice."
                                    :effects [[:unleash]
                                              [:unleash]]}
                       :quote      "'None remembered the true name of The World That Was until Yan Magda spoke it aloud: Khasad Vol.'"})

(defn withering-beam-destroy-spells [{:keys [players] :as game} _]
  (let [sorted-spells        (->> players
                                  (map-indexed (fn [player-no {:keys [breaches]}]
                                                 (->> breaches
                                                      (map-indexed (fn [breach-no breach]
                                                                     (->> (:prepped-spells breach)
                                                                          (map (fn [{:keys [name cost]}]
                                                                                 {:player-no player-no
                                                                                  :breach-no breach-no
                                                                                  :card-name name
                                                                                  :cost      cost})))))
                                                      (apply concat))))
                                  (apply concat)
                                  (sort-by :cost >))
        [_ cost-2 cost-3] (map :cost sorted-spells)
        auto-destroy-cards   (cond
                               (<= (count sorted-spells) 2) sorted-spells
                               (not= cost-2 cost-3) (take 2 sorted-spells)
                               :else (->> sorted-spells
                                          (filter (comp #(> % cost-2) :cost))))
        manual-destroy-count (- (min 2 (count sorted-spells))
                                (count auto-destroy-cards))]
    (push-effect-stack game {:effects (concat
                                        (when (not-empty auto-destroy-cards)
                                          [[:destroy-prepped-spells {:spells auto-destroy-cards}]])
                                        (when (pos? manual-destroy-count)
                                          [[:give-choice {:title   :withering-beam
                                                          :text    (str "The players collectively destroy the "
                                                                        (when (> manual-destroy-count 1)
                                                                          (ut/number->text manual-destroy-count))
                                                                        " most expensive prepped spell"
                                                                        (when (> manual-destroy-count 1)
                                                                          "s")
                                                                        ".")
                                                          :choice  :destroy-prepped-spells
                                                          :options [:players :prepped-spells {:min-cost cost-2}]
                                                          :min     manual-destroy-count
                                                          :max     manual-destroy-count}]]))})))

(effects/register {::withering-beam-destroy-spells withering-beam-destroy-spells})

(def withering-beam {:name  :withering-beam
                     :type  :power
                     :tier  3
                     :power {:power   2
                             :text    "Unleash twice. The players collectively destroy the two most expensive prepped spells."
                             :effects [[:unleash]
                                       [:unleash]
                                       [::withering-beam-destroy-spells]]}
                     :quote "'I watched a fellow merchant atrophy and fall to the ground in ash as the beam hit his cart.' Ohat, Dirt Merchant"})

(defn generic [tier & [idx]]
  (let [name (str (case tier
                    1 "Minor"
                    2 "Medium"
                    3 "Major")
                  " Power"
                  (when idx
                    (str " (" idx ")")))]
    {:name       name
     :type       :power
     :tier       tier
     :to-discard (case tier
                   1 {:text      "Spend 6 Aether."
                      :predicate [::can-afford? {:amount 6}]
                      :effects   [[:pay {:amount 6 :type :discard-power-card}]]}
                   2 {:text      "Spend 7 Aether."
                      :predicate [::can-afford? {:amount 7}]
                      :effects   [[:pay {:amount 7 :type :discard-power-card}]]}
                   3 {:text      "Spend 8 Aether."
                      :predicate [::can-afford? {:amount 8}]
                      :effects   [[:pay {:amount 8 :type :discard-power-card}]]})
     :power      (case tier
                   1 {:power   2
                      :text    "Unleash twice."
                      :effects [[:unleash]
                                [:unleash]]}
                   2 {:power   2
                      :text    "Unleash 4 times."
                      :effects [[:unleash]
                                [:unleash]
                                [:unleash]
                                [:unleash]]}
                   3 {:power   2
                      :text    ["Any player suffers 6 damage."
                                "OR"
                                "Gravehold suffers 6 damage."]
                      :effects [[:give-choice {:title     name
                                               :text      "Any player suffers 6 damage."
                                               :choice    [:damage-player {:arg 6}]
                                               :or-choice {:text    "Gravehold suffers 6 damage."
                                                           :effects [[:damage-gravehold 6]]}
                                               :options   [:players]
                                               :max       1}]]})}))
