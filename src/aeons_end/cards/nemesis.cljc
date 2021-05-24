(ns aeons-end.cards.nemesis
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(def unleash-1 {:name        :unleash-1
                :text        "Unleash."
                :type        :attack
                :immediately [[:unleash]]
                :tier        1})

(def unleash-2 {:name        :unleash-2
                :text        "Unleash twice."
                :type        :attack
                :immediately [[:unleash]
                              [:unleash]]
                :tier        2})

(def unleash-3 {:name        :unleash-3
                :text        "Unleash three times."
                :type        :attack
                :immediately [[:unleash]
                              [:unleash]
                              [:unleash]]
                :tier        3})

(defn afflict-damage-player [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:give-choice {:title   :afflict
                                                      :text    "You may place a card in your discard pile into your hand."
                                                      :choice  :take-from-discard
                                                      :options [:player :discard]
                                                      :max     1}]]}))

(effects/register {::afflict-damage-player afflict-damage-player})

(def afflict {:name        :afflict
              :type        :attack
              :tier        1
              :text        ["Unleash."
                            "Any player suffers 3 damage and may place a card in their discard pile into their hand."]
              :immediately [[:unleash]
                            [:give-choice {:title   :afflict
                                           :text    "Any player suffers 3 damage and may place a card in their discard pile into their hand."
                                           :choice  ::afflict-damage-player
                                           :options [:players]
                                           :min     1
                                           :max     1}]]
              :quote       "'Such wisdom comes at a conciderable price.' Xaxos, Voidbringer"})

(defn heart-of-nothing-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :unleash [[:unleash]
                                                [:unleash]]
                                      :damage [[:give-choice {:title   :heart-of-nothing
                                                              :text    "Any player suffers 4 damage."
                                                              :choice  [:damage-player {:arg 4}]
                                                              :options [:players]
                                                              :min     1
                                                              :max     1}]])}))

(effects/register {::heart-of-nothing-choice heart-of-nothing-choice})

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
                                    :text    "Unleash twice.\nOR\nAny player suffers 4 damage."
                                    :effects [[:give-choice {:title   :heart-of-nothing
                                                             :choice  ::heart-of-nothing-choice
                                                             :options [:special
                                                                       {:option :unleash :text "Unleash twice"}
                                                                       {:option :damage :text "Any player suffers 4 damage"}]
                                                             :min     1
                                                             :max     1}]]}
                       :quote      "'Beyond our world is a vast nothing. At the center of this lies The Nameless.' Mist, Voidwalker"})

(def howling-spinners {:name       :howling-spinners
                       :type       :minion
                       :tier       1
                       :life       5
                       :persistent {:text    "Any player suffers 2 damage."
                                    :effects [[:give-choice {:title   :howling-spinners
                                                             :text    "Any player suffers 2 damage."
                                                             :choice  [:damage-player {:arg 2}]
                                                             :options [:players]
                                                             :min     1
                                                             :max     1}]]}
                       :quote      "'The sound they make as they weave echoes through the cave is like the otherworldly throes of our lost companions.' Ges, Breach Mage Adviser"})

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

(defn nix-damage-player [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 1]
                                       [:give-choice {:title   :nix
                                                      :text    "Discard the most expensive card in your hand."
                                                      :choice  :discard-from-hand
                                                      :options [:player :hand {:most-expensive true}]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::nix-damage-player nix-damage-player})

(def nix {:name        :nix
          :type        :attack
          :tier        1
          :text        ["Unleash."
                        "Any player suffers 1 damage and discards their most expensive card in hand."]
          :immediately [[:unleash]
                        [:give-choice {:title   :afflict
                                       :text    "Any player suffers 1 damage and discards their most expensive card in hand."
                                       :choice  ::nix-damage-player
                                       :options [:players]
                                       :min     1
                                       :max     1}]]
          :quote       "'It's as if the world itself is screaming.' Nerva, Survivor"})

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

(def cards (concat [afflict
                    heart-of-nothing
                    howling-spinners
                    night-unending
                    nix
                    planar-collision]
                   (concat
                     (repeat 2 unleash-1)
                     (repeat 7 unleash-2)
                     (repeat 7 unleash-3))))
