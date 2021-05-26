(ns aeons-end.cards.nemesis
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

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

(def mangleroot {:name       :mangleroot
                 :type       :minion
                 :tier       2
                 :life       12
                 :persistent {:text    "Gravehold suffers 3 damage.\nThis minion suffers 2 damage."
                              :effects [[:damage-gravehold 3]
                                        [:deal-damage-to-minion {:card-name :mangleroot
                                                                 :damage    2}]]}
                 :quote      "'One of the few indigenous creatures of the cave, though corrupted from exposure to the breach.' ― Gex, Breach Mage Advisor"})

(def morbid-gyre {:name       :morbid-gyre
                  :type       :power
                  :tier       2
                  :to-discard {:text      "Spend 7 Aether."
                               :predicate ::aphotic-sun-can-discard?
                               :effects   [[:pay 7]]}
                  :power      {:power   1
                               :text    "Unleash twice.\nThe players collectively discard three cards in hand."
                               :effects [[:unleash]
                                         [:unleash]
                                         [:give-choice {:title   :morbid-gyre
                                                        :text    "The players collectively discard three cards in hand."
                                                        :choice  :collective-discard-from-hand
                                                        :options [:collective-hands]
                                                        :min     3
                                                        :max     3}]]}
                  :quote      "'It churned and rolled , a maelstrom of malign power. The void was upon me and yet I felt only the throes of freedom from my prison of sleep.' ― Yan Magda, Enlightened Exile"})

(defn monstrosity-of-omens-modify-damage [damage]
  (min damage 1))

(effects/register-predicates {::monstrosity-of-omens-modify-damage monstrosity-of-omens-modify-damage})

(defn monstrosity-of-omens-damage [game _]
  (let [{{:keys [life]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name :monstrosity-of-omens})]
    (push-effect-stack game {:effects [[:damage-gravehold life]]})))

(effects/register {::monstrosity-of-omens-damage monstrosity-of-omens-damage})

(def monstrosity-of-omens {:name          :monstrosity-of-omens
                           :type          :minion
                           :tier          3
                           :life          5
                           :modify-damage ::monstrosity-of-omens-modify-damage
                           :text          "When damage is dealt to this minion, reduce that damage to 1."
                           :persistent    {:text    "Gravehold suffers damage equal to this minion's current life."
                                           :effects [[::monstrosity-of-omens-damage]]}
                           :quote         "'Never have I seen such a creature, even among the ranks of the Nameless.' ― Yan Magda, Enlightened Exile"})

(def mutilate {:name        :mutilate
               :type        :attack
               :tier        2
               :text        ["Unleash."
                             "The players collectively discard two prepped spells. Any player suffers 2 damage."]
               :immediately [[:unleash]
                             [:give-choice {:title   :mutilate
                                            :text    "The players collectively discard two prepped spells."
                                            :choice  :collective-discard-prepped-spells
                                            :options [:prepped-spells]
                                            :min     2
                                            :max     2}]
                             [:give-choice {:title   :mutilate
                                            :text    "Any player suffers 2 damage."
                                            :choice  [:damage-player {:arg 2}]
                                            :options [:players]
                                            :min     1
                                            :max     1}]]
               :quote       "'They say that before The Nameless found Gravehold, nothing grew in these caves. It appears the cave burgeons through our blood.' ― Gex, Breach Mage Advisor"})

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
                        [:give-choice {:title   :nix
                                       :text    "Any player suffers 1 damage and discards their most expensive card in hand."
                                       :choice  ::nix-damage-player
                                       :options [:players]
                                       :min     1
                                       :max     1}]]
          :quote       "'It's as if the world itself is screaming.' Nerva, Survivor"})

(def null-scion {:name       :null-scion
                 :type       :minion
                 :tier       2
                 :life       11
                 :persistent {:text    "Unleash."
                              :effects [[:unleash]]}
                 :quote      "'Qiulius's first blow glanced off its carapace like breath, but her second blow carved it in twain.' ― Dezmodia, Voidborn Prodigy"})

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

(defn quell-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :damage [[:damage-gravehold 7]]
                                      :unleash [[:unleash]
                                                [:unleash]
                                                [:unleash]])}))

(effects/register {::quell-choice quell-choice})

(def quell {:name        :quell
            :type        :attack
            :tier        3
            :text        ["Gravehold suffers 7 damage.\nOR\nUnleash three times."]
            :immediately [[:give-choice {:title   :quell
                                         :choice  ::quell-choice
                                         :options [:special
                                                   {:option :damage :text "Gravehold suffers 7 damage."}
                                                   {:option :unleash :text "Unleash three times."}]
                                         :min     1
                                         :max     1}]]
            :quote       "'The Nameless hunger for the same thing we do; an end to this war.' ― Garu, Oathsworn Protector"})

(def smite {:name        :smite
            :type        :attack
            :tier        2
            :text        ["Unleash twice."
                          "Gravehold suffers 2 damage."]
            :immediately [[:unleash]
                          [:unleash]
                          [:damage-gravehold 2]]
            :quote       "'One side of this struggle must fall for the other to truly live.' Xaxos, Voidbringer"})

(defn throttle-destroy-cards [game {:keys [player-no]}]
  (let [sorted-hand          (->> (get-in game [:players player-no :hand])
                                  (sort-by :cost >)
                                  vec)
        third-highest-cost   (->> sorted-hand
                                  (take 3)
                                  last
                                  :cost)
        auto-destroy-cards   (cond
                               (<= (count sorted-hand) 3) sorted-hand
                               (not= third-highest-cost (get-in sorted-hand [3 :cost])) (take 3 sorted-hand)
                               :else (->> sorted-hand
                                          (filter (comp #(> % third-highest-cost) :cost))))
        manual-destroy-count (- (min 3 (count sorted-hand))
                                (count auto-destroy-cards))]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat
                                          (when (not-empty auto-destroy-cards)
                                            [[:move-cards {:card-names (map :name auto-destroy-cards)
                                                           :from       :hand
                                                           :to         :trash}]])
                                          (when (pos? manual-destroy-count)
                                            [[:give-choice {:title   :throttle
                                                            :text    (str "Destroy the "
                                                                          (when (> manual-destroy-count 1)
                                                                            (ut/number->text manual-destroy-count))
                                                                          " most expensive card"
                                                                          (when (> manual-destroy-count 1)
                                                                            "s")
                                                                          " in your hand.")
                                                            :choice  [:move-cards {:from :hand
                                                                                   :to   :trash}]
                                                            :options [:player :hand {:cost third-highest-cost}]
                                                            :min     manual-destroy-count
                                                            :max     manual-destroy-count}]]))})))

(effects/register {::throttle-destroy-cards throttle-destroy-cards})

(def throttle {:name        :throttle
               :type        :attack
               :tier        3
               :text        ["Unleash twice."
                             "Any player destroys their three most expensive cards in hand."]
               :immediately [[:unleash]
                             [:unleash]
                             [:give-choice {:title   :throttle
                                            :text    "Any player destroys their three most expensive cards in hand."
                                            :choice  ::throttle-destroy-cards
                                            :options [:players]
                                            :min     1
                                            :max     1}]]
               :quote       "'Were I made of muscle and blood like the others, the impact would have surely ended me.' ― Remnant, Aetherial Entity"})

(def basic-cards (concat [afflict
                          heart-of-nothing
                          howling-spinners
                          night-unending
                          nix
                          planar-collision]
                         [aphotic-sun
                          mangleroot
                          morbid-gyre
                          mutilate
                          null-scion
                          smite]
                         [apocalypse-ritual
                          monstrosity-of-omens
                          quell
                          throttle]
                         [apocalypse-ritual
                          apocalypse-ritual
                          quell
                          throttle]))
