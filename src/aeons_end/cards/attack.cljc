(ns aeons-end.cards.attack
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn afflict-damage-player [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:give-choice {:title   :afflict
                                                      :text    "You may place a card in your discard pile into your hand."
                                                      :choice  :take-from-discard
                                                      :options [:player :discard]
                                                      :max     1}]]}))

(effects/register {::afflict-damage-player afflict-damage-player})

(def afflict {:name    :afflict
              :type    :attack
              :tier    1
              :text    ["Unleash."
                        "Any player suffers 3 damage and may place a card in their discard pile into their hand."]
              :effects [[:unleash]
                        [:give-choice {:title   :afflict
                                       :text    "Any player suffers 3 damage and may place a card in their discard pile into their hand."
                                       :choice  ::afflict-damage-player
                                       :options [:players]
                                       :min     1
                                       :max     1}]]
              :quote   "'Such wisdom comes at a conciderable price.' Xaxos, Voidbringer"})

(defn banish-damage [{:keys [players] :as game} _]
  (let [most-prepped-spells (->> players
                                 (map ut/count-prepped-spells)
                                 (apply max))]
    (cond-> game
            (pos? most-prepped-spells) (push-effect-stack {:effects [[:give-choice {:title   :banish
                                                                                    :text    "The player with the most prepped spells suffers 1 damage for each of their prepped spells."
                                                                                    :choice  [:damage-player {:arg most-prepped-spells}]
                                                                                    :options [:players {:number-of-prepped-spells most-prepped-spells}]
                                                                                    :min     1
                                                                                    :max     1}]]}))))

(effects/register {::banish-damage banish-damage})

(def banish {:name    :banish
             :type    :attack
             :tier    3
             :text    ["Unleash twice."
                       "The player with the most prepped spells suffers 1 damage for each of their prepped spells."]
             :effects [[:unleash]
                       [:unleash]
                       [::banish-damage]]
             :quote   "'Get down! It's ejecting back at us!' Ohat, Dirt Merchant"})

(def mutilate {:name    :mutilate
               :type    :attack
               :tier    2
               :text    ["Unleash."
                         "The players collectively discard two prepped spells. Any player suffers 2 damage."]
               :effects [[:unleash]
                         [:give-choice {:title   :mutilate
                                        :text    "The players collectively discard two prepped spells."
                                        :choice  :collective-discard-prepped-spells
                                        :options [:players :prepped-spells]
                                        :min     2
                                        :max     2}]
                         [:give-choice {:title   :mutilate
                                        :text    "Any player suffers 2 damage."
                                        :choice  [:damage-player {:arg 2}]
                                        :options [:players]
                                        :min     1
                                        :max     1}]]
               :quote   "'They say that before The Nameless found Gravehold, nothing grew in these caves. It appears the cave burgeons through our blood.' Gex, Breach Mage Advisor"})

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

(def nix {:name    :nix
          :type    :attack
          :tier    1
          :text    ["Unleash."
                    "Any player suffers 1 damage and discards their most expensive card in hand."]
          :effects [[:unleash]
                    [:give-choice {:title   :nix
                                   :text    "Any player suffers 1 damage and discards their most expensive card in hand."
                                   :choice  ::nix-damage-player
                                   :options [:players]
                                   :min     1
                                   :max     1}]]
          :quote   "'It's as if the world itself is screaming.' Nerva, Survivor"})

(defn quell-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :damage [[:damage-gravehold 7]]
                                      :unleash [[:unleash]
                                                [:unleash]
                                                [:unleash]])}))

(effects/register {::quell-choice quell-choice})

(def quell {:name    :quell
            :type    :attack
            :tier    3
            :text    ["Gravehold suffers 7 damage."
                      "OR"
                      "Unleash three times."]
            :effects [[:give-choice {:title   :quell
                                     :choice  ::quell-choice
                                     :options [:special
                                               {:option :damage :text "Gravehold suffers 7 damage."}
                                               {:option :unleash :text "Unleash three times."}]
                                     :min     1
                                     :max     1}]]
            :quote   "'The Nameless hunger for the same thing we do; an end to this war.' Garu, Oathsworn Protector"})

(def smite {:name    :smite
            :type    :attack
            :tier    2
            :text    ["Unleash twice."
                      "Gravehold suffers 2 damage."]
            :effects [[:unleash]
                      [:unleash]
                      [:damage-gravehold 2]]
            :quote   "'One side of this struggle must fall for the other to truly live.' Xaxos, Voidbringer"})

(def thrash {:name    :thrash
             :type    :attack
             :tier    1
             :text    ["Unleash."
                       "The players collectively discard two cards in hand."]
             :effects [[:unleash]
                       [:give-choice {:title   :thrash
                                      :text    "The players collectively discard two cards in hand."
                                      :choice  :collective-discard-from-hand
                                      :options [:players :hand]
                                      :min     2
                                      :max     2}]]
             :quote   "'The creature thrashed as it burned, its massive body crashing into the South Wall like a hot blade through cloth.' Garu, Oathsworn Keeper"})

(defn throttle-destroy-cards [game {:keys [player-no]}]
  (let [sorted-hand          (->> (get-in game [:players player-no :hand])
                                  (sort-by :cost >))
        [_ _ cost-3 cost-4] (map :cost sorted-hand)
        auto-destroy-cards   (cond
                               (<= (count sorted-hand) 3) sorted-hand
                               (not= cost-3 cost-4) (take 3 sorted-hand)
                               :else (->> sorted-hand
                                          (filter (comp #(> % cost-3) :cost))))
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
                                                            :options [:player :hand {:min-cost cost-3}]
                                                            :min     manual-destroy-count
                                                            :max     manual-destroy-count}]]))})))

(effects/register {::throttle-destroy-cards throttle-destroy-cards})

(def throttle {:name    :throttle
               :type    :attack
               :tier    3
               :text    ["Unleash twice."
                         "Any player destroys their three most expensive cards in hand."]
               :effects [[:unleash]
                         [:unleash]
                         [:give-choice {:title   :throttle
                                        :text    "Any player destroys their three most expensive cards in hand."
                                        :choice  ::throttle-destroy-cards
                                        :options [:players]
                                        :min     1
                                        :max     1}]]
               :quote   "'Were I made of muscle and blood like the others, the impact would have surely ended me.' Remnant, Aetherial Entity"})
