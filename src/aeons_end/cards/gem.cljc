(ns aeons-end.cards.gem
  (:require [aeons-end.cards.common :refer [gain-aether]]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn alien-element-gain-aether [game {:keys [player-no]}]
  (let [number-of-breaches           (->> (get-in game [:players player-no :breaches])
                                          count)
        breaches-with-prepped-spells (->> (range number-of-breaches)
                                          (keep (fn [breach-no]
                                                  (->> (ut/get-prepped-spells game {:player-no player-no
                                                                                    :breach-no breach-no})
                                                       not-empty)))
                                          count)]
    (gain-aether game {:player-no player-no
                       :arg       (inc breaches-with-prepped-spells)})))

(effects/register {::alien-element-gain-aether alien-element-gain-aether})

(def alien-element {:name    :alien-element
                    :type    :gem
                    :cost    4
                    :text    ["Gain 1 Aether."
                              "For each of your breaches with a spell prepped to it, gain an additional 1 Aether."]
                    :effects [[::alien-element-gain-aether]]
                    :quote   "'It matters not what it is, only what it can do' Garu, Oathsworn Protector"})

(defn banishing-topaz-topdeck [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:move-card {:card-name   card-name
                                                                 :from        :hand
                                                                 :to          :deck
                                                                 :to-position :top}]
                                                    [:gain-aether 2]]})))

(effects/register {::banishing-topaz-topdeck banishing-topaz-topdeck})

(def banishing-topaz {:name            :banishing-topaz
                      :type            :gem
                      :cost            5
                      :auto-play-index 1
                      :text            ["Gain 2 Aether."
                                        "You may place a card in hand on top of your deck. If you do, gain an additional 2 Aether."]
                      :effects         [[:gain-aether 2]
                                        [:give-choice {:title   :banishing-topaz
                                                       :text    "You may place a card in hand on top of your deck. If you do, gain an additional 2 Aether."
                                                       :choice  ::banishing-topaz-topdeck
                                                       :options [:player :hand]
                                                       :max     1}]]
                      :quote           "'They give. And then they take.' Mist, Dagger Captain"})

(defn bloodstone-jewel-on-gain [game {:keys [player-no]}]
  (let [this-turn (get-in game [:players player-no :this-turn])]
    (cond-> game
            (not (some #{{:gain :bloodstone-jewel}} this-turn)) (push-effect-stack {:player-no player-no
                                                                                    :effects   [[:gain-aether 3]]}))))

(effects/register {::bloodstone-jewel-on-gain bloodstone-jewel-on-gain})

(def bloodstone-jewel {:name    :bloodstone-jewel
                       :type    :gem
                       :cost    6
                       :text    ["When you gain a Bloodstone Jewel for the first time on you turn, gain 3 Aether."
                                 "Gain 3 Aether."]
                       :on-gain [[::bloodstone-jewel-on-gain]]
                       :effects [[:gain-aether 3]]
                       :quote   "'This world bleeds, like all living things.' Mazahaedron, Henge Mystic"})

(def breach-ore {:name            :breach-ore
                 :type            :gem
                 :cost            4
                 :auto-play-index 1
                 :text            ["Focus your closed breach with the lowest focus cost."
                                   "OR"
                                   "Gain 2 Aether."]
                 :effects         [[:give-choice {:title     :breach-ore
                                                  :text      "Focus your closed breach with the lowest focus cost."
                                                  :choice    :focus-lowest-cost-breach
                                                  :options   [:player :breaches {:lowest-focus-cost true}]
                                                  :or-choice {:text    "Gain 2 Aether"
                                                              :effects [[:gain-aether 2]]}
                                                  :max       1}]]
                 :quote           "'If you want to make those wretched things scream, try this.' Sparrow, Breach Mage Soldier"})

(defn burning-opal-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-choice {:title   :burning-opal
                                                                   :text    "Any ally draws a card."
                                                                   :choice  [:draw {:arg 1}]
                                                                   :options [:players {:ally true}]
                                                                   :min     1
                                                                   :max     1}]]})))

(effects/register {::burning-opal-discard burning-opal-discard})

(def burning-opal {:name            :burning-opal
                   :type            :gem
                   :cost            5
                   :auto-play-index -1
                   :text            ["Gain 3 Aether."
                                     "You may discard a card in hand. If you do, any ally draws a card."]
                   :effects         [[:gain-aether 3]
                                     [:give-choice {:title   :burning-opal
                                                    :text    "You may discard a card in hand. If you do, any ally draws a card."
                                                    :choice  ::burning-opal-discard
                                                    :options [:player :hand]
                                                    :max     1}]]
                   :quote           "'Careful, youngling. You'll be wanting tongs to handle that!' Adelheim, Breach Mage Weaponsmith"})

(defn clouded-sapphire-gain-charge [game {:keys [player-no]}]
  (let [clouded-sapphires-played (->> (get-in game [:players player-no :this-turn])
                                      (filter (comp #{:clouded-sapphire} :play))
                                      count)]
    (cond-> game
            (= 1 clouded-sapphires-played) (push-effect-stack {:player-no player-no
                                                               :effects   [[:give-choice {:title   :clouded-sapphire
                                                                                          :text    "Any ally gains 1 charge"
                                                                                          :choice  :gain-charge
                                                                                          :options [:players :ability {:ally true :fully-charged false}]
                                                                                          :min     1
                                                                                          :max     1}]]}))))

(effects/register {::clouded-sapphire-gain-charge clouded-sapphire-gain-charge})

(def clouded-sapphire {:name    :clouded-sapphire
                       :type    :gem
                       :cost    6
                       :text    ["Gain 3 Aether."
                                 "If this is the first time you have played Clouded Sapphire this turn, any ally gains 1 charge."]
                       :effects [[:gain-aether 3]
                                 [::clouded-sapphire-gain-charge]]
                       :quote   "'They say if you look closely, the shadow of The World That Was is imprisoned within.' Phaedraxa, Breach Mage Seer"})

(defn diamond-cluster-gain-aether [game {:keys [player-no]}]
  (let [diamond-clusters-played (->> (get-in game [:players player-no :this-turn])
                                     (filter (comp #{:diamond-cluster} :play))
                                     count)]
    (cond-> game
            (= 2 diamond-clusters-played) (push-effect-stack {:player-no player-no
                                                              :effects   [[:gain-aether 2]]}))))

(effects/register {::diamond-cluster-gain-aether diamond-cluster-gain-aether})

(def diamond-cluster {:name    :diamond-cluster
                      :type    :gem
                      :cost    4
                      :text    ["Gain 2 Aether."
                                "If this is the second time you have played Diamond Cluster this turn, gain an additional 2 Aether."]
                      :effects [[:gain-aether 2]
                                [::diamond-cluster-gain-aether]]
                      :quote   "'These gems were once the most precious, the most scarce. Now they line the walls of caves like shimmering fangs.'"})

(defn dread-diamond-discard [game {:keys [player-no breach-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-prepped-spells {:breach-no breach-no
                                                                              :card-name card-name}]
                                                    [:gain-aether 1]]})))

(effects/register {::dread-diamond-discard dread-diamond-discard})

(def dread-diamond {:name            :dread-diamond
                    :type            :gem
                    :cost            3
                    :auto-play-index 1
                    :text            ["Gain 2 Aether."
                                      "You may discard a prepped spell. If you do, gain an additional 1 Aether."]
                    :effects         [[:gain-aether 2]
                                      [:give-choice {:title   :dread-diamond
                                                     :text    "You may discard a prepped spell. If you do, gain an additional 1 Aether."
                                                     :choice  ::dread-diamond-discard
                                                     :options [:player :prepped-spells]
                                                     :max     1}]]
                    :quote           "'There is a cruelty within this stone, a hunger veiled as beauty' Xaxos, Voidbringer"})

(defn erratic-ingot-gain-aether [game {:keys [player-no]}]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:gain-aether (if (pos? discarded-nemesis-cards) 4 2)]]})))

(effects/register {::erratic-ingot-gain-aether erratic-ingot-gain-aether})

(def erratic-ingot {:name    :erratic-ingot
                    :type    :gem
                    :cost    5
                    :text    ["Gain 2 Aether."
                              "Gain an additional 2 Aether if there is at least one nemesis turn order card in the turn order discard pile."]
                    :effects [[::erratic-ingot-gain-aether]]
                    :quote   "'Hit it all you like, Adelheim. It has a mind of its own.' Gex, Breach Mage Adviser"})

(def frozen-magmite {:name    :frozen-magmite
                     :type    :gem
                     :cost    3
                     :text    ["Gain 2 Aether."
                               "You may place the next card you gain this turn on top of your deck."]
                     :effects [[:gain-aether 2]
                               [:add-trigger {:trigger {:event    :on-gain
                                                        :duration :once-turn
                                                        :effects  [[:give-choice {:title   :frozen-magmite
                                                                                  :text    "You may place the card you gained on top of your deck."
                                                                                  :choice  :topdeck-from-gained
                                                                                  :options [:player :gaining]
                                                                                  :max     1}]]}}]]
                     :quote   "'Fear not returning to the dust, younglings. Even fire must sleep.' Mazahaedron, Henge Mystic"})

(defn haunted-berylite-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:gain-charges 2]]})))

(effects/register {::haunted-berylite-discard haunted-berylite-discard})

(def haunted-berylite {:name            :haunted-berylite
                       :type            :gem
                       :cost            3
                       :auto-play-index -1
                       :text            ["Discard a card in hand. If you do, gain 2 charges."
                                         "OR"
                                         "Gain 2 Aether."]
                       :effects         [[:give-choice {:title     :haunted-berylite
                                                        :text      "Discard a card in hand. If you do, gain 2 charges."
                                                        :choice    ::haunted-berylite-discard
                                                        :options   [:player :hand]
                                                        :or-choice {:text    "Gain 2 Aether"
                                                                    :effects [[:gain-aether 2]]}
                                                        :max       1}]]
                       :quote           "'All things end. And each leaves behind a shadow, a skull, a memory.' Yan Magda, Enlightened Exile"})

(def jade {:name    :jade
           :type    :gem
           :cost    2
           :text    "Gain 2 Aether."
           :effects [[:gain-aether 2]]
           :quote   "'Jade is as common as bone at the mouth of the cave.'"})

(def leeching-agate {:name    :leeching-agate
                     :type    :gem
                     :cost    3
                     :text    ["When you gain this, gain 1 charge."
                               "Gain 2 Aether."]
                     :on-gain [[:gain-charge]]
                     :effects [[:gain-aether 2]]
                     :quote   "'These stones are among the most powerful in the labyrinth of lost caves. They are also the most corrupted.' Kadir, Breach Mage Delver"})

(defn pain-stone-choices [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :aether [[:gain-aether 3]]
                                        :damage [[:gain-aether 2]
                                                 [:deal-damage 1]])}))

(effects/register {::pain-stone-choices pain-stone-choices})

(def pain-stone {:name            :pain-stone
                 :type            :gem
                 :cost            6
                 :auto-play-index 1
                 :text            ["Gain 3 Aether."
                                   "OR"
                                   "Gain 2 Aether and deal 1 damage."]
                 :effects         [[:give-choice {:title   :pain-stone
                                                  :choice  ::pain-stone-choices
                                                  :options [:special
                                                            {:option :aether :text "Gain 3 Aether"}
                                                            {:option :damage :text "Gain 2 Aether and deal 1 damage"}]
                                                  :min     1
                                                  :max     1}]]
                 :quote           "'Pain is the true and only tongue of The Nameless.' Garu, Oathsworn Protector"})

(defn scoria-slag-gain-aether [game {:keys [player-no]}]
  (let [tier (ut/get-nemesis-tier game)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:gain-aether (if (>= tier 2) 3 2)]]})))

(effects/register {::scoria-slag-gain-aether scoria-slag-gain-aether})

(def scoria-slag {:name    :scoria-slag
                  :type    :gem
                  :cost    4
                  :text    ["Gain 2 Aether"
                            "If the nemesis tier is 2 or higher, gain an additional 1 Aether."]
                  :effects [[::scoria-slag-gain-aether]]
                  :quote   "'The cave shudders when the breaches open. And when they close, the rocks are but glowing' Nerva, Survivor"})

(def searing-ruby {:name    :searing-ruby
                   :type    :gem
                   :cost    4
                   :text    ["Gain 2 Aether"
                             "Gain an additional 1 Aether that can only be used to gain a spell."]
                   :effects [[:gain-aether 2]
                             [:gain-aether {:arg 1 :earmark #{:spell}}]]
                   :quote   "'Some use these to heat and light their hovels against the dark of the cave. Others know that within these gems lies a fire that can be tamed.' Ghan, Gem Scavenger"})

(def sifters-pearl {:name    :sifter's-pearl
                    :type    :gem
                    :cost    3
                    :text    ["Gain 2 Aether."
                              "Each player reveals the top card of their deck and either discards it or returns it to the top of their deck."]
                    :effects [[:gain-aether 2]
                              [:all-players {:effects [[:reveal-from-deck 1]
                                                       [:give-choice {:title   :sifter's-pearl
                                                                      :text    "You may discard the top card of your deck."
                                                                      :choice  :discard-from-revealed
                                                                      :options [:player :revealed]
                                                                      :max     1}]
                                                       [:topdeck-all-revealed]]}]]
                    :quote   "'To those who have stepped through the breach, the pearl shows the splinters of time itself.' Phaedraxa, Breach Mage Seer"})

(defn volcanic-glass-donate [game {:keys [player-no donator no-choice?]}]
  (if no-choice?
    game
    (let [last-devoured (->> (get-in game [:nemesis :devoured])
                             last
                             :name)]
      (-> game
          (update-in [:players donator :aether] - 2)
          (push-effect-stack {:player-no player-no
                              :effects   [[:gain {:card-name   (if (= :volcanic-glass last-devoured)
                                                                 :devoured
                                                                 :volcanic-glass)
                                                  :to          :deck
                                                  :to-position :top}]]})))))

(defn volcanic-glass-on-gain [{:keys [current-player] :as game} {:keys [player-no]}]
  (let [{:keys [aether]} (get-in game [:players player-no])
        {:keys [pile-size]} (ut/get-pile-idx game :volcanic-glass)
        last-devoured (->> (get-in game [:nemesis :devoured])
                           last
                           :name)]
    (cond-> game
            (and (= player-no current-player)
                 (>= aether 2)
                 (or (pos? pile-size)
                     (= :volcanic-glass last-devoured))) (push-effect-stack {:player-no player-no
                                                                             :effects   [[:give-choice {:title   :volcanic-glass
                                                                                                        :text    "Any ally may gain a Volcanic Glass on top of their deck, if you spend 2 Aether."
                                                                                                        :choice  [::volcanic-glass-donate {:donator player-no}]
                                                                                                        :options [:players {:ally true}]
                                                                                                        :max     1}]]}))))

(effects/register {::volcanic-glass-donate  volcanic-glass-donate
                   ::volcanic-glass-on-gain volcanic-glass-on-gain})

(def volcanic-glass {:name    :volcanic-glass
                     :type    :gem
                     :cost    3
                     :text    ["When you gain this on your turn, you may spend 2 Aether. If you do, any ally also gains a Volcanic Glass and places it on top of their deck."
                               "Gain 2 Aether."]
                     :on-gain [[::volcanic-glass-on-gain]]
                     :effects [[:gain-aether 2]]
                     :quote   "'Long has this brittle slag been called \"god's mirror.\"' Mazahaedron, Henge Mystic"})

(def vriswood-amber {:name    :v'riswood-amber
                     :type    :gem
                     :cost    3
                     :text    ["When you gain this, you may place it on top of your deck."
                               "Gain 2 Aether."]
                     :on-gain [[:give-choice {:title   :v'riswood-amber
                                              :text    "You may place the gained V'riswood Amber on top of your deck."
                                              :choice  :topdeck-from-gained
                                              :options [:player :gaining]
                                              :max     1}]]
                     :effects [[:gain-aether 2]]
                     :quote   "'Once, the trees of V'riswood towered ancient and beautiful. These polished bits of their amber is all that remains beyond that memory.' Brama, Breach Mage Elder"})

(def cards [alien-element
            banishing-topaz
            bloodstone-jewel
            breach-ore
            burning-opal
            clouded-sapphire
            diamond-cluster
            dread-diamond
            erratic-ingot
            frozen-magmite
            haunted-berylite
            jade
            leeching-agate
            pain-stone
            scoria-slag
            searing-ruby
            sifters-pearl
            volcanic-glass
            vriswood-amber])
