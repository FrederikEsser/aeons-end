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
    (-> game
        (update-in [:players donator :aether] - 2)
        (push-effect-stack {:player-no player-no
                            :effects   [[:gain {:card-name   :volcanic-glass
                                                :to          :deck
                                                :to-position :top}]]}))))

(defn volcanic-glass-on-gain [{:keys [current-player] :as game} {:keys [player-no]}]
  (let [{:keys [aether]} (get-in game [:players player-no])
        {:keys [pile-size]} (ut/get-pile-idx game :volcanic-glass)]
    (cond-> game
            (and (= player-no current-player)
                 (>= aether 2)
                 (pos? pile-size)) (push-effect-stack {:player-no player-no
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
            bloodstone-jewel
            breach-ore
            burning-opal
            erratic-ingot
            haunted-berylite
            jade
            leeching-agate
            pain-stone
            searing-ruby
            sifters-pearl
            volcanic-glass
            vriswood-amber])
