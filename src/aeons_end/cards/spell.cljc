(ns aeons-end.cards.spell
  (:require [aeons-end.nemesis]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut])
  (:refer-clojure :exclude [char]))

(defn amplify-vision-damage [game {:keys [player-no] :as args}]
  (let [all-breaches-opened? (->> (get-in game [:players player-no :breaches])
                                  (remove (comp #{:destroyed} :status))
                                  (every? (comp #{:opened} :status)))]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (if all-breaches-opened? 3 2)]]})))

(effects/register {::amplify-vision-damage amplify-vision-damage})

(def amplify-vision {:name    :amplify-vision
                     :type    :spell
                     :cost    4
                     :cast    ["Focus your closed breach with the lowest focus cost."
                               "Deal 2 damage. If all of your breaches are opened, deal 1 additional damage."]
                     :effects [[:focus-lowest-cost-breach]
                               [::amplify-vision-damage]]
                     :quote   "'The breaches are merely a mirror through which worlds whisper.' Phaedraxa, Breach Mage Seer"})

(defn arcane-nexus-can-use? [game {:keys [player-no]}]
  (->> (get-in game [:players player-no :play-area])
       (some (comp #{:gem} :type))))

(effects/register-predicates {::arcane-nexus-can-use? arcane-nexus-can-use?})

(def arcane-nexus {:name          :arcane-nexus
                   :type          :spell
                   :cost          7
                   :text          "While prepped, once per turn during your main phase you may return a gem you played this turn to your hand."
                   :cast          "Deal 4 damage."
                   :while-prepped {:phase    :main
                                   :once     true
                                   :can-use? ::arcane-nexus-can-use?
                                   :effects  [[:give-choice {:title   :arcane-nexus
                                                             :text    "Return a gem you played this turn to your hand."
                                                             :choice  [:move-card {:from :play-area
                                                                                   :to   :hand}]
                                                             :options [:player :play-area {:type :gem}]
                                                             :min     1
                                                             :max     1}]]}
                   :effects       [[:deal-damage 4]]
                   :quote         "'The taint of aether clings to everything in Gravehold. And we that survived near-extinction reek with it, too.' Nerva, Survivor"})

(defn aurora-can-use? [game {:keys [player-no]}]
  (let [{:keys [charges charge-cost]} (get-in game [:players player-no :ability])]
    (< charges charge-cost)))

(effects/register-predicates {::aurora-can-use? aurora-can-use?})

(def aurora {:name          :aurora
             :type          :spell
             :cost          5
             :text          "While prepped, once per turn during your main phase you may gain 1 charge."
             :cast          "Deal 3 damage."
             :while-prepped {:phase    :main
                             :once     true
                             :can-use? ::aurora-can-use?
                             :effects  [[:gain-charge]]}
             :effects       [[:deal-damage 3]]
             :quote         "'It is only natural that they question my being here. But let my answer be written in fire.' Yan Magda, Enlightened Exile"})

(defn blaze-move-card [game {:keys [from-player card-id player-no]}]
  (cond-> game
          card-id (push-effect-stack {:player-no from-player
                                      :effects   [[:move-card {:move-card-id card-id
                                                               :from         :gaining
                                                               :to-player    player-no
                                                               :to           :discard}]]})))

(defn blaze-on-gain [game {:keys [player-no gained-card-id]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :blaze
                                                      :text    "You may place the gained Blaze on top of any player's discard pile."
                                                      :choice  [::blaze-move-card {:card-id     gained-card-id
                                                                                   :from-player player-no}]
                                                      :options [:players]
                                                      :max     1}]]}))

(defn blaze-damage [game {:keys [player-no card-id] :as args}]
  (let [additional-damage (+ (->> (get-in game [:players player-no :this-turn])
                                  (filter (comp #{:blaze} :cast))
                                  count)
                             (->> (get-in game [:players player-no :breaches])
                                  (mapcat :prepped-spells)
                                  (filter (comp #{:blaze} :name))
                                  (remove (comp #{card-id} :id))
                                  count))]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (+ 2 additional-damage)]]})))

(effects/register {::blaze-move-card blaze-move-card
                   ::blaze-on-gain   blaze-on-gain
                   ::blaze-damage    blaze-damage})

(def blaze {:name    :blaze
            :type    :spell
            :cost    4
            :text    "When you gain this, you may place it on top of any player's discard pile."
            :cast    ["Deal 2 damage."
                      "Deal 1 additional damage for each other time you have cast Blaze this turn and for each other Blaze you currently have prepped."]
            :on-gain [[::blaze-on-gain]]
            :effects [[::blaze-damage]]})

(defn carbonize-choice [game {:keys [player-no]}]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   [[:give-choice {:title   :carbonize
                                                     :text    "You may place the revealed turn order card on the bottom of the turn order deck."
                                                     :choice  :put-turn-order-top-to-bottom
                                                     :options [:turn-order :revealed]
                                                     :max     1}]]})))

(effects/register {::carbonize-choice carbonize-choice})

(def carbonize {:name    :carbonize
                :type    :spell
                :cost    4
                :cast    ["Deal 3 damage."
                          "Reveal the top card of the turn order deck. You may place that card on the bottom of the turn order deck."]
                :effects [[:deal-damage 3]
                          [:reveal-top-turn-order]
                          [::carbonize-choice]]})

(defn catalyst-deal-damage [game {:keys [player-no] :as args}]
  (let [{:keys [life]} (get-in game [:players player-no])
        damage (if (<= life 2) 7 2)]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage damage]]})))

(effects/register {::catalyst-deal-damage catalyst-deal-damage})

(def catalyst {:name    :catalyst
               :type    :spell
               :cost    6
               :cast    ["Deal 2 damage."
                         "If you have 2 life or less, deal 5 additional damage."]
               :effects [[::catalyst-deal-damage]]
               :quote   "'What little quiet we have found is drowned out by the screams of the many we have lost.' Gex, Breach Mage Adviser"})

(defn celestial-spire-draw [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :celestial-spire)]
    (cond-> game
            (zero? pile-size) (push-effect-stack {:player-no player-no
                                                  :effects   [[:give-choice {:title   :celestial-spire
                                                                             :text    "Any ally draws a card."
                                                                             :choice  [:draw {:arg 1}]
                                                                             :options [:players {:ally true}]
                                                                             :min     1
                                                                             :max     1}]]}))))

(effects/register {::celestial-spire-draw celestial-spire-draw})

(def celestial-spire {:name    :celestial-spire
                      :type    :spell
                      :cost    5
                      :cast    ["Deal 3 damage."
                                "If this card's supply pile is empty, any ally draws a card."]
                      :effects [[:deal-damage 3]
                                [::celestial-spire-draw]]
                      :quote   "'The spires are beacons in the spaces in between.' Mist, Voidbringer"})

(defn chaos-arc-damage [game {:keys [player-no breach-player-no breach-no] :as args}]
  (let [adjacent-spells (->> (get-in game [:players breach-player-no :breaches])
                             (keep-indexed (fn [idx _]
                                             (when (or (= idx (dec breach-no))
                                                       (= idx (inc breach-no)))
                                               (ut/get-prepped-spells game {:player-no breach-player-no
                                                                            :breach-no idx}))))
                             (apply concat)
                             count)]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (+ 3 (* 2 adjacent-spells))]]})))

(effects/register {::chaos-arc-damage chaos-arc-damage})

(def chaos-arc {:name    :chaos-arc
                :type    :spell
                :cost    6
                :cast    ["Deal 3 damage."
                          "Deal 2 additional damage for each prepped spell in an adjacent breach."]
                :effects [[::chaos-arc-damage]]
                :quote   "'Even the blind can see what little is left of our world' Phaedraxa, Breach Mage Seer"})

(def char {:name    :char
           :type    :spell
           :cost    8
           :cast    ["Deal 6 damage."
                     "If this damage causes a minion from the nemesis deck to be discarded, any player gains 2 life."]
           :effects [[:deal-damage {:arg          6
                                    :kill-effects [[:give-choice {:title   :char
                                                                  :text    "Any player gains 2 life."
                                                                  :choice  [:heal {:life 2}]
                                                                  :options [:players {:can-heal true}]
                                                                  :min     1
                                                                  :max     1}]]}]]
           :quote   "'It is not killing. It is research.' Xaxos, Voidbringer"})

(defn conjure-the-lost-destroy [game {:keys [player-no area card-id] :as args}]
  (cond-> game
          area (push-effect-stack {:player-no player-no
                                   :effects   (concat
                                                (case area
                                                  :discard [[:destroy-from-discard {:card-id card-id}]]
                                                  :prepped-spells [[:destroy-prepped-spells args]])
                                                [[:heal-gravehold 4]])})))

(effects/register {::conjure-the-lost-destroy conjure-the-lost-destroy})

(def conjure-the-lost {:name    :conjure-the-lost
                       :type    :spell
                       :cost    6
                       :cast    ["Deal 5 damage."
                                 "You may destroy this. If you do, Gravehold gains 4 life."]
                       :effects [[:deal-damage 5]
                                 [:give-choice {:title   :conjure-the-lost
                                                :text    "You may destroy this. If you do, Gravehold gains 4 life."
                                                :choice  ::conjure-the-lost-destroy
                                                :options [:mixed
                                                          [:players :discard {:this true}]
                                                          [:players :prepped-spells {:this true}]]
                                                :max     1}]]
                       :quote   "'We would all gladly give our lives for Gravehold to live but another day.' Indira, Breach Apprentice"})

(defn consuming-void-destroy [game {:keys [player-no card-name card-names] :as args}]
  (let [card-count (cond card-name 1
                         card-names (count card-names)
                         :else 0)]
    (push-effect-stack game (merge {:player-no player-no
                                    :args      args         ; bonus-damage
                                    :effects   [[:destroy-from-hand args]
                                                [:deal-damage (* 3 card-count)]]}))))

(effects/register {::consuming-void-destroy consuming-void-destroy})

(def consuming-void {:name    :consuming-void
                     :type    :spell
                     :cost    7
                     :cast    ["Destroy up to two cards in hand."
                               "Deal 3 damage for each card destroyed this way."]
                     :effects [[:give-choice {:title   :consuming-void
                                              :text    "Destroy up to two cards in hand."
                                              :choice  ::consuming-void-destroy
                                              :options [:player :hand]
                                              :max     2}]]
                     :quote   "'The Far Hollow hovels still smolder from the last time a mage lost his wits and will to the void.' Ghan, Gem Scavenger"})

(defn convection-field-choice [{:keys [nemesis] :as game} {:keys [player-no bonus-damage]
                                                           :or   {bonus-damage 0}}]
  (let [{:keys [name]} nemesis
        big-damage   (+ 4 bonus-damage)
        small-damage (+ 2 bonus-damage)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:give-choice {:title     :convection-field
                                                        :text      (str "Deal " big-damage " damage to " (ut/format-name (or name :nemesis)) " or a Minion.")
                                                        :choice    [:deal-damage-to-target {:damage big-damage}]
                                                        :options   [:mixed
                                                                    [:nemesis]
                                                                    [:nemesis :minions]]
                                                        :or-choice {:text    (str "Deal " small-damage " damage.\nAny ally may destroy a card in hand.")
                                                                    :effects [[:deal-damage small-damage]
                                                                              [:give-choice {:title   :convection-field
                                                                                             :text    "Any ally may destroy a card in hand."
                                                                                             :choice  :destroy-from-hand
                                                                                             :options [:players :hand [:ally true]]
                                                                                             :max     1}]]}
                                                        :max       1}]]})))

(effects/register {::convection-field-choice convection-field-choice})

(def convection-field {:name    :convection-field
                       :type    :spell
                       :cost    5
                       :cast    ["Deal 4 damage."
                                 "OR"
                                 "Deal 2 damage."
                                 "Any ally may destroy a card in hand."]
                       :effects [[::convection-field-choice]]
                       :quote   "'Much to Ulgimor's dismay, no shadow would be cast without coward Ohat' Nerva, Survivor"})

(defn crystallize-reveal [game {:keys [player-no caster] :as args}]
  (let [number-of-gems (->> (get-in game [:players player-no :hand])
                            (filter (comp #{:gem} :type))
                            count)]
    (push-effect-stack game (merge {:player-no caster
                                    :args      args         ; bonus-damage
                                    :effects   [[:deal-damage (* 2 number-of-gems)]]}))))

(defn crystallize-give-choice [game {:keys [player-no] :as args}]
  (push-effect-stack game {:player-no player-no
                           :args      args                  ; bonus-damage
                           :effects   [[:give-choice {:title   :crystallize
                                                      :text    "Any ally reveals their hand. Deal 2 damage for each gem in that ally's hand."
                                                      :choice  [::crystallize-reveal {:caster player-no}]
                                                      :options [:players {:ally true}]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::crystallize-reveal      crystallize-reveal
                   ::crystallize-give-choice crystallize-give-choice})

(def crystallize {:name        :crystallize
                  :type        :spell
                  :dual-breach true
                  :cost        8
                  :text        "This spell must be prepped to two adjacent breaches. This fully occupies both breaches."
                  :cast        "Any ally reveals their hand. Deal 2 damage for each gem in that ally's hand."
                  :effects     [[::crystallize-give-choice]]
                  :quote       "'Born is dirt, buried in dirt.' Henge Mystic Gospel"})

(defn dark-fire-discard [game {:keys [player-no card-name card-names] :as args}]
  (let [card-count (cond card-name 1
                         card-names (count card-names)
                         :else 0)]
    (push-effect-stack game (merge {:player-no player-no
                                    :args      args         ; bonus-damage
                                    :effects   [[:discard-from-hand args]
                                                [:deal-damage (* 3 card-count)]]}))))

(effects/register {::dark-fire-discard dark-fire-discard})

(def dark-fire {:name    :dark-fire
                :type    :spell
                :cost    5
                :cast    ["Discard up to two cards in hand."
                          "Deal 3 damage for each card discarded this way."]
                :effects [[:give-choice {:title   :dark-fire
                                         :text    "Discard up to two cards in hand."
                                         :choice  ::dark-fire-discard
                                         :options [:player :hand]
                                         :max     2}]]
                :quote   "'Xaxos always says, to come back to light, we must first visit the shadow.' Malastar, Breach Mage Mentor"})

(defn essence-theft-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-choice {:title   :essence-theft
                                                                   :text    "Any player gains 1 life."
                                                                   :choice  [:heal {:life 1}]
                                                                   :options [:players {:can-heal true}]
                                                                   :min     1
                                                                   :max     1}]]})))

(effects/register {::essence-theft-discard essence-theft-discard})

(def essence-theft {:name    :essence-theft
                    :type    :spell
                    :cost    5
                    :cast    ["Deal 3 damage."
                              "You may discard a card in hand. If you do, any player gains 1 life."]
                    :effects [[:deal-damage 3]
                              [:give-choice {:title   :essence-theft
                                             :text    "You may discard a card in hand. If you do, any player gains 1 life."
                                             :choice  ::essence-theft-discard
                                             :options [:player :hand]
                                             :max     1}]]
                    :quote   "'There is talk of the link between the mages and our adversaries. More and more, the difference between the two becomes harder to see.' Ohat, Dirt Merchant"})

(defn feedback-aura-deal-damage [game {:keys [player-no] :as args}]
  (let [{:keys [charges]} (get-in game [:players player-no :ability])
        damage (if (>= charges 4) 6 3)]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage damage]]})))

(effects/register {::feedback-aura-deal-damage feedback-aura-deal-damage})

(def feedback-aura {:name    :feedback-aura
                    :type    :spell
                    :cost    5
                    :cast    ["Deal 3 damage."
                              "If you have 4 or more charges, deal 3 additional damage."]
                    :effects [[::feedback-aura-deal-damage]]
                    :quote   "'The void flows through me now; I am one with nothing.' Xaxos, Voidbringer"})

(def feral-lightning {:name                      :feral-lightning
                      :type                      :spell
                      :cost                      5
                      :text                      "This spell may be prepped to a closed breach without focusing it."
                      :cast                      "Deal 3 damage."
                      :may-prep-to-closed-breach true
                      :effects                   [[:deal-damage 3]]
                      :quote                     "'All things can be tamed.' Lash, Breach Mage Scout"})

(defn fiery-torrent-damage [{:keys [players] :as game} {:keys [player-no card-id] :as args}]
  (let [additional-damage (->> players
                               (mapcat :breaches)
                               (mapcat :prepped-spells)
                               (filter (comp #{:fiery-torrent} :name))
                               (remove (comp #{card-id} :id))
                               count
                               (* 2))]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (+ 2 additional-damage)]]})))

(effects/register {::fiery-torrent-damage fiery-torrent-damage})

(def fiery-torrent {:name    :fiery-torrent
                    :type    :spell
                    :cost    5
                    :cast    ["Deal 2 damage."
                              "Deal 2 additional damage for each other Fiery Torrent prepped by any player."]
                    :effects [[::fiery-torrent-damage]]
                    :quote   "'Fire feeds fire.' Dezmodia, Voidborn Prodigy"})

(def ignite {:name    :ignite
             :type    :spell
             :cost    4
             :cast    ["Deal 2 damage."
                       "Any ally gains 1 charge."]
             :effects [[:deal-damage 2]
                       [:give-choice {:title   :ignite
                                      :text    "Any ally gains 1 charge"
                                      :choice  :gain-charge
                                      :options [:players :ability {:ally true :fully-charged false}]
                                      :min     1
                                      :max     1}]]
             :quote   "'We are but one flickering flame against unimaginable dark.' Kadir, Breach Mage Delver"})

(defn jagged-lightning-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-choice {:title   :jagged-lightning
                                                                   :text    "Any player focuses their closed breach with the lowest focus cost."
                                                                   :choice  :focus-breach
                                                                   :options [:players :breaches {:lowest-focus-cost true}]
                                                                   :min     1
                                                                   :max     1}]]})))

(effects/register {::jagged-lightning-discard jagged-lightning-discard})

(def jagged-lightning {:name    :jagged-lightning
                       :type    :spell
                       :cost    4
                       :cast    ["Deal 3 damage."
                                 "You may discard a card in hand. If you do, any player focuses their closed breach with the lowest focus cost."]
                       :effects [[:deal-damage 3]
                                 [:give-choice {:title   :jagged-lightning
                                                :text    "You may discard a card in hand. If you do, any player focuses their closed breach with the lowest focus cost."
                                                :choice  ::jagged-lightning-discard
                                                :options [:player :hand]
                                                :max     1}]]
                       :quote   "'Let us hope it hurts.' Sparrow, Breach Mage Soldier"})

(defn kindle-can-use? [game {:keys [player-no breach-no]}]
  (and (->> (get-in game [:players player-no :breaches breach-no :prepped-spells])
            (not-any? (comp #{:spark} :name)))
       (->> (get-in game [:players player-no :hand])
            (some (comp #{:spark} :name)))))

(effects/register-predicates {::kindle-can-use? kindle-can-use?})

(defn kindle-prep-spark [game {:keys [player-no breach-no]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name :spark})]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:move-card {:card-name :spark
                                                      :from      :hand
                                                      :to        :breach
                                                      :breach-no breach-no}]
                                         [:on-prep-spell {:card card}]]})))

(effects/register {::kindle-prep-spark kindle-prep-spark})

(def kindle {:name          :kindle
             :type          :spell
             :cost          4
             :text          "While prepped, during your main phase you may also prep one Spark to the breach this spell is prepped to."
             :cast          "Deal 3 damage."
             :while-prepped {:phase    :main
                             :can-use? ::kindle-can-use?
                             :effects  [[::kindle-prep-spark]]}
             :effects       [[:deal-damage 3]]
             :quote         "'Let them burn along with us.' Garu, Oathsworn Protector"})

(def lava-tendril {:name          :lava-tendril
                   :type          :spell
                   :cost          4
                   :text          "While prepped, at the end of your casting phase deal 1 damage."
                   :cast          "Deal 3 damage."
                   :while-prepped {:at-end-casting [[:deal-damage 1]]}
                   :effects       [[:deal-damage 3]]
                   :quote         "'My turn!' Nym, Breach Apprentice"})

(defn nether-conduit-reveal [game {:keys [player-no card-name] :as args}]
  (let [{:keys [card pile-size]} (ut/get-pile-idx game card-name)
        cards-missing (when card
                        (- (if (= :gem (:type card)) 7 5)
                           pile-size))]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :args      args        ; bonus-damage
                                     :effects   (concat [[:deal-damage cards-missing]]
                                                        (when (pos? pile-size)
                                                          [[:give-choice {:title   :nether-conduit
                                                                          :text    (str "Any ally may gain a " (ut/format-name card-name) ".")
                                                                          :choice  [:gain {:card-name card-name}]
                                                                          :options [:players {:ally true}]
                                                                          :max     1}]]))}))))

(effects/register {::nether-conduit-reveal nether-conduit-reveal})

(def nether-conduit {:name    :nether-conduit
                     :type    :spell
                     :cost    7
                     :cast    "Reveal a card in hand that costs 2 Aether or more. If you do, deal damage equal to the number of cards missing in that card's supply pile. Then, any ally may gain a card from that supply pile."
                     :effects [[:give-choice {:title   :nether-conduit
                                              :text    "Reveal a card in hand that costs 2 Aether or more."
                                              :choice  ::nether-conduit-reveal
                                              :options [:player :hand {:min-cost 2}]
                                              :min     1
                                              :max     1}]]})

(def nova-forge {:name          :nova-forge
                 :type          :spell
                 :cost          6
                 :text          "While prepped, once per turn during your main phase you may gain 2 Aether that can only be used to gain a spell."
                 :cast          "Deal 4 damage."
                 :while-prepped {:phase   :main
                                 :once    true
                                 :effects [[:gain-aether {:arg 2 :earmark #{:spell}}]]}
                 :effects       [[:deal-damage 4]]
                 :quote         "'As I have said before, the breach is my anvil, Adelheim.' Gex, Breach Mage Adviser"})

(defn oblivion-swell-damage [game {:keys [player-no area damage card-name] :as args}]
  (let [{{:keys [cost]
          :or   {cost 0}} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (= :hand area)
                                          [[:discard-from-hand {:card-name card-name}]
                                           [:deal-damage (+ cost damage)]]
                                          [[:deal-damage-to-target args]])})))

(defn oblivion-swell-choice [{:keys [nemesis] :as game} {:keys [player-no bonus-damage]
                                                         :or   {bonus-damage 0}}]
  (let [{:keys [name]} nemesis
        damage (+ 2 bonus-damage)
        gems?  (->> (get-in game [:players player-no :hand])
                    (some (comp #{:gem} :type)))]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:give-choice {:title   :oblivion-swell
                                                        :text    (concat [(str "Deal " damage " damage to " (ut/format-name (or name :nemesis)) " or a Minion.")]
                                                                         (when gems?
                                                                           ["You may discard a gem. If you do, deal additional damage equal to its cost."]))
                                                        :choice  [::oblivion-swell-damage {:damage damage}]
                                                        :options [:mixed
                                                                  [:nemesis]
                                                                  [:nemesis :minions]
                                                                  [:player :hand {:type :gem}]]
                                                        :min     1
                                                        :max     1}]]})))

(effects/register {::oblivion-swell-damage oblivion-swell-damage
                   ::oblivion-swell-choice oblivion-swell-choice})

(def oblivion-swell {:name          :oblivion-swell
                     :type          :spell
                     :cost          5
                     :text          "While prepped, once per turn during your main phase you may gain 1 Aether."
                     :cast          ["Deal 2 damage."
                                     "You may discard a gem. If you do, deal additional damage equal to its cost."]
                     :while-prepped {:phase   :main
                                     :once    true
                                     :effects [[:gain-aether 1]]}
                     :effects       [[::oblivion-swell-choice]]
                     :quote         "'Many folk have made their fortune harvesting gems. The very breaches through which The Nameless come bestow the stones with strange properties.'"})

(defn phoenix-flame-damage [game {:keys [player-no area damage] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   (if (= :ability area)
                                        [[:spend-charges 1]
                                         [:deal-damage (+ 2 damage)]]
                                        [[:deal-damage-to-target args]])}))

(defn phoenix-flame-choice [{:keys [nemesis] :as game} {:keys [player-no bonus-damage]
                                                        :or   {bonus-damage 0}}]
  (let [{:keys [name]} nemesis
        damage  (+ 2 bonus-damage)
        charges (get-in game [:players player-no :ability :charges])]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:give-choice {:title   :phoenix-flame
                                                        :text    (concat [(str "Deal " damage " damage to " (ut/format-name (or name :nemesis)) " or a Minion.")]
                                                                         (when (and charges (pos? charges))
                                                                           ["OR"
                                                                            "Lose 1 charge to deal 2 additional damage."]))
                                                        :choice  [::phoenix-flame-damage {:damage damage}]
                                                        :options [:mixed
                                                                  [:nemesis]
                                                                  [:nemesis :minions]
                                                                  [:player :ability {:min-charges 1}]]
                                                        :min     1
                                                        :max     1}]]})))

(effects/register {::phoenix-flame-damage phoenix-flame-damage
                   ::phoenix-flame-choice phoenix-flame-choice})

(def phoenix-flame {:name    :phoenix-flame
                    :type    :spell
                    :cost    3
                    :cast    ["Deal 2 damage."
                              "You may lose 1 charge to deal 2 additional damage."]
                    :effects [[::phoenix-flame-choice]]
                    :quote   "'In The World That Was, there were great birds, born of fire and ash. They were thought to be immortal, and yet none now remain.' Malastar, Breach Mage Mentor"})

(defn planar-insight-damage [game {:keys [player-no] :as args}]
  (let [opened-breaches (->> (get-in game [:players player-no :breaches])
                             (filter (comp #{:opened} :status))
                             count)]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (+ 2 opened-breaches)]]})))

(effects/register {::planar-insight-damage planar-insight-damage})

(def planar-insight {:name    :planar-insight
                     :type    :spell
                     :cost    6
                     :cast    ["Deal 2 damage."
                               "Deal 1 additional damage for each of your opened breaches."]
                     :effects [[::planar-insight-damage]]
                     :quote   "'She may no longer speak, but she sees much.' Brama, Breach Mage Elder"})

(defn pyromancy-discard [game {:keys [caster card-name player-card-names] :as args}]
  (let [card-count (cond card-name 1
                         player-card-names (count player-card-names)
                         :else 0)]
    (push-effect-stack game (merge {:player-no caster
                                    :args      args         ; bonus-damage
                                    :effects   [[:collective-discard-from-hand args]
                                                [:deal-damage (+ 1 (* 3 card-count))]]}))))

(defn pyromancy-give-choice [game {:keys [player-no] :as args}]
  (push-effect-stack game {:player-no player-no
                           :args      args                  ; bonus-damage
                           :effects   [[:give-choice {:title   :pyromancy
                                                      :text    "Allies may collectively discard up to two cards in hand."
                                                      :choice  [::pyromancy-discard {:caster player-no}]
                                                      :options [:players :hand {:ally true}]
                                                      :max     2}]]}))

(effects/register {::pyromancy-discard     pyromancy-discard
                   ::pyromancy-give-choice pyromancy-give-choice})

(def pyromancy {:name    :pyromancy
                :type    :spell
                :cost    7
                :cast    ["Deal 1 damage."
                          "Allies may collectively discard up to two cards in hand. For each card discarded this way, deal 3 additional damage."]
                :effects [[::pyromancy-give-choice]]
                :quote   "'It is not killing. It is research.' Xaxos, Voidbringer"})

(def pyrotechnic-surge {:name        :pyrotechnic-surge
                        :type        :spell
                        :dual-breach true
                        :cost        4
                        :text        "This spell must be prepped to two adjacent breaches. This fully occupies both breaches."
                        :cast        ["Deal 4 damage."
                                      "You may destroy a card in your discard pile."]
                        :effects     [[:deal-damage 4]
                                      [:give-choice {:title   :pyrotechnic-surge
                                                     :text    "You may destroy a card in your discard pile."
                                                     :choice  :destroy-from-discard
                                                     :options [:player :discard]
                                                     :max     1}]]})

(def radiance {:name    :radiance
               :type    :spell
               :cost    8
               :cast    ["Deal 5 damage."
                         "Each ally draws a card."]
               :effects [[:deal-damage 5]
                         [:other-players {:effects [[:draw 1]]}]]
               :quote   "'All of us, together. For apart we are doomed.' Brama, Breach Mage Elder"})

(def reduce-to-ash {:name          :reduce-to-ash
                    :type          :spell
                    :cost          7
                    :text          "While prepped, at the start of your casting phase reveal the top card of your deck. You may destroy the revealed card."
                    :while-prepped {:at-start-casting [[:reveal-from-deck 1]
                                                       [:give-choice {:title   :reduce-to-ash
                                                                      :text    "Reveal the top card of your deck. You may destroy it."
                                                                      :choice  :trash-from-revealed
                                                                      :options [:player :revealed]
                                                                      :max     1}]
                                                       [:topdeck-all-revealed]]}
                    :cast          "Deal 4 damage."
                    :effects       [[:deal-damage 4]]
                    :quote         "'Whether the shadowkin beast known as Ulgimor resides in that lump of coal or it is some part of Ohat remains a topic of controversy.'"})

(def sages-brand {:name          :sage's-brand
                  :type          :spell
                  :dual-breach   true
                  :cost          7
                  :text          ["This spell must be prepped to two adjacent breaches. This fully occupies both breaches."
                                  "While prepped, draw an additional card during your draw phase."]
                  :while-prepped {:at-end-draw [[:draw 1]]}
                  :cast          "Deal 6 damage."
                  :effects       [[:deal-damage 6]]
                  :quote         "'The mark of the void lay beneath our skin, not upon it.' Xaxos, Breach Mage Adept"})

(def scorch {:name    :scorch
             :type    :spell
             :cost    5
             :cast    ["Deal 4 damage."
                       "If this damage causes a minion from the nemesis deck to be discarded, any ally gains 2 charges."]
             :effects [[:deal-damage {:arg          4
                                      :kill-effects [[:give-choice {:title   :scorch
                                                                    :text    "Any ally gains 2 charges."
                                                                    :choice  [:gain-charges {:arg 2}]
                                                                    :options [:players :ability {:ally true :fully-charged false}]
                                                                    :min     1
                                                                    :max     1}]]}]]
             :quote   "'With each beast she marks, Quilius thinks herself closer to enlightenment.' Yan Magda, Enlightened Exile"})

(def spectral-echo {:name    :spectral-echo
                    :type    :spell
                    :cost    3
                    :cast    ["Deal 2 damage."
                              "You may destroy a card in hand."]
                    :effects [[:deal-damage 2]
                              [:give-choice {:title   :spectral-echo
                                             :text    "You may destroy a card in hand."
                                             :choice  :destroy-from-hand
                                             :options [:player :hand]
                                             :max     1}]]
                    :quote   "'Clear your mind, child. Drink in the void.' Xaxos, Breach Mage Adept"})

(defn thoughtform-familiar-damage [game {:keys [player-no card-id] :as args}]
  (let [other-prepped-spells (->> (get-in game [:players player-no :breaches])
                                  (mapcat :prepped-spells)
                                  (remove (comp #{card-id} :id))
                                  count)]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (+ 2 other-prepped-spells)]]})))

(effects/register {::thoughtform-familiar-damage thoughtform-familiar-damage})

(def thoughtform-familiar {:name    :thoughtform-familiar
                           :type    :spell
                           :cost    3
                           :cast    ["Deal 2 damage."
                                     "Deal 1 additional damage for each of your other prepped spells."]
                           :effects [[::thoughtform-familiar-damage]]
                           :quote   "'We make nothing. We simply call it forth.' Yan Magda, Enlightened Exile"})

(defn wildfire-whip-can-use? [game {:keys [player-no]}]
  (let [{:keys [aether]} (get-in game [:players player-no])]
    (and aether
         (>= aether 2))))

(effects/register-predicates {::wildfire-whip-can-use? wildfire-whip-can-use?})

(defn wildfire-whip-cast [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :wildfire-whip
                                                      :text    "Cast any player's prepped spell."
                                                      :choice  [:cast-spell {:caster player-no}]
                                                      :options [:players :prepped-spells]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::wildfire-whip-cast wildfire-whip-cast})

(def wildfire-whip {:name          :wildfire-whip
                    :type          :spell
                    :cost          6
                    :text          "While prepped, during your main phase you may spend 2 Aether to cast any player's prepped spell."
                    :cast          "Deal 4 damage."
                    :while-prepped {:phase    :main
                                    :can-use? ::wildfire-whip-can-use?
                                    :effects  [[:pay {:amount 2
                                                      :type   :wildfire-whip}]
                                               [::wildfire-whip-cast]]}
                    :effects       [[:deal-damage 4]]
                    :quote         "'Sometimes a little motivation is needed for dire situations.' Z'hana, Breach Mage Renegade"})

(def cards [amplify-vision
            arcane-nexus
            aurora
            blaze
            carbonize
            catalyst
            celestial-spire
            chaos-arc
            char
            conjure-the-lost
            consuming-void
            convection-field
            crystallize
            dark-fire
            essence-theft
            feedback-aura
            feral-lightning
            fiery-torrent
            ignite
            jagged-lightning
            kindle
            lava-tendril
            nether-conduit
            nova-forge
            oblivion-swell
            phoenix-flame
            planar-insight
            pyromancy
            pyrotechnic-surge
            radiance
            reduce-to-ash
            sages-brand
            scorch
            spectral-echo
            thoughtform-familiar
            wildfire-whip])
