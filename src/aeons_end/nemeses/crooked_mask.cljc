(ns aeons-end.nemeses.crooked-mask
  (:require [aeons-end.operations :refer [push-effect-stack add-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.power :as power]))

(defn gain-corruption [game {:keys [player-no to]}]
  (let [[card & new-corruption-deck] (get-in game [:nemesis :corruption-deck])
        to-position (if (= :deck to)
                      :top
                      :bottom)]
    (-> game
        (assoc-in [:nemesis :corruption-deck] (vec new-corruption-deck))
        (as-> game
              (if card
                (-> game
                    (add-card [:players player-no to] to-position card)
                    (cond-> (= :deck to) (update-in [:players player-no] dissoc :revealed-cards)))
                (push-effect-stack game {:effects [[:damage-gravehold 2]]}))))))

(defn gain-corruption-and-shuffle [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[::gain-corruption {:to :deck}]
                                       [:shuffle-discard-into-deck]]}))

(defn do-unleash [game args]
  (let [title (keyword (or (:resolving args)
                           (:resolving game))
                       "Unleash")]
    (push-effect-stack game {:effects [[:give-choice {:title   title
                                                      :text    "Any player gains a corruption and places it on top of their deck. That player shuffles their discard pile into their deck."
                                                      :choice  ::gain-corruption-and-shuffle
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::gain-corruption             gain-corruption
                   ::gain-corruption-and-shuffle gain-corruption-and-shuffle
                   ::unleash                     do-unleash})

(defn corruption-on-trash [{:keys [difficulty] :as game} {:keys [card-id]}]
  (let [{{:keys [name]} :card} (ut/get-card-idx game [:trash] {:id card-id})]
    (push-effect-stack game {:effects (if (#{:beginner :normal} difficulty)
                                        [[:move-card {:move-card-id card-id
                                                      :from         :trash
                                                      :to           :corruption-deck
                                                      :to-position  :bottom}]]
                                        [[:give-choice {:title   name
                                                        :text    (str "Place " (ut/format-name name) " into any player's discard pile.")
                                                        :choice  [:move-card {:move-card-id card-id
                                                                              :from         :trash
                                                                              :to           :discard}]
                                                        :options [:players]
                                                        :min     1
                                                        :max     1}]])})))

(effects/register {::corruption-on-trash corruption-on-trash})

(defn resolve-corruption [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat (when card
                                                  [[:move-card {:card-name card-name
                                                                :from      :hand
                                                                :to        :play-area}]
                                                   [:card-effect {:card card}]])
                                                [[:give-choice {:title   :corruption
                                                                :text    "Resolve all corruptions in your hand, in any order."
                                                                :choice  ::resolve-corruption
                                                                :options [:player :hand {:type :corruption}]
                                                                :min     1
                                                                :max     1}]])})))

(effects/register {::resolve-corruption resolve-corruption})

(defn additional-rules [{:keys [difficulty]}]
  [(if (#{:beginner :normal} difficulty)
     "- When a corruption is destroyed, place it on the bottom of the corruption deck."
     "- When a corruption is destroyed, place it into any player's discard pile.")
   "- When a player would gain a corruption and the corruption deck is empty, Gravehold suffers 2 damage instead."
   "- At the start of each player's turn, before that player's casting phase, that player resolves all corruptions in their hand, in any order."
   "- At the end of each player's main phase, that player resolves all corruptions they drew during their turn."
   "- Corruptions may not be played at any other time."])

(effects/register-predicates {::additional-rules additional-rules})

(defn setup [game _]
  (let [corruption-deck (->> (get-in game [:nemesis :corruption-deck])
                             shuffle
                             (mapv ut/give-id!))]
    (assoc-in game [:nemesis :corruption-deck] corruption-deck)))

(effects/register {::setup setup})

(def blind-abandon {:name     :blind-abandon
                    :type     :corruption
                    :cost     0
                    :text     ["Suffer 1 damage."
                               "You may destroy a non-corruption card in hand."
                               "Destroy this."]
                    :effects  [[:damage-player 1]
                               [:give-choice {:title   :blind-abandon
                                              :text    "You may destroy a non-corruption card in hand."
                                              :choice  :destroy-from-hand
                                              :options [:player :hand {:not-type :corruption}]
                                              :max     1}]
                               [:destroy-this]]
                    :on-trash [[::corruption-on-trash]]})

(def contagion {:name     :contagion
                :type     :corruption
                :cost     0
                :text     ["Suffer 1 damage."
                           "Return any card that costs 0 Aether in your discard pile to your hand."
                           "Destroy this."]
                :effects  [[:damage-player 1]
                           [:give-choice {:title   :contagion
                                          :text    "Return any card that costs 0 Aether in your discard pile to your hand."
                                          :choice  :take-from-discard
                                          :options [:player :discard {:cost 0}]
                                          :min     1
                                          :max     1}]
                           [:destroy-this]]
                :on-trash [[::corruption-on-trash]]})

(def delirium-veil {:name     :delirium-veil
                    :type     :corruption
                    :cost     0
                    :text     ["Gravehold suffers 2 damage."
                               "Focus a breach."
                               "Destroy this."]
                    :effects  [[:damage-gravehold 2]
                               [:give-choice {:title   :delirium-veil
                                              :text    "Focus a breach."
                                              :choice  :focus-breach
                                              :options [:player :breaches {:opened false}]
                                              :min     1
                                              :max     1}]
                               [:destroy-this]]
                    :on-trash [[::corruption-on-trash]]})

(def dire-wisdom {:name     :dire-wisdom
                  :type     :corruption
                  :cost     0
                  :text     ["Gain a spell from any spell supply pile."
                             "Gain three corruptions and place them on top of your deck."
                             "Destroy this."]
                  :effects  [[:give-choice {:title   :dire-wisdom
                                            :text    "Gain a spell from any spell supply pile."
                                            :choice  :gain
                                            :options [:supply {:type :spell}]
                                            :min     1
                                            :max     1}]
                             [::gain-corruption {:to :deck}]
                             [::gain-corruption {:to :deck}]
                             [::gain-corruption {:to :deck}]
                             [:destroy-this]]
                  :on-trash [[::corruption-on-trash]]})

(def endless-hunger {:name     :endless-hunger
                     :type     :corruption
                     :cost     0
                     :text     ["Gravehold suffers 3 damage."
                                "Gain 2 life."
                                "Destroy this."]
                     :effects  [[:damage-gravehold 3]
                                [:heal {:life 2}]
                                [:destroy-this]]
                     :on-trash [[::corruption-on-trash]]})

(def fever-of-war {:name     :fever-of-war
                   :type     :corruption
                   :cost     0
                   :text     ["Suffer 1 damage."
                              "Deal 2 damage."
                              "Destroy this."]
                   :effects  [[:damage-player 1]
                              [:deal-damage 2]
                              [:destroy-this]]
                   :on-trash [[::corruption-on-trash]]})

(def grim-sight {:name     :grim-sight
                 :type     :corruption
                 :cost     0
                 :text     ["Gravehold suffers 2 damage."
                            "Look at the top card of your deck. You may destroy it."
                            "Destroy this."]
                 :effects  [[:damage-gravehold 2]
                            [:reveal-from-deck 1]
                            [:give-choice {:title   :grim-sight
                                           :text    "Look at the top card of your deck. You may destroy it."
                                           :choice  :trash-from-revealed
                                           :options [:player :revealed]
                                           :max     1}]
                            [:topdeck-all-revealed]
                            [:destroy-this]]
                 :on-trash [[::corruption-on-trash]]})

(def insatiable-avarice {:name     :insatiable-avarice
                         :type     :corruption
                         :cost     0
                         :text     ["Suffer 2 damage."
                                    "Gain a gem from the least expensive gem supply pile and place it into your hand."
                                    "Destroy this."]
                         :effects  [[:damage-player 2]
                                    [:give-choice {:title   :insatiable-avarice
                                                   :text    "Gain a gem from the least expensive gem supply pile and place it into your hand."
                                                   :choice  [:gain {:to :hand}]
                                                   :options [:supply {:type :gem :least-expensive true}]
                                                   :min     1
                                                   :max     1}]
                                    [:destroy-this]]
                         :on-trash [[::corruption-on-trash]]})

(def lust-for-power {:name     :lust-for-power
                     :type     :corruption
                     :cost     0
                     :text     ["Suffer 1 damage."
                                "Gain 1 charge."
                                "Destroy this."]
                     :effects  [[:damage-player 1]
                                [:gain-charge]
                                [:destroy-this]]
                     :on-trash [[::corruption-on-trash]]})

(def nothingness {:name     :nothingness
                  :type     :corruption
                  :cost     0
                  :text     ["Discard two non-corruption cards. Suffer 2 damage."
                             "Shuffle any player's turn order card into the turn order deck."
                             "Destroy this."]
                  :effects  [[:give-choice {:title   :nothingness
                                            :text    "Discards two non-corruption cards."
                                            :choice  :discard-from-hand
                                            :options [:player :hand {:not-type :corruption}]
                                            :min     2
                                            :max     2}]
                             [:damage-player 2]
                             [:give-choice {:title   :nothingness
                                            :text    "Shuffle any player's turn order card into the turn order deck."
                                            :choice  :shuffle-into-turn-order-deck
                                            :options [:turn-order :discard {:not-type :nemesis}]
                                            :min     1
                                            :max     1}]
                             [:destroy-this]]
                  :on-trash [[::corruption-on-trash]]})

(defn reckless-might-gain [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-pile-idx game card-name)
        damage (-> (:cost card)
                   inc
                   (quot 2))]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:gain {:card-name   card-name
                                                 :to          :deck
                                                 :to-position :top}]
                                         [:damage-player damage]]})))

(effects/register {::reckless-might-gain reckless-might-gain})

(def reckless-might {:name     :reckless-might
                     :type     :corruption
                     :cost     0
                     :text     ["Gain a relic from any relic supply pile and place on top of your deck."
                                "Suffer damage equal to half its cost, rounded up."
                                "Destroy this."]
                     :effects  [[:give-choice {:title   :reckless-might
                                               :text    "Gain a relic from any relic supply pile and place on top of your deck.\nSuffer damage equal to half its cost, rounded up."
                                               :choice  ::reckless-might-gain
                                               :options [:supply {:type :relic}]
                                               :min     1
                                               :max     1}]
                                [:destroy-this]]
                     :on-trash [[::corruption-on-trash]]})

(def bedlam-sage {:name       :bedlam-sage
                  :type       :minion
                  :tier       3
                  :life       14
                  :persistent {:text    ["Gravehold gains 2 life."
                                         "The player with the most life suffers 3 damage."]
                               :effects [[:heal-gravehold 2]
                                         [:give-choice {:title   :bedlam-sage
                                                        :text    "The player with the most life suffers 3 damage."
                                                        :choice  [:damage-player {:arg 3}]
                                                        :options [:players {:most-life true}]
                                                        :min     1
                                                        :max     1}]]}
                  :quote      "'Employing a complex stratagem against such an unpredictable foe is beyond foolish. Instinct is our only and best weapon.' Mist, Breach Mage Dagger Captain"})

(defn afflict-corrupt [game {:keys [player-no player-card-names]}]
  (let [player-numbers (or (some->> player-card-names
                                    (map :player-no))
                           (when player-no
                             [player-no player-no]))]
    (->> player-numbers
         reverse
         (reduce (fn [game player-no]
                   (push-effect-stack game {:player-no player-no
                                            :effects   [[::gain-corruption {:to :deck}]
                                                        [::gain-corruption {:to :deck}]
                                                        [:heal {:life 1}]]}))
                 game))))

(effects/register {::afflict-corrupt afflict-corrupt})

(def afflict {:name    :afflict
              :type    :attack
              :tier    3
              :text    ["Two different players each gain two corruptions and place them on top of their decks."
                        "Each of those players gains 1 life."]
              :effects [[:give-choice {:title   :afflict
                                       :text    "Two different players each gain two corruptions and place them on top of their decks."
                                       :choice  ::afflict-corrupt
                                       :options [:players]
                                       :min     2
                                       :max     2}]]
              :quote   "'It was as if a whisper echoed behind my eyes, tempting me to stray into the raw abyss.' Xaxos, Breach Mage Adept"})

(defn burden-gain-corruption [game {:keys [player-card-names]}]
  (->> player-card-names
       (map :player-no)
       reverse
       (reduce (fn [game player-no]
                 (push-effect-stack game {:player-no player-no
                                          :effects   [[::gain-corruption {:to :deck}]]}))
               game)))

(effects/register {::burden-gain-corruption burden-gain-corruption})

(def burden {:name    :burden
             :type    :attack
             :tier    1
             :text    ["The players collectively gain two corruptions and place them on top of their decks."
                       "Any player focuses a breach."]
             :effects [[:give-choice {:title       :burden
                                      :text        "The players collectively gain two corruptions and place them on top of their decks."
                                      :choice      ::burden-gain-corruption
                                      :options     [:players]
                                      :min         2
                                      :max         2
                                      :repeatable? true}]
                       [:give-choice {:title   :burden
                                      :text    "Any player focuses a breach."
                                      :choice  :focus-breach
                                      :options [:players :breaches {:opened false}]
                                      :min     1
                                      :max     1}]]})

(def corrupter {:name       :corrupter
                :type       :minion
                :tier       1
                :life       6
                :text       "When this minion suffers damage, the player who dealt that damage gains a corruption and places it on top of their deck."
                :when-hit   [[::gain-corruption {:to :deck}]]
                :persistent {:text    "Gravehold suffers 1 damage."
                             :effects [[:damage-gravehold 1]]}
                :quote      "'The Corrupters are but a shadow of the Crooked Mask, bringing madness to all that have endured its pollution.'"})

(defn pain-sower-damage [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 2]
                                       [:give-choice {:title   :pain-sower
                                                      :text    "A different player focuses a breach."
                                                      :choice  :focus-breach
                                                      :options [:players :breaches {:ally true :opened false}]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::pain-sower-damage pain-sower-damage})

(def pain-sower {:name       :pain-sower
                 :type       :minion
                 :tier       2
                 :life       11
                 :persistent {:text    ["Any player suffers 2 damage."
                                        "A different player focuses a breach."]
                              :effects [[:give-choice {:title   :pain-sower
                                                       :text    "Any player suffers 2 damage."
                                                       :choice  ::pain-sower-damage
                                                       :options [:players]
                                                       :min     1
                                                       :max     1}]]}
                 :quote      "'Even its beasts seem to delight in lunacy.' Brama, Breach Mage Elder"})

(defn ruin-priest-discard [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :ruin-priest
                                                      :text    "Discards three non-corruption cards."
                                                      :choice  :discard-from-hand
                                                      :options [:player :hand {:not-type :corruption}]
                                                      :min     3
                                                      :max     3}]
                                       [::gain-corruption {:to :hand}]
                                       [::gain-corruption {:to :hand}]
                                       [::gain-corruption {:to :hand}]]}))

(defn ruin-priest-choice [{:keys [players] :as game} _]
  (let [max-cards (->> players
                       (map ut/count-non-corruption)
                       (apply max 0))]
    (push-effect-stack game {:effects [[:give-choice {:title   :ruin-priest
                                                      :text    "Any player discards three non-corruption cards. That player gains three corruptions and places them into their hand."
                                                      :choice  ::ruin-priest-discard
                                                      :options [:players {:min-non-corruption (min 3 max-cards)}]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::ruin-priest-discard ruin-priest-discard
                   ::ruin-priest-choice  ruin-priest-choice})

(def ruin-priest {:name       :ruin-priest
                  :type       :minion
                  :tier       3
                  :life       17
                  :persistent {:text    "Any player discards three non-corruption cards. That player gains three corruptions and places them into their hand."
                               :effects [[::ruin-priest-choice]]}
                  :quote      "'In the dark, there is a light. And in madness, there is reason.' Xaxos, Breach Mage Adept"})

(defn tempt-attack [game {:keys [player-no]}]
  (let [crystals (->> (get-in game [:players player-no :hand])
                      (map :name)
                      (filter #{:crystal}))]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:discard-from-hand {:card-names crystals}]
                                         [:damage-player 3]
                                         [:give-choice {:title   :tempt
                                                        :text    "Gain a card from any supply pile that costs 4 Aether or less."
                                                        :choice  :gain
                                                        :options [:supply {:max-cost 4}]
                                                        :min     1
                                                        :max     1}]]})))

(effects/register {::tempt-attack tempt-attack})

(def tempt {:name    :tempt
            :type    :attack
            :tier    1
            :text    "The player with the most Crystals in hand discards all of their Crystals, suffers 3 damage, and gains a card from any supply pile that costs 4 Aether or less."
            :effects [[:give-choice {:title   :tempt
                                     :text    "The player with the most Crystals in hand discards all of their Crystals."
                                     :choice  ::tempt-attack
                                     :options [:players {:most-crystals true}]
                                     :min     1
                                     :max     1}]]
            :quote   "'There is a dust that fills the air when a hole is torn through the wall of the world. For some it is just that, dust. But for others, it warps the mind and grants strange power.'"})

(defn twisting-madness-discard [game {:keys [player-no] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:discard-from-hand args]
                                       [:draw 1]]}))

(effects/register {::twisting-madness-discard twisting-madness-discard})

(def twisting-madness {:name       :twisting-madness
                       :type       :power
                       :tier       2
                       :to-discard {:text      "Discard four cards in hand and draw one card."
                                    :predicate [::power/cards-in-hand? {:amount 4}]
                                    :effects   [[:give-choice {:title   :twisting-madness
                                                               :text    "Discard four cards in hand and draw one card."
                                                               :choice  ::twisting-madness-discard
                                                               :options [:player :hand]
                                                               :min     4
                                                               :max     4}]]}
                       :power      {:power   2
                                    :text    ["Gravehold gains 3 life."
                                              "Crooked Mask gains 13 life."]
                                    :effects [[:heal-gravehold 3]
                                              [:heal-nemesis 13]]}
                       :quote      "'Is it laughing at us?' Nym, Breach Mage Apprentice"})

(defn vex-discard [game _]
  (let [damage (->> (get-in game [:nemesis :deck])
                    (take 2)
                    (filter (comp #{3} :tier))
                    count
                    (* 4))]
    (push-effect-stack game {:effects [[:move-cards {:number-of-cards 2
                                                     :from            :deck
                                                     :from-position   :top
                                                     :to              :discard}]
                                       [:damage-gravehold damage]]})))

(effects/register {::vex-discard vex-discard})

(def vex {:name    :vex
          :type    :attack
          :tier    2
          :text    ["Discard the top two cards of the nemesis deck."
                    "For each tier 3 nemesis card discarded this way, Gravehold suffers 4 damage."]
          :effects [[::vex-discard]]
          :quote   "'It is one thing to swing a sword or beckon flame from the void. But it is another act indeed to fight blinding madness from within.' Brama, Breach Mage Elder"})

(def crooked-mask {:name             :crooked-mask
                   :level            5
                   :life             70
                   :setup            [[::setup]]
                   :unleash          [[::unleash]]
                   :unleash-text     "Any player gains a corruption and places it on top of their deck. That player shuffles their discard pile into their deck."
                   :at-start-casting [[::resolve-corruption]]
                   :at-end-main      [[::resolve-corruption]]
                   :additional-rules ::additional-rules
                   :cards            [burden corrupter tempt
                                      pain-sower twisting-madness vex
                                      afflict bedlam-sage ruin-priest]
                   :corruption-deck  [blind-abandon
                                      contagion
                                      delirium-veil
                                      dire-wisdom
                                      endless-hunger
                                      fever-of-war
                                      grim-sight
                                      insatiable-avarice
                                      lust-for-power
                                      nothingness
                                      reckless-might]})
