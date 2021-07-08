(ns aeons-end.mages
  (:require [aeons-end.cards.starter :refer [crystal spark]]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn amethyst-shard-sift [game {:keys [player-no no-choice?]}]
  (cond-> game
          (and player-no
               (not no-choice?)) (push-effect-stack {:player-no player-no
                                                     :effects   [[:draw 1]
                                                                 [:give-choice {:title   :amethyst-shard
                                                                                :text    "Discard a card in hand."
                                                                                :choice  :discard-from-hand
                                                                                :options [:player :hand]
                                                                                :min     1
                                                                                :max     1}]]})))

(effects/register {::amethyst-shard-sift amethyst-shard-sift})

(def amethyst-shard {:name    :amethyst-shard
                     :type    :gem
                     :cost    0
                     :text    ["Gain 1 Aether."
                               "Any ally may draw a card and then discard a card in hand."]
                     :effects [[:gain-aether 1]
                               [:give-choice {:title   :amethyst-shard
                                              :text    "Any ally may draw a card and then discard a card in hand."
                                              :choice  ::amethyst-shard-sift
                                              :options [:players {:ally true}]
                                              :max     1}]]})

(defn aethereal-ward-discard [game _]
  (let [{:keys [name]} (-> game :nemesis :play-area last)]
    (push-effect-stack game {:effects [[:discard-nemesis-card {:card-name name}]]})))

(effects/register {::aethereal-ward-discard aethereal-ward-discard})

(def aethereal-ward {:name        :aethereal-ward
                     :activation  :nemesis-draw
                     :charge-cost 5
                     :text        ["When a nemesis attack or power card is drawn but before it is resolved, you may discard it. It has no effect."
                                   "(The nemesis does not draw a replacement card)"]
                     :effects     [[::aethereal-ward-discard]]})

(def adelheim {:name     :adelheim
               :title    "Breach Mage Weaponsmith"
               :breaches [{}
                          {:stage 2}
                          {:stage 0}
                          {:stage 1}]
               :hand     [amethyst-shard crystal crystal spark spark]
               :deck     [crystal crystal crystal crystal crystal]
               :ability  aethereal-ward})

(def buried-light {:name    :buried-light
                   :type    :spell
                   :cost    0
                   :cast    "Deal 1 damage. Gain 1 Aether."
                   :effects [[:deal-damage 1]
                             [:gain-aether 1]]})

(def brink-siphon {:name        :brink-siphon
                   :activation  :your-main-phase
                   :charge-cost 5
                   :text        "Any player gains 4 life."
                   :effects     [[:give-choice {:title   :brink-siphon
                                                :text    "Any player gains 4 life"
                                                :choice  [:heal {:life 4}]
                                                :options [:players {:not-exhausted true}]
                                                :min     1
                                                :max     1}]]})

(def brama {:name     :brama
            :title    "Breach Mage Elder"
            :breaches [{}
                       {:stage 1}
                       {:stage 0}
                       {:stage 2}]
            :hand     [buried-light crystal crystal crystal crystal]
            :deck     [crystal crystal crystal spark spark]
            :ability  brink-siphon})

(def oblivion-shard {:name    :oblivion-shard
                     :type    :gem
                     :cost    0
                     :text    "Gain 2 Aether that cannot be used to gain a relic or spell."
                     :effects [[:gain-aether {:arg 2 :restrict #{:relic :spell}}]]})

(defn tempest-sigil-replace-breach [game {:keys [player-no breach-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:destroy-breach {:breach-no             breach-no
                                                         :put-prepped-spells-in :hand}]
                                       [:gain-breach {:breach-no breach-no
                                                      :breach    {:status       :opened
                                                                  :type         :sigil
                                                                  :bonus-damage 2}}]
                                       [:give-choice {:title   :tempest-sigil
                                                      :text    "You may prep a spell from your hand to a breach."
                                                      :choice  [:prep-spell {:breach-no breach-no}]
                                                      :options [:player :hand {:type :spell}]
                                                      :max     1}]]}))

(effects/register {::tempest-sigil-replace-breach tempest-sigil-replace-breach})

(def tempest-sigil {:name        :tempest-sigil
                    :activation  :your-main-phase
                    :charge-cost 6
                    :text        ["Any player destroys an opened I or II breach and returns any spells prepped to that breach to their hand. That player gains a Sigil breach and places it where the destroyed breach was. Then, that player may prep a spell from their hand to a breach."]
                    :effects     [[:give-choice {:title   :tempest-sigil
                                                 :text    "Any player destroys an opened I or II breach and returns any spells prepped to that breach to their hand. That player gains a Sigil breach and places it where the destroyed breach was. Then, that player may prep a spell from their hand to a breach."
                                                 :choice  ::tempest-sigil-replace-breach
                                                 :options [:players :breaches {:opened        true
                                                                               :max-breach-no 1}]
                                                 :min     1
                                                 :max     1}]]})

(def dezmodia {:name     :dezmodia
               :title    "Voidborn Prodigy"
               :breaches [{}
                          {:stage 1}
                          {:stage 0}
                          {:stage 1}]
               :hand     [oblivion-shard crystal crystal spark spark]
               :deck     [crystal crystal crystal crystal spark]
               :ability  tempest-sigil})

(defn shattered-geode-take-card [game {:keys [player-no card-id to-player]}]
  (cond-> game
          card-id (push-effect-stack {:player-no player-no
                                      :effects   [[:move-card {:move-card-id card-id
                                                               :from         :discard
                                                               :to-player    to-player
                                                               :to           :hand}]]})))

(defn shattered-geode-choice [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :shattered-geode
                                                      :text    "You may place the top card of any ally's discard pile into your hand."
                                                      :choice  [::shattered-geode-take-card {:to-player player-no}]
                                                      :options [:players :discard {:ally true :last true}]
                                                      :max     1}]]}))

(effects/register {::shattered-geode-take-card shattered-geode-take-card
                   ::shattered-geode-choice    shattered-geode-choice})

(def shattered-geode {:name    :shattered-geode
                      :type    :gem
                      :cost    0
                      :text    ["Gain 1 Aether."
                                "You may place the top card of any ally's discard pile into your hand."]
                      :effects [[:gain-aether 1]
                                [::shattered-geode-choice]]})

(defn vimcraft-oath-aid-ally [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:draw 1]
                                       [:heal {:life 2}]]}))

(effects/register {::vimcraft-oath-aid-ally vimcraft-oath-aid-ally})

(def vimcraft-oath {:name        :vimcraft-oath
                    :activation  :your-main-phase
                    :charge-cost 5
                    :text        "Destroy up to two cards in your discard pile that cost 0 Aether.\nAny ally draws one card and gains 2 life."
                    :effects     [[:give-choice {:title   :vimcraft-oath
                                                 :text    "Destroy up to two cards in your discard pile that cost 0 Aether."
                                                 :choice  :destroy-from-discard
                                                 :options [:player :discard {:cost 0}]
                                                 :max     2}]
                                  [:give-choice {:title   :vimcraft-oath
                                                 :text    "Any ally draws one card and gains 2 life."
                                                 :choice  ::vimcraft-oath-aid-ally
                                                 :options [:players {:ally true}]
                                                 :min     1
                                                 :max     1}]]})

(def gex {:name     :gex
          :title    "Breach Mage Adviser"
          :breaches [{:status :destroyed}
                     {:status :opened}
                     {:stage 1}
                     {:stage 1}]
          :hand     [shattered-geode crystal crystal crystal spark]
          :deck     [crystal crystal crystal crystal spark]
          :ability  vimcraft-oath})

(defn quartz-shard-move-turn-order-card [game {:keys [choice]}]
  (cond-> game
          (= :bottom choice) (push-effect-stack {:effects [[:put-turn-order-top-to-bottom]]})))

(defn quartz-shard-choice [game {:keys [player-no]}]
  (let [{:keys [type]} (-> game :turn-order :deck first)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   (concat [[:give-choice {:title   :quartz-shard
                                                               :text    "You may place the revealed turn order card on the bottom or the top of the turn order deck."
                                                               :choice  ::quartz-shard-move-turn-order-card
                                                               :options [:special
                                                                         {:option :top :text "Top"}
                                                                         {:option :bottom :text "Bottom"}]
                                                               :min     1
                                                               :max     1}]]
                                               (when-not (= :nemesis type)
                                                 [[:gain-aether 1]]))}))))

(effects/register {::quartz-shard-move-turn-order-card quartz-shard-move-turn-order-card
                   ::quartz-shard-choice               quartz-shard-choice})

(def quartz-shard {:name    :quartz-shard
                   :type    :gem
                   :cost    0
                   :text    ["Gain 1 Aether.png."
                             "Reveal the top card of the turn order deck. You may place that card on the bottom or the top of the turn order deck. If you revealed a player turn order card, gain an additional 1 Aether."]
                   :effects [[:gain-aether 1]
                             [:reveal-top-turn-order]
                             [::quartz-shard-choice]]})

(defn quicken-thought-shuffle-into-turn-order-deck [game {:keys [card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:turn-order :discard] {:name card-name})
        player-no (get-in card [:type :player-no])]
    (cond-> game
            (int? player-no) (push-effect-stack {:player-no player-no
                                                 :effects   [[:shuffle-into-turn-order-deck {:card-name card-name}]
                                                             [:damage-player 1]]}))))

(effects/register {::quicken-thought-shuffle-into-turn-order-deck quicken-thought-shuffle-into-turn-order-deck})

(def quicken-thought {:name        :quicken-thought
                      :activation  :any-main-phase
                      :charge-cost 5
                      :text        ["Shuffle any player's turn order card into the turn order deck. That player suffers 1 damage."
                                    "(You may not choose the wildcard turn order card.)"]
                      :effects     [[:give-choice {:title   :quicken-thought
                                                   :text    "Shuffle any player's turn order card into the turn order deck. That player suffers 1 damage."
                                                   :choice  ::quicken-thought-shuffle-into-turn-order-deck
                                                   :options [:turn-order :discard {:player-non-wild true}]
                                                   :min     1
                                                   :max     1}]]})

(def lash {:name     :lash
           :title    "Beach Mage Scout"
           :breaches [{}
                      {:stage 2}
                      {:stage 0}
                      {:stage 2}]
           :hand     [quartz-shard crystal crystal crystal spark]
           :deck     [crystal crystal crystal spark spark]
           :ability  quicken-thought})

(def moonstone-shard {:name    :moonstone-shard
                      :type    :gem
                      :cost    0
                      :text    ["Gain 1 Aether."
                                "Gain an additional 1 Aether that can only be used to gain a gem."]
                      :effects [[:gain-aether 1]
                                [:gain-aether {:arg 1 :earmark #{:gem}}]]})

(defn black-mirror-cast [game {:keys [player-no] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:spell-effect args]
                                       [:cast-spell args]]}))

(defn black-mirror-choice [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :black-mirror
                                                      :text    "Cast any player's prepped spell without discarding it.\nThen cast that prepped spell again.\n(Discard it afterward)"
                                                      :choice  [::black-mirror-cast {:caster player-no}]
                                                      :options [:players :prepped-spells]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::black-mirror-cast   black-mirror-cast
                   ::black-mirror-choice black-mirror-choice})

(def black-mirror {:name        :black-mirror
                   :activation  :your-main-phase
                   :charge-cost 4
                   :text        "Cast any player's prepped spell without discarding it.\nThen cast that prepped spell again.\n(Discard it afterward)"
                   :effects     [[::black-mirror-choice]]})

(def jian {:name     :jian
           :title    "Breach Mage Orphan"
           :breaches [{}
                      {:status :opened}
                      {:stage 0}
                      {:stage 1}]
           :hand     [moonstone-shard crystal crystal spark spark]
           :deck     [crystal crystal crystal spark spark]
           :ability  black-mirror})

(defn garnet-shard-choice [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title     :garnet-shard
                                                      :text      "Cast any player's prepped spell."
                                                      :choice    [:cast-spell {:caster player-no}]
                                                      :or-choice {:text    "Gain 1 Aether"
                                                                  :effects [[:gain-aether 1]]}
                                                      :options   [:players :prepped-spells]
                                                      :max       1}]]}))

(effects/register {::garnet-shard-choice garnet-shard-choice})

(def garnet-shard {:name            :garnet-shard
                   :type            :gem
                   :cost            0
                   :auto-play-index 1
                   :text            "Gain 1 Aether. OR Cast any player's prepped spell."
                   :effects         [[::garnet-shard-choice]]})

(def divine-augury {:name        :divine-augury
                    :activation  :your-main-phase
                    :charge-cost 5
                    :text        "Any ally draws four cards."
                    :effects     [[:give-choice {:title   :divine-augury
                                                 :text    "Any ally draws four cards"
                                                 :choice  [:draw {:arg 4}]
                                                 :options [:players {:ally true}]
                                                 :min     1
                                                 :max     1}]]})

(def mist {:name     :mist
           :title    "Dagger Captain"
           :breaches [{}
                      {:stage 2}
                      {:stage 1}
                      {:stage 1}]
           :hand     [garnet-shard crystal crystal crystal spark]
           :deck     [crystal crystal crystal spark spark]
           :ability  divine-augury})

(defn quilius-gain-trophy [game _]
  (let [{:keys [idx]} (ut/get-card-idx game [:players] {:name :quilius})]
    (update-in game [:players idx :trophies] ut/plus 1)))

(effects/register {::quilius-gain-trophy quilius-gain-trophy})

(defn quietus-vow-damage [game {:keys [player-no]}]
  (let [{:keys [trophies]} (get-in game [:players player-no])]
    (cond-> game
            trophies (push-effect-stack {:player-no player-no
                                         :effects   [[:deal-damage (* 2 trophies)]]}))))

(effects/register {::quietus-vow-damage quietus-vow-damage})

(def quietus-vow {:name        :quietus-vow
                  :activation  :your-main-phase
                  :charge-cost 5
                  :text        "Deal 2 damage for each Trophy token you have."
                  :effects     [[::quietus-vow-damage]]})

(def extinguish {:name    :extinguish
                 :type    :spell
                 :cost    0
                 :cast    ["Deal 1 damage."
                           "If this damage causes a minion from the nemesis deck to be discarded, Quilius gains a Trophy token."]
                 :effects [[:deal-damage {:arg          1
                                          :kill-effects [[::quilius-gain-trophy]]}]]})

(def quilius {:name     :quilius
              :title    "Breach Mage Assassin"
              :breaches [{}
                         {:status :opened}
                         {:stage 0}
                         {:stage 1}]
              :hand     [crystal crystal crystal extinguish spark]
              :deck     [crystal crystal crystal extinguish spark]
              :ability  quietus-vow
              :trophies 0})

(def mages [adelheim
            brama
            dezmodia
            gex
            jian
            lash
            mist
            quilius])
