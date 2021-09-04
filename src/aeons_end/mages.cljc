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
                     :text        "When a nemesis attack or power card is drawn but before it is resolved, you may discard it. It has no effect."
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
                                                :text    "Any player gains 4 life."
                                                :choice  [:heal {:life 4}]
                                                :options [:players {:can-heal true}]
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
                                                      :choice  [:prep-spell]
                                                      :options [:player :hand {:type     :spell
                                                                               :can-prep {}}]
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

(def twin-opal {:name    :twin-opal
                :type    :gem
                :cost    0
                :text    ["Gain 1 Aether."
                          "You may cast a spell in hand."]
                :effects [[:gain-aether 1]
                          [:give-choice {:title   :twin-opal
                                         :text    "You may cast a spell in hand."
                                         :choice  :cast-spell-in-hand
                                         :options [:player :hand {:type :spell}]
                                         :max     1}]]})

(defn pyromancers-guile-cast [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:cast-spell-in-hand {:card-name         card-name
                                                                          :additional-damage 1}]
                                                    [:give-choice {:title   :pyromancer's-guile
                                                                   :text    "Cast any number of spells in hand. Those spells each deal 1 additional damage."
                                                                   :choice  ::pyromancer's-guile-cast
                                                                   :options [:player :hand {:type :spell}]
                                                                   :max     1}]]})))

(effects/register {::pyromancer's-guile-cast pyromancers-guile-cast})

(def pyromancers-guile {:name        :pyromancer's-guile
                        :activation  :your-main-phase
                        :charge-cost 4
                        :text        ["Cast any number of spells in hand. Those spells each deal 1 additional damage."
                                      "You may destroy a card in your discard pile."]
                        :effects     [[:give-choice {:title   :pyromancer's-guile
                                                     :text    "Cast any number of spells in hand. Those spells each deal 1 additional damage."
                                                     :choice  ::pyromancer's-guile-cast
                                                     :options [:player :hand {:type :spell}]
                                                     :max     1}]
                                      [:give-choice {:title   :pyromancer's-guile
                                                     :text    "You may destroy a card in your discard pile."
                                                     :choice  :destroy-from-discard
                                                     :options [:player :discard]
                                                     :max     1}]]})

(def indira {:name     :indira
             :title    "Breach Apprentice"
             :breaches [{:status :destroyed}
                        {:status :destroyed}
                        {:stage 1}
                        {:status :destroyed}]
             :hand     [crystal crystal crystal twin-opal spark]
             :deck     [crystal crystal crystal twin-opal spark]
             :ability  pyromancers-guile})

(def emerald-shard {:name            :emerald-shard
                    :type            :gem
                    :cost            0
                    :auto-play-index 1
                    :text            ["Any player gains 1 life."
                                      "OR"
                                      "Gain 1 Aether."]
                    :effects         [[:give-choice {:title     :emerald-shard
                                                     :text      "Any player gains 1 life."
                                                     :choice    [:heal {:life 1}]
                                                     :options   [:players {:can-heal true}]
                                                     :or-choice {:text    "Gain 1 Aether"
                                                                 :effects [[:gain-aether 1]]}
                                                     :max       1}]]})

(defn otherworldly-gate-effects [{:keys [current-player] :as game} _]
  (-> game
      (assoc-in [:players current-player :breach-capacity] 2)
      (push-effect-stack {:player-no current-player
                          :effects   [[:give-choice {:title   :otherworldly-gate
                                                     :text    "You may return up to three spells in your discard pile to your hand."
                                                     :choice  :take-from-discard
                                                     :options [:player :discard {:type :spell}]
                                                     :max     3}]]})))

(effects/register {::otherworldly-gate-effects otherworldly-gate-effects})

(def otherworldly-gate {:name        :otherworldly-gate
                        :activation  :any-main-phase
                        :charge-cost 5
                        :text        "That player may return up to three spells in their discard pile to their hand. That player may prep up to two spells to each of their opened breaches this turn."
                        :effects     [[::otherworldly-gate-effects]]})

(def kadir {:name     :kadir
            :title    "Breach Mage Delver"
            :breaches [{}
                       {:stage 2}
                       {:stage 1}
                       {:stage 2}]
            :hand     [emerald-shard crystal crystal crystal spark]
            :deck     [crystal crystal crystal spark spark]
            :ability  otherworldly-gate})

(defn quartz-shard-choice [game {:keys [player-no]}]
  (let [{:keys [type]} (-> game :turn-order :deck first)]
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   (concat [[:give-choice {:title   :quartz-shard
                                                               :text    "You may place the revealed turn order card on the bottom of the turn order deck."
                                                               :choice  :put-turn-order-top-to-bottom
                                                               :options [:turn-order :revealed]
                                                               :max     1}]]
                                               (when-not (= :nemesis type)
                                                 [[:gain-aether 1]]))}))))

(effects/register {::quartz-shard-choice quartz-shard-choice})

(def quartz-shard {:name    :quartz-shard
                   :type    :gem
                   :cost    0
                   :text    ["Gain 1 Aether."
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

(def immolate {:name          :immolate
               :type          :spell
               :cost          0
               :text          "While prepped, when you suffer damage, gain 1 charge."
               :while-prepped {:at-suffer-damage [[:gain-charge]]}
               :cast          "Deal 1 damage."
               :effects       [[:deal-damage 1]]})

(defn gift-of-aether-prep [game {:keys [from-player player-no breach-no card-id]}]
  (cond-> game
          breach-no (push-effect-stack {:player-no player-no
                                        :effects   [[:move-card {:move-card-id card-id
                                                                 :from-player  from-player
                                                                 :from         :discard
                                                                 :to           :breach
                                                                 :breach-no    breach-no}]]})))

(defn gift-of-aether-on-gain [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :discard] {:id card-id})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:give-choice {:title   :gift-of-aether
                                                                :text    (str "You may prep the gained " (ut/format-name (:name card)) " to any player's opened breach.")
                                                                :choice  [::gift-of-aether-prep {:card-id     card-id
                                                                                                 :from-player player-no}]
                                                                :options [:players :breaches {:can-prep {:card             card
                                                                                                         :opened-breaches? true}}]
                                                                :max     1}]]}))))

(defn gift-of-aether-gain [game {:keys [player-no card-name]}]
  (let [card-id (ut/peek-next-id)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:gain {:card-name card-name}]
                                         [::gift-of-aether-on-gain {:card-id card-id}]]})))

(effects/register {::gift-of-aether-prep    gift-of-aether-prep
                   ::gift-of-aether-on-gain gift-of-aether-on-gain
                   ::gift-of-aether-gain    gift-of-aether-gain})

(def gift-of-aether {:name        :gift-of-aether
                     :activation  :your-main-phase
                     :charge-cost 6
                     :text        "Gain a spell from any supply pile. You may prep that spell to any player's opened breach."
                     :effects     [[:give-choice {:title   :gift-of-aether
                                                  :text    "Gain a spell from any supply pile."
                                                  :choice  ::gift-of-aether-gain
                                                  :options [:supply {:type :spell}]
                                                  :min     1
                                                  :max     1}]]})

(def malastar {:name     :malastar
               :title    "Breach Mage Mentor"
               :breaches [{:status :destroyed}
                          {:stage 0}
                          {:stage 3}
                          {:stage 1}]
               :hand     [immolate crystal crystal crystal crystal]
               :deck     [crystal crystal crystal crystal crystal]
               :ability  gift-of-aether})

(def moonstone-shard {:name    :moonstone-shard
                      :type    :gem
                      :cost    0
                      :text    ["Gain 1 Aether."
                                "Gain an additional 1 Aether that can only be used to gain a gem."]
                      :effects [[:gain-aether 1]
                                [:gain-aether {:arg 1 :earmark #{:gem}}]]})

(defn black-mirror-cast [game {:keys [player-no breach-no card-name] :as args}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :breaches breach-no :prepped-spells] {:name card-name})]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:spell-effect args]
                                         [:cast-spell (assoc args :card card)]]})))

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
                   :text            ["Cast any player's prepped spell."
                                     "OR"
                                     "Gain 1 Aether."]
                   :effects         [[::garnet-shard-choice]]})

(def divine-augury {:name        :divine-augury
                    :activation  :your-main-phase
                    :charge-cost 5
                    :text        "Any ally draws four cards."
                    :effects     [[:give-choice {:title   :divine-augury
                                                 :text    "Any ally draws four cards."
                                                 :choice  [:draw {:arg 4}]
                                                 :options [:players {:ally true}]
                                                 :min     1
                                                 :max     1}]]})

(def mist-ae {:name     :mist
              :set      :aeon's-end
              :title    "Dagger Captain"
              :breaches [{}
                         {:stage 2}
                         {:stage 1}
                         {:stage 1}]
              :hand     [garnet-shard crystal crystal crystal spark]
              :deck     [crystal crystal crystal spark spark]
              :ability  divine-augury})

(def amethyst-paragon {:name    :amethyst-paragon
                       :type    :gem
                       :cost    0
                       :text    ["Gain 1 Aether."
                                 "Any ally may prep a spell in hand to their opened or closed breach(es)."]
                       :effects [[:gain-aether 1]
                                 [:give-choice {:title   :amethyst-paragon
                                                :text    "Any ally may prep a spell in hand to their opened or closed breach(es)."
                                                :choice  [:prep-spell {:closed-breaches? true}]
                                                :options [:players :hand {:ally     true
                                                                          :type     :spell
                                                                          :can-prep {:closed-breaches? true}}]
                                                :max     1}]]})

(defn exalted-brand-give-spell [game {:keys [player-no from-player card-id]}]
  (let [{{:keys [name] :as card} :card} (ut/get-card-idx game [:players from-player :discard] {:id card-id})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:give-choice {:title   :exalted-brand
                                                                :text    (str "Place the " (ut/format-name name) " into any ally's hand.")
                                                                :choice  [:move-card {:move-card-id card-id
                                                                                      :from-player  from-player
                                                                                      :from         :discard
                                                                                      :to           :hand}]
                                                                :options [:players {:ally true}]
                                                                :min     1
                                                                :max     1}]]}))))

(defn exalted-brand-cast [game {:keys [caster player-no breach-no card-name spells]}]
  (-> game
      (push-effect-stack {:player-no caster
                          :effects   (->> (or spells
                                              (when (and player-no breach-no card-name)
                                                [{:player-no player-no
                                                  :breach-no breach-no
                                                  :card-name card-name}]))
                                          (mapcat (fn [{:keys [player-no breach-no card-name] :as args}]
                                                    (let [{{:keys [id]} :card} (ut/get-card-idx game [:players player-no :breaches breach-no :prepped-spells] {:name card-name})]
                                                      [[:cast-spell (assoc args :caster caster)]
                                                       [::exalted-brand-give-spell {:card-id     id
                                                                                    :from-player player-no}]]))))})))

(defn exalted-brand-choice [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :exalted-brand
                                                      :text    "Cast up to three different spells prepped by any number of players. For each spell cast this way, place that spell into any ally's hand."
                                                      :choice  [::exalted-brand-cast {:caster player-no}]
                                                      :options [:players :prepped-spells]
                                                      :max     3}]]}))

(effects/register {::exalted-brand-give-spell exalted-brand-give-spell
                   ::exalted-brand-cast       exalted-brand-cast
                   ::exalted-brand-choice     exalted-brand-choice})

(def exalted-brand {:name        :exalted-brand
                    :activation  :your-main-phase
                    :charge-cost 6
                    :text        "Cast up to three different spells prepped by any number of players. For each spell cast this way, place that spell into any ally's hand."
                    :effects     [[::exalted-brand-choice]]})

(def mist-we {:name     :mist
              :set      :war-eternal
              :title    "Voidwalker"
              :breaches [{}
                         {:stage 2}
                         {:stage 1}
                         {:stage 1}]
              :hand     [amethyst-paragon crystal crystal crystal spark]
              :deck     [crystal crystal crystal spark spark]
              :ability  exalted-brand})

(defn tourmaline-shard-destroy [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:damage-player 1]
                                                    [:destroy-from-hand {:card-name card-name}]]})))

(effects/register {::tourmaline-shard-destroy tourmaline-shard-destroy})

(def tourmaline-shard {:name    :tourmaline-shard
                       :type    :gem
                       :cost    0
                       :text    ["Gain 1 Aether."
                                 "Any ally may suffer 1 damage. If they do, they destroy a card in hand."]
                       :effects [[:gain-aether 1]
                                 [:give-choice {:title   :tourmaline-shard
                                                :text    "Any ally may suffer 1 damage. If they do, they destroy a card in hand."
                                                :choice  ::tourmaline-shard-destroy
                                                :options [:players :hand {:ally true}]
                                                :max     1}]]})

(defn auspex-rune-prevent-damage [game _]
  (assoc game :prevent-damage true))

(effects/register {::auspex-rune-prevent-damage auspex-rune-prevent-damage})

(def auspex-rune {:name        :auspex-rune
                  :activation  :turn-order-drawn
                  :charge-cost 5
                  :text        "Prevent any damage that the players or Gravehold would suffer during that turn."
                  :effects     [[::auspex-rune-prevent-damage]]})

(def phaedraxa {:name     :phaedraxa
                :title    "Breach Mage Seer"
                :breaches [{:status :destroyed}
                           {:status :opened}
                           {:stage 1}
                           {:stage 2}]
                :hand     [tourmaline-shard crystal crystal crystal spark]
                :deck     [crystal crystal crystal crystal spark]
                :ability  auspex-rune})

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

(def void-shard {:name    :void-shard
                 :type    :gem
                 :cost    0
                 :text    ["Gain 1 Aether."
                           "Gain an additional 1 Aether that can only be used to focus or open a breach."]
                 :effects [[:gain-aether 1]
                           [:gain-aether {:arg 1 :earmark #{:breach}}]]})

(defn ephemera-masque-return-cards [game {:keys [player-no]}]
  (cond-> game
          player-no (push-effect-stack {:player-no player-no
                                        :effects   [[:give-choice {:title   :ephemera-masque
                                                                   :text    "Returns two cards in your discard pile to your hand."
                                                                   :choice  :take-from-discard
                                                                   :options [:player :discard]
                                                                   :min     2
                                                                   :max     2}]]})))

(effects/register {::ephemera-masque-return-cards ephemera-masque-return-cards})

(def ephemera-masque {:name        :ephemera-masque
                      :activation  :your-main-phase
                      :charge-cost 5
                      :text        ["Any ally returns two cards in their discard pile to their hand."
                                    "OR"
                                    "Gravehold gains 5 life."]
                      :effects     [[:give-choice {:title     :ephemera-masque
                                                   :text      "Any ally returns two cards in their discard pile to their hand."
                                                   :choice    ::ephemera-masque-return-cards
                                                   :or-choice {:text    "Gravehold gains 5 life."
                                                               :effects [[:heal-gravehold 5]]}
                                                   :options   [:players {:ally true}]
                                                   :max       1}]]})

(def remnant {:name     :remnant
              :title    "Aethereal Entity"
              :breaches [{}
                         {:stage          0
                          :type           :aethereal
                          :opened-effects [[:gain-aether 1]]}
                         {:stage 0}
                         {:stage 1}]
              :hand     [void-shard crystal crystal spark spark]
              :deck     [crystal crystal crystal crystal spark]
              :ability  ephemera-masque})

(defn coal-shard-effects [game {:keys [player-no card-id]}]
  (let [{:keys [life]} (get-in game [:players player-no])]
    (push-effect-stack game {:player-no player-no
                             :card-id   card-id
                             :effects   (if (<= life 2)
                                          [[:destroy-this]]
                                          [[:gain-aether 3]
                                           [:gain-charge]
                                           [:damage-player 2]])})))

(effects/register {::coal-shard-effects coal-shard-effects})

(def coal-shard {:name    :coal-shard
                 :type    :gem
                 :cost    0
                 :text    ["If you have 2 life or less, destroy this."
                           "Otherwise, gain 3 Aether, gain 1 charge, and suffer 2 damage."]
                 :effects [[::coal-shard-effects]]})

(defn eidolon-shroud-heal [game {:keys [player-no]}]
  (let [{:keys [life]} (get-in game [:players player-no])]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (pos? life)
                                          [[:heal {:life 6}]]
                                          [[:give-choice {:title   :eidolon-shroud
                                                          :text    "Any ally gains 5 life."
                                                          :choice  [:heal {:life 5}]
                                                          :options [:players {:ally true :can-heal true}]
                                                          :min     1
                                                          :max     1}]])})))

(effects/register {::eidolon-shroud-heal eidolon-shroud-heal})

(def eidolon-shroud {:name        :eidolon-shroud
                     :activation  :your-main-phase
                     :charge-cost 6
                     :text        ["Gain 6 life."
                                   "If you are exhausted, any ally gains 5 life instead."]
                     :effects     [[::eidolon-shroud-heal]]})

(def ulgimor {:name     :ulgimor
              :title    "Shadowkin Beast"
              :breaches [{}
                         {:stage 1}
                         {:stage 0}
                         {:stage 1}]
              :hand     [coal-shard crystal crystal spark spark]
              :deck     [crystal crystal crystal spark spark]
              :ability  eidolon-shroud})

(defn flare-damage [game {:keys [player-no] :as args}]
  (let [{:keys [type]} (-> game :turn-order :deck first)
        damage (if (= :nemesis type) 1 3)]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage damage]]})))

(effects/register {::flare-damage flare-damage})

(def flare {:name    :flare
            :type    :spell
            :cost    0
            :cast    "Reveal the top card of the turn order deck, and then place it back on top of the deck. If you revealed a player turn order card, deal 3 damage. Otherwise, deal 1 damage"
            :effects [[:reveal-top-turn-order]
                      [::flare-damage]]})

(def metaphysical-link {:name        :metaphysical-link
                        :activation  :any-main-phase
                        :charge-cost 5
                        :text        ["Allies collectively gain 4 charges."
                                      "Reveal the turn order deck and return the revealed cards in any order."]
                        :effects     [[:give-choice {:title   :metaphysical-link
                                                     :text    "Allies collectively gain 4 charges."
                                                     :choice  :gain-charge
                                                     :options [:players :ability {:ally true :fully-charged false}]
                                                     :min     1
                                                     :max     1}]
                                      [:give-choice {:title   :metaphysical-link
                                                     :text    "Allies collectively gain 3 more charges."
                                                     :choice  :gain-charge
                                                     :options [:players :ability {:ally true :fully-charged false}]
                                                     :min     1
                                                     :max     1}]
                                      [:give-choice {:title   :metaphysical-link
                                                     :text    "Allies collectively gain 2 more charges."
                                                     :choice  :gain-charge
                                                     :options [:players :ability {:ally true :fully-charged false}]
                                                     :min     1
                                                     :max     1}]
                                      [:give-choice {:title   :metaphysical-link
                                                     :text    "Allies collectively gain 1 more charge."
                                                     :choice  :gain-charge
                                                     :options [:players :ability {:ally true :fully-charged false}]
                                                     :min     1
                                                     :max     1}]
                                      [:reveal-turn-order-deck]
                                      [:give-choice {:title    :metaphysical-link
                                                     :text     "Return the revealed turn order cards in any order (top to bottom)."
                                                     :choice   :topdeck-revealed-turn-order-cards
                                                     :options  [:turn-order :revealed]
                                                     :min      6
                                                     :max      6
                                                     :unswift? true}]]})

(def xaxos {:name     :xaxos
            :title    "Breach Mage Adept"
            :breaches [{}
                       {:stage 0}
                       {:stage 0}
                       {:stage 1}]
            :hand     [flare crystal crystal crystal crystal]
            :deck     [crystal crystal crystal crystal spark]
            :ability  metaphysical-link})

(defn illuminate-while-prepped [{:keys [current-player] :as game} {:keys [player-no]}]
  (cond-> game
          (= current-player player-no) (push-effect-stack {:player-no player-no
                                                           :effects   [[:deal-damage 1]]})))

(effects/register {::illuminate-while-prepped illuminate-while-prepped})

(def illuminate {:name          :illuminate
                 :type          :spell
                 :cost          0
                 :text          "While prepped, when you focus or open one of your breaches during your turn, deal 1 damage."
                 :cast          "Deal 1 damage."
                 :while-prepped {:at-focus-breach [[::illuminate-while-prepped]]
                                 :at-open-breach  [[::illuminate-while-prepped]]}
                 :effects       [[:deal-damage 1]]})

(defn imperium-ritual-gain [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :imperium-ritual
                                                      :text    "Gain a card from any supply pile and places it on top of your deck."
                                                      :choice  [:gain {:to          :deck
                                                                       :to-position :top}]
                                                      :options [:supply]
                                                      :min     1
                                                      :max     1}]]}))

(defn imperium-ritual-check-breaches [game {:keys [player-no]}]
  (let [opened-breaches (->> (get-in game [:players player-no :breaches])
                             (filter (comp #{:opened} :status))
                             count)]
    (cond-> game
            (= 4 opened-breaches) (push-effect-stack {:player-no player-no
                                                      :effects   [[:give-choice {:title   :imperium-ritual
                                                                                 :text    "Any ally gains a card from any supply pile and places it on top of their deck."
                                                                                 :choice  ::imperium-ritual-gain
                                                                                 :options [:players {:ally true}]
                                                                                 :min     1
                                                                                 :max     1}]]}))))

(effects/register {::imperium-ritual-gain           imperium-ritual-gain
                   ::imperium-ritual-check-breaches imperium-ritual-check-breaches})

(def imperium-ritual {:name        :imperium-ritual
                      :activation  :your-main-phase
                      :charge-cost 5
                      :text        "Gain a card from any supply pile. If you have four opened breaches, any ally gains a card from any supply pile and places it on top of their deck."
                      :effects     [[:give-choice {:title   :imperium-ritual
                                                   :text    "Gain a card from any supply pile."
                                                   :choice  :gain
                                                   :options [:supply]
                                                   :min     1
                                                   :max     1}]
                                    [::imperium-ritual-check-breaches]]})

(def yan-magda {:name     :yan-magda
                :title    "Enlightened Exile"
                :breaches [{}
                           {:stage 0}
                           {:stage 0}
                           {:stage 0}]
                :hand     [illuminate crystal crystal crystal crystal]
                :deck     [crystal crystal crystal crystal spark]
                :ability  imperium-ritual})

(def mages [adelheim
            brama
            dezmodia
            gex
            indira
            jian
            kadir
            lash
            malastar
            mist-ae
            mist-we
            phaedraxa
            quilius
            remnant
            ulgimor
            xaxos
            yan-magda])
