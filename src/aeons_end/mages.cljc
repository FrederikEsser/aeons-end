(ns aeons-end.mages
  (:require [aeons-end.cards.starter :refer [crystal spark]]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

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

(defn shattered-geode-take-card [game {:keys [player-no card-id to-player]}]
  (cond-> game
          card-id (push-effect-stack {:player-no player-no
                                      :effects   [[:move-card {:card-id   card-id
                                                               :from      :discard
                                                               :to-player to-player
                                                               :to        :hand}]]})))

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
  (let [exhausted? (zero? (get-in game [:players player-no :life]))]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[:draw 1]]
                                                (when-not exhausted?
                                                  [[:heal {:life 2}]]))})))

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

(def mages [brama
            gex
            jian
            mist
            quilius])
