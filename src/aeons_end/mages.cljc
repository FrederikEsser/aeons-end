(ns aeons-end.mages
  (:require [aeons-end.cards.starter :refer [crystal spark]]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(def buried-light {:name    :buried-light
                   :type    :spell
                   :cost    0
                   :text    "Cast: Deal 1 damage. Gain 1 Aether."
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

(def moonstone-shard {:name    :moonstone-shard
                      :type    :gem
                      :cost    0
                      :text    "Gain 1 Aether\nGain an additional 1 Aether that can only be used to gain a gem."
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
