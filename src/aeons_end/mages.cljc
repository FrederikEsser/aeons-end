(ns aeons-end.mages
  (:require [aeons-end.cards.base :refer [crystal spark]]
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
