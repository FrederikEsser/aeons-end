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

(def brama {:name     :brama
            :title    "Breach Mage Elder"
            :breaches [{}
                       {:stage 1}
                       {:stage 0}
                       {:stage 2}]
            :hand     [buried-light crystal crystal crystal crystal]
            :deck     [crystal crystal crystal spark spark]
            :ability  {:name        :brink-siphon
                       :activation  :your-main-phase
                       :charge-cost 5
                       :text        "Any player gains 4 life."
                       :effects     []}})

(defn garnet-shard-choices [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :aether [[:gain-aether 1]]
                                        :cast [[:give-choice {:text    "Cast any player's prepped spell"
                                                              :choice  :cast-spell
                                                              :options [:players :prepped-spells]
                                                              :min     1
                                                              :max     1}]])}))

(effects/register {::garnet-shard-choices garnet-shard-choices})

(def garnet-shard {:name    :garnet-shard
                   :type    :gem
                   :cost    0
                   :text    "Gain 1 Aether. OR Cast any player's prepped spell."
                   :effects [[:give-choice {:text    "Choose one:"
                                            :choice  ::garnet-shard-choices
                                            :options [:special
                                                      {:option :aether :text "Gain 1 Aether"}
                                                      {:option :cast :text "Cast any player's prepped spell"}]
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
           :ability  {:name        :divine-augury
                      :activation  :your-main-phase
                      :charge-cost 5
                      :text        "Any ally draws four cards."
                      :effects     []}})
