(ns aeons-end.cards.relics
  (:require [aeons-end.cards.common]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(defn unstable-prism-play [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:play-twice {:card-name card-name}]
                                       [:move-card {:card-name card-name
                                                    :from      :play-area
                                                    :to        :trash}]]}))

(defn unstable-prism-choices [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :play-gem [[:give-choice {:text    "Unstable Prism - Play a gem in hand twice and destroy it."
                                                                  :choice  ::unstable-prism-play
                                                                  :options [:player :hand {:type :gem}]
                                                                  :min     1
                                                                  :max     1}]]
                                        :aether [[:gain-aether 2]])}))

(effects/register {::unstable-prism-choices unstable-prism-choices
                   ::unstable-prism-play    unstable-prism-play})

(def unstable-prism {:name    :unstable-prism
                     :type    :relic
                     :cost    3
                     :text    "Play a gem in hand twice and destroy it. OR Gain 2 Aether."
                     :effects [[:give-choice {:text    "Unstable Prism - Choose one:"
                                              :choice  ::unstable-prism-choices
                                              :options [:special
                                                        {:option :play-gem :text "Play a gem in hand twice and destroy it"}
                                                        {:option :aether :text "Gain 2 Aether"}]
                                              :min     1
                                              :max     1}]]})
