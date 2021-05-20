(ns aeons-end.cards.relics
  (:require [aeons-end.cards.common]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(defn unstable-prism-play-gem [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:play-twice {:card-name card-name}]
                                       [:move-card {:card-name card-name
                                                    :from      :play-area
                                                    :to        :trash}]]}))

(effects/register {::unstable-prism-play-gem unstable-prism-play-gem})

(def unstable-prism {:name    :unstable-prism
                     :type    :relic
                     :cost    3
                     :text    "Play a gem in hand twice and destroy it. OR Gain 2 Aether."
                     :effects [[:give-choice {:title     :unstable-prism
                                              :text      "Play a gem in hand twice and destroy it."
                                              :choice    ::unstable-prism-play-gem
                                              :or-choice {:text    "Gain 2 Aether"
                                                          :effects [[:gain-aether 2]]}
                                              :options   [:player :hand {:type :gem}]
                                              :max       1}]]})
