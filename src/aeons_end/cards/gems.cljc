(ns aeons-end.cards.gems
  (:require [aeons-end.cards.common]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(def jade {:name    :jade
           :type    :gem
           :cost    2
           :text    "Gain 2 Aether."
           :effects [[:gain-aether 2]]})

(defn pain-stone-choices [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :aether [[:gain-aether 3]]
                                        :damage [[:gain-aether 2]
                                                 [:deal-damage 1]])}))

(effects/register {::pain-stone-choices pain-stone-choices})

(def pain-stone {:name    :pain-stone
                 :type    :gem
                 :cost    6
                 :text    "Gain 3 Aether. OR Gain 2 Aether and deal 1 damage."
                 :effects [[:give-choice {:text    "Pain Stone - Choose one:"
                                          :choice  ::pain-stone-choices
                                          :options [:special
                                                    {:option :aether :text "Gain 3 Aether"}
                                                    {:option :damage :text "Gain 2 Aether and deal 1 damage"}]
                                          :min     1
                                          :max     1}]]})
