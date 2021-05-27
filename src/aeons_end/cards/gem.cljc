(ns aeons-end.cards.gem
  (:require [aeons-end.cards.common :refer [gain-aether]]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(defn alien-element-gain-aether [game {:keys [player-no]}]
  (let [breaches-with-prepped-spells (->> (get-in game [:players player-no :breaches])
                                          (filter (comp not-empty :prepped-spells))
                                          count)]
    (gain-aether game {:player-no player-no
                       :arg       (inc breaches-with-prepped-spells)})))

(effects/register {::alien-element-gain-aether alien-element-gain-aether})

(def alien-element {:name    :alien-element
                    :type    :gem
                    :cost    4
                    :text    "Gain 1 Aether.\nFor each of your breaches with a spell prepped to it, gain an additional 1 Aether."
                    :effects [[::alien-element-gain-aether]]})

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

(def pain-stone {:name            :pain-stone
                 :type            :gem
                 :cost            6
                 :auto-play-index 1
                 :text            "Gain 3 Aether.\nOR\nGain 2 Aether and deal 1 damage."
                 :effects         [[:give-choice {:title   :pain-stone
                                                  :choice  ::pain-stone-choices
                                                  :options [:special
                                                            {:option :aether :text "Gain 3 Aether"}
                                                            {:option :damage :text "Gain 2 Aether and deal 1 damage"}]
                                                  :min     1
                                                  :max     1}]]})
