(ns aeons-end.cards.spells
  (:require [aeons-end.cards.common]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(def ignite {:name    :ignite
             :type    :spell
             :cost    4
             :text    "Cast: Deal 2 damage. Any ally gains 1 charge."
             :effects [[:deal-damage 2]
                       [:give-choice {:text    "Ignite - Any ally gains 1 charge"
                                      :choice  :gain-charge
                                      :options [:players {:ally true}]
                                      :min     1
                                      :max     1}]]})

(defn dark-fire-discard [game {:keys [player-no card-name card-names] :as args}]
  (let [card-count (cond card-name 1
                         card-names (count card-names)
                         :else 0)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:discard-from-hand args]
                                         [:deal-damage (* 3 card-count)]]})))

(effects/register {::dark-fire-discard dark-fire-discard})

(def dark-fire {:name    :dark-fire
                :type    :spell
                :cost    5
                :text    "Cast: Discard up to two cards in hand. Deal 3 damage for each card discarded this way."
                :effects [[:give-choice {:text    "Dark Fire - Discard up to two cards in hand"
                                         :choice  ::dark-fire-discard
                                         :options [:player :hand]
                                         :max     2}]]})
