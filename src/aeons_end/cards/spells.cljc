(ns aeons-end.cards.spells
  (:require [aeons-end.cards.common]))

(def ignite {:name    :ignite
             :type    :spell
             :cost    4
             :text    "Cast: Deal 2 damage. Any ally gains 1 charge."
             :effects [[:deal-damage 2]
                       [:give-choice {:text    "Ignite: Any ally gains 1 charge"
                                      :choice  :gain-charge
                                      :options [:players {:ally true}]
                                      :min     1
                                      :max     1}]]})
