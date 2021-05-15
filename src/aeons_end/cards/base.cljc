(ns aeons-end.cards.base)

(def spark {:name    :spark
            :type    :spell
            :cost    0
            :text    "Cast: Deal 1 damage."
            :effects [[:deal-damage 1]]})

(def crystal {:name    :crystal
              :type    :gem
              :cost    0
              :text    "Gain 1 Aether."
              :effects [[:gain-aether 1]]})
