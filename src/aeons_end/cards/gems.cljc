(ns aeons-end.cards.gems
  (:require [aeons-end.cards.common]))

(def jade {:name    :jade
           :type    :gem
           :cost    2
           :text    "Gain 2 Aether."
           :effects [[:gain-aether 2]]})
