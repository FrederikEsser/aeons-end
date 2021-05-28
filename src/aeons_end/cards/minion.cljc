(ns aeons-end.cards.minion
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(def catacomb-drone {:name       :catacomb-drone
                     :type       :minion
                     :tier       1
                     :life       5
                     :persistent {:text    ["Unleash."
                                            "Gravehold suffers 1 damage."]
                                  :effects [[:unleash]
                                            [:damage-gravehold 1]]}
                     :quote      "'Kadir remains convinced the drones were once merely creatures from The Depths.'"})

(def howling-spinners {:name       :howling-spinners
                       :type       :minion
                       :tier       1
                       :life       5
                       :persistent {:text    "Any player suffers 2 damage."
                                    :effects [[:give-choice {:title   :howling-spinners
                                                             :text    "Any player suffers 2 damage."
                                                             :choice  [:damage-player {:arg 2}]
                                                             :options [:players]
                                                             :min     1
                                                             :max     1}]]}
                       :quote      "'The sound they make as they weave echoes through the cave is like the otherworldly throes of our lost companions.' Ges, Breach Mage Adviser"})

(def mangleroot {:name       :mangleroot
                 :type       :minion
                 :tier       2
                 :life       12
                 :persistent {:text    "Gravehold suffers 3 damage.\nThis minion suffers 2 damage."
                              :effects [[:damage-gravehold 3]
                                        [:deal-damage-to-minion {:card-name :mangleroot
                                                                 :damage    2}]]}
                 :quote      "'One of the few indigenous creatures of the cave, though corrupted from exposure to the breach.' ― Gex, Breach Mage Advisor"})

(defn monstrosity-of-omens-modify-damage [damage]
  (min damage 1))

(effects/register-predicates {::monstrosity-of-omens-modify-damage monstrosity-of-omens-modify-damage})

(defn monstrosity-of-omens-damage [game _]
  (let [{{:keys [life]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name :monstrosity-of-omens})]
    (push-effect-stack game {:effects [[:damage-gravehold life]]})))

(effects/register {::monstrosity-of-omens-damage monstrosity-of-omens-damage})

(def monstrosity-of-omens {:name          :monstrosity-of-omens
                           :type          :minion
                           :tier          3
                           :life          5
                           :modify-damage ::monstrosity-of-omens-modify-damage
                           :text          "When damage is dealt to this minion, reduce that damage to 1."
                           :persistent    {:text    "Gravehold suffers damage equal to this minion's current life."
                                           :effects [[::monstrosity-of-omens-damage]]}
                           :quote         "'Never have I seen such a creature, even among the ranks of the Nameless.' ― Yan Magda, Enlightened Exile"})

(def null-scion {:name       :null-scion
                 :type       :minion
                 :tier       2
                 :life       11
                 :persistent {:text    "Unleash."
                              :effects [[:unleash]]}
                 :quote      "'Qiulius's first blow glanced off its carapace like breath, but her second blow carved it in twain.' ― Dezmodia, Voidborn Prodigy"})