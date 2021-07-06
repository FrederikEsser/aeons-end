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

(defn labyrinth-wisp-choice [game {:keys [player-no area breach-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :prepped-spells [[:discard-prepped-spells {:breach-no breach-no :card-name card-name}]]
                                        :ability [[:spend-charges 1]])}))

(effects/register {::labyrinth-wisp-choice labyrinth-wisp-choice})

(def labyrinth-wisp {:name       :labyrinth-wisp
                     :type       :minion
                     :tier       1
                     :life       5
                     :persistent {:text    ["Any player discards a prepped spell."
                                            "OR"
                                            "Any player loses 1 charge."]
                                  :effects [[:give-choice {:title   :labyrinth-wisp
                                                           :text    ["Any player discards a prepped spell."
                                                                     "OR"
                                                                     "Any player loses 1 charge."]
                                                           :choice  ::labyrinth-wisp-choice
                                                           :options [:mixed
                                                                     [:players :prepped-spells]
                                                                     [:players :ability {:min-charges 1}]]
                                                           :min     1
                                                           :max     1}]]}
                     :quote      "'It is the very breath of the void.' Yan Magda, Enlightened Exile"})

(def mage-ender {:name       :mage-ender
                 :type       :minion
                 :tier       2
                 :life       9
                 :persistent {:text    "The player with the most opened breaches suffers 2 damage."
                              :effects [[:give-choice {:title   :mage-ender
                                                       :text    "The player with the most opened breaches suffers 2 damage."
                                                       :choice  [:damage-player {:arg 2}]
                                                       :options [:players {:most-opened-breaches true}]
                                                       :min     1
                                                       :max     1}]]}
                 :quote      "'It sneered as it lunged at us, an awful maw of jagged teeth eager to end one of us, if not all.' Ohat, Dirt Merchant"})

(def mangleroot {:name       :mangleroot
                 :type       :minion
                 :tier       2
                 :life       12
                 :persistent {:text    "Gravehold suffers 3 damage.\nThis minion suffers 2 damage."
                              :effects [[:damage-gravehold 3]
                                        [:deal-damage-to-minion {:card-name :mangleroot
                                                                 :damage    2}]]}
                 :quote      "'One of the few indigenous creatures of the cave, though corrupted from exposure to the breach.' Gex, Breach Mage Advisor"})

(defn monstrosity-of-omens-modify-damage [_ damage]
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
                           :text          "When damage is dealt to this minion, reduce that damage to 1."
                           :modify-damage ::monstrosity-of-omens-modify-damage
                           :persistent    {:text    "Gravehold suffers damage equal to this minion's current life."
                                           :effects [[::monstrosity-of-omens-damage]]}
                           :quote         "'Never have I seen such a creature, even among the ranks of the Nameless.' Yan Magda, Enlightened Exile"})

(def null-scion {:name       :null-scion
                 :type       :minion
                 :tier       2
                 :life       11
                 :persistent {:text    "Unleash."
                              :effects [[:unleash]]}
                 :quote      "'Qiulius's first blow glanced off its carapace like breath, but her second blow carved it in twain.' Dezmodia, Voidborn Prodigy"})

(defn generic [tier & [idx]]
  (let [name (str (case tier
                    1 "Small"
                    2 "Medium"
                    3 "Large")
                  " Minion"
                  (when idx
                    (str " (" idx ")")))]
    {:name       name
     :type       :minion
     :tier       tier
     :life       (case tier
                   1 6
                   2 11
                   3 14)
     :persistent (case tier
                   1 {:text    "Unleash."
                      :effects [[:unleash]]}
                   2 {:text    "Gravehold suffers 2 damage."
                      :effects [[:damage-gravehold 2]]}
                   3 {:text    "Unleash twice."
                      :effects [[:unleash]
                                [:unleash]]})}))
