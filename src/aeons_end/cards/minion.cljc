(ns aeons-end.cards.minion
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(def bane-sire {:name       :bane-sire
                :type       :minion
                :tier       1
                :life       6
                :persistent {:text    "Unleash."
                             :effects [[:unleash]]}
                :quote      "'Best to kill these first.' Reeve, Breach Mage Elite"})

(def catacomb-drone {:name       :catacomb-drone
                     :type       :minion
                     :tier       1
                     :life       5
                     :persistent {:text    ["Unleash."
                                            "Gravehold suffers 1 damage."]
                                  :effects [[:unleash]
                                            [:damage-gravehold 1]]}
                     :quote      "'Kadir remains convinced the drones were once merely creatures from The Depths.'"})

(defn cauterizer-modify-damage [_ damage]
  (min damage 1))

(effects/register-predicates {::cauterizer-modify-damage cauterizer-modify-damage})

(defn cauterizer-damage [game _]
  (let [{{:keys [life]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name :cauterizer})]
    (push-effect-stack game {:effects [[:give-choice {:title   :cauterizer
                                                      :text    (str "Any player suffers " life " damage.")
                                                      :choice  [:damage-player {:arg life}]
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::cauterizer-damage cauterizer-damage})

(def cauterizer {:name          :cauterizer
                 :type          :minion
                 :tier          2
                 :life          3
                 :text          "When damage is dealt to this minion, reduce that damage to 1."
                 :modify-damage ::cauterizer-modify-damage
                 :persistent    {:text    "Any player suffers damage equal to this minion's current life."
                                 :effects [[::cauterizer-damage]]}
                 :quote         "'Hack away all you like, youngling. It will only grow back another... whatever that part is.' Adelheim, Breach Mage Weaponsmith"})

(defn haze-spewer-damage [game _]
  (let [{{:keys [life]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name :haze-spewer})]
    (push-effect-stack game {:effects [[:damage-gravehold life]]})))

(effects/register {::haze-spewer-damage haze-spewer-damage})

(def haze-spewer {:name       :haze-spewer
                  :type       :minion
                  :tier       1
                  :life       5
                  :persistent {:text    ["Gravehold suffers damage equal to this minion's current life."
                                         "Then, this minion suffers 1 damage."]
                               :effects [[::haze-spewer-damage]
                                         [:deal-damage-to-minion {:card-name :haze-spewer
                                                                  :damage    1}]]}
                  :quote      "'The fumes they belch are so caustic that they rarely live long themselves.'"})

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

(def jagged-one {:name       :jagged-one
                 :type       :minion
                 :tier       3
                 :life       14
                 :persistent {:text    "Unleash twice."
                              :effects [[:unleash]
                                        [:unleash]]}
                 :quote      "'Jagged Ones are heralds of The Nameless, leading legions in legions in siege after siege of Gravehold.'"})

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
                 :persistent {:text    ["Gravehold suffers 3 damage."
                                        "This minion suffers 2 damage."]
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

(def needlemaw {:name       :needlemaw
                :type       :minion
                :tier       2
                :life       11
                :persistent {:text    "Gravehold suffers 2 damage."
                             :effects [[:damage-gravehold 2]]}
                :quote      "'The teeth of this beast make a fine trophy. The trick is keeping all your fingers in the taking of them.' Reeve, Breach Mage Elite"})

(def null-scion {:name       :null-scion
                 :type       :minion
                 :tier       2
                 :life       11
                 :persistent {:text    "Unleash."
                              :effects [[:unleash]]}
                 :quote      "'Qiulius's first blow glanced off its carapace like breath, but her second blow carved it in twain.' Dezmodia, Voidborn Prodigy"})

(defn venomite-choice [game {:keys [area player-no breach-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :players [[:damage-player 2]]
                                        :prepped-spells [[:discard-prepped-spells {:breach-no breach-no
                                                                                   :card-name card-name}]])}))

(effects/register {::venomite-choice venomite-choice})

(def venomite {:name       :venomite
               :type       :minion
               :tier       2
               :life       9
               :persistent {:text    ["The player with the lowest life suffers 2 damage."
                                      "OR"
                                      "Any player discards a prepped spell that costs 3 Aether or more."]
                            :effects [[:give-choice {:title   :venomite
                                                     :text    ["The player with the lowest life suffers 2 damage."
                                                               "OR"
                                                               "Any player discards a prepped spell that costs 3 Aether or more."]
                                                     :choice  ::venomite-choice
                                                     :options [:mixed
                                                               [:players {:lowest-life true}]
                                                               [:players :prepped-spells {:min-cost 3}]]
                                                     :min     1
                                                     :max     1}]]}
               :quote      "'In The World That Was, there were many creatures that used poison as a means of survival. But these things ARE poison.' Lash, Breach Mage Scout"})

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
