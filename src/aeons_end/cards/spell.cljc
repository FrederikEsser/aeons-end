(ns aeons-end.cards.spell
  (:require [aeons-end.nemesis]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn amplify-vision-damage [game {:keys [player-no] :as args}]
  (let [all-breaches-opened? (->> (get-in game [:players player-no :breaches])
                                  (remove (comp #{:destroyed} :status))
                                  (every? (comp #{:opened} :status)))]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (if all-breaches-opened? 3 2)]]})))

(effects/register {::amplify-vision-damage amplify-vision-damage})

(def amplify-vision {:name    :amplify-vision
                     :type    :spell
                     :cost    4
                     :cast    ["Focus your closed breach with the lowest focus cost."
                               "Deal 2 damage. If all of your breaches are opened, deal 1 additional damage."]
                     :effects [[:focus-lowest-cost-breach]
                               [::amplify-vision-damage]]
                     :quote   "'The breaches are merely a mirror through which worlds whisper.' Phaedraxa, Breach Mage Seer"})

(defn dark-fire-discard [game {:keys [player-no card-name card-names] :as args}]
  (let [card-count (cond card-name 1
                         card-names (count card-names)
                         :else 0)]
    (push-effect-stack game (merge {:player-no player-no
                                    :args      args         ; bonus-damage
                                    :effects   [[:discard-from-hand args]
                                                [:deal-damage (* 3 card-count)]]}))))

(effects/register {::dark-fire-discard dark-fire-discard})

(def dark-fire {:name    :dark-fire
                :type    :spell
                :cost    5
                :cast    ["Discard up to two cards in hand."
                          "Deal 3 damage for each card discarded this way."]
                :effects [[:give-choice {:title   :dark-fire
                                         :text    "Discard up to two cards in hand"
                                         :choice  ::dark-fire-discard
                                         :options [:player :hand]
                                         :max     2}]]
                :quote   "'Xaxos always says, to come back to light, we must first visit the shadow.' Malastar, Breach Mage Mentor"})

(defn essence-theft-discard [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:discard-from-hand {:card-name card-name}]
                                                    [:give-choice {:title   :essence-theft
                                                                   :text    "Any player gains 1 life."
                                                                   :choice  [:heal {:life 1}]
                                                                   :options [:players {:not-exhausted true}]
                                                                   :min     1
                                                                   :max     1}]]})))

(effects/register {::essence-theft-discard essence-theft-discard})

(def essence-theft {:name    :essence-theft
                    :type    :spell
                    :cost    5
                    :cast    ["Deal 3 damage."
                              "You may discard a card in hand. If you do, any player gains 1 life."]
                    :effects [[:deal-damage 3]
                              [:give-choice {:title   :essence-theft
                                             :text    "You may discard a card in hand. If you do, any player gains 1 life."
                                             :choice  ::essence-theft-discard
                                             :options [:player :hand]
                                             :max     1}]]
                    :quote   "'There is talk of the link between the mages and our adversaries. More and more, the difference between the two becomes harder to see.' Ohat, Dirt Merchant"})

(def ignite {:name    :ignite
             :type    :spell
             :cost    4
             :cast    ["Deal 2 damage."
                       "Any ally gains 1 charge."]
             :effects [[:deal-damage 2]
                       [:give-choice {:title   :ignite
                                      :text    "Any ally gains 1 charge"
                                      :choice  :gain-charge
                                      :options [:players {:ally true}]
                                      :min     1
                                      :max     1}]]
             :quote   "'We are but one flickering flame against unimaginable dark.' Kadir, Breach Mage Delver"})

(def nova-forge {:name          :nova-forge
                 :type          :spell
                 :cost          6
                 :text          "While prepped, once per turn during your main phase you may gain 2 Aether that can only be used to gain a spell."
                 :cast          "Deal 4 damage."
                 :while-prepped {:at-start-main [[:gain-aether {:arg 2 :earmark #{:spell}}]]}
                 :effects       [[:deal-damage 4]]
                 :quote         "'As I have said before, the breach is my anvil, Adelheim.' Gex, Breach Mage Adviser"})

(defn phoenix-flame-damage [game {:keys [player-no area damage] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   (if (= :charges area)
                                        [[:spend-charges 1]
                                         [:deal-damage (+ 2 damage)]]
                                        [[:deal-damage-to-target args]])}))

(defn phoenix-flame-choice [{:keys [nemesis] :as game} {:keys [player-no bonus-damage]
                                                        :or   {bonus-damage 0}}]
  (let [{:keys [name]} nemesis
        damage  (+ 2 bonus-damage)
        charges (get-in game [:players player-no :ability :charges])]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:give-choice {:title   :phoenix-flame
                                                        :text    (concat [(str "Deal " damage " damage to " (ut/format-name (or name :nemesis)) " or a Minion.")]
                                                                         (when (and charges (pos? charges))
                                                                           ["OR"
                                                                            "Lose 1 charge to deal 2 additional damage."]))
                                                        :choice  [::phoenix-flame-damage {:damage damage}]
                                                        :options [:mixed
                                                                  [:nemesis]
                                                                  [:nemesis :minions]
                                                                  [:player :charges]]
                                                        :min     1
                                                        :max     1}]]})))

(effects/register {::phoenix-flame-damage phoenix-flame-damage
                   ::phoenix-flame-choice phoenix-flame-choice})

(def phoenix-flame {:name    :phoenix-flame
                    :type    :spell
                    :cost    3
                    :cast    ["Deal 2 damage."
                              "You may lose 1 charge to deal 2 additional damage."]
                    :effects [[::phoenix-flame-choice]]
                    :quote   "'In The World That Was, there were great birds, born of fire and ash. They were thought to be immortal, and yet none now remain.' Malastar, Breach Mage Mentor"})

(defn planar-insight-damage [game {:keys [player-no] :as args}]
  (let [opened-breaches (->> (get-in game [:players player-no :breaches])
                             (filter (comp #{:opened} :status))
                             count)]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (+ 2 opened-breaches)]]})))

(effects/register {::planar-insight-damage planar-insight-damage})

(def planar-insight {:name    :planar-insight
                     :type    :spell
                     :cost    6
                     :cast    ["Deal 2 damage."
                               "Deal 1 additional damage for each of your opened breaches."]
                     :effects [[::planar-insight-damage]]
                     :quote   "'She may no longer speak, but she sees much.' Brama, Breach Mage Elder"})

(def spectral-echo {:name    :spectral-echo
                    :type    :spell
                    :cost    3
                    :cast    ["Deal 2 damage."
                              "You may destroy a card in hand."]
                    :effects [[:deal-damage 2]
                              [:give-choice {:title   :spectral-echo
                                             :text    "You may destroy a card in hand."
                                             :choice  :destroy-from-hand
                                             :options [:player :hand]
                                             :max     1}]]
                    :quote   "'Clear your mind, child. Drink in the void.' Xaxos, Breach Mage Adept"})

(def radiance {:name    :radiance
               :type    :spell
               :cost    8
               :cast    ["Deal 5 damage."
                         "Each ally draws a card."]
               :effects [[:deal-damage 5]
                         [:other-players {:effects [[:draw 1]]}]]
               :quote   "'All of us, together. For apart we are doomed.' Brama, Breach Mage Elder"})

(def cards [amplify-vision
            dark-fire
            essence-theft
            ignite
            nova-forge
            phoenix-flame
            planar-insight
            radiance
            spectral-echo])
