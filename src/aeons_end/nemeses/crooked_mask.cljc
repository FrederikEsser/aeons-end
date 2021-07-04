(ns aeons-end.nemeses.crooked-mask
  (:require [aeons-end.operations :refer [push-effect-stack add-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.power :as power]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.attack :as attack]))

(defn gain-corruption [game {:keys [player-no to]}]
  (let [[card & new-corruption-deck] (get-in game [:nemesis :corruption-deck])
        to-position (if (= :deck to)
                      :top
                      :bottom)]
    (-> game
        (assoc-in [:nemesis :corruption-deck] (vec new-corruption-deck))
        (as-> game
              (if card
                (-> game
                    (add-card [:players player-no to] to-position card)
                    (cond-> (= :deck to) (update-in [:players player-no] dissoc :revealed-cards)))
                (push-effect-stack game {:effects [[:damage-gravehold 2]]}))))))

(defn gain-corruption-and-shuffle [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[::gain-corruption {:to :deck}]
                                       [:shuffle-discard-into-deck]]}))

(defn do-unleash [game args]
  (let [title (keyword (or (:resolving args)
                           (:resolving game))
                       "Unleash")]
    (push-effect-stack game {:effects [[:give-choice {:title   title
                                                      :text    "Any player gains a corruption and places it on top of their deck. That player shuffles their discard pile into their deck."
                                                      :choice  ::gain-corruption-and-shuffle
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::gain-corruption             gain-corruption
                   ::gain-corruption-and-shuffle gain-corruption-and-shuffle
                   ::unleash                     do-unleash})

(defn corruption-on-trash [{:keys [difficulty] :as game} {:keys [card-id]}]
  (let [{{:keys [name]} :card} (ut/get-card-idx game [:trash] {:id card-id})]
    (push-effect-stack game {:effects (if (#{:beginner :normal} difficulty)
                                        [[:move-card {:move-card-id card-id
                                                      :from         :trash
                                                      :to           :corruption-deck
                                                      :to-position  :bottom}]]
                                        [[:give-choice {:title   name
                                                        :text    (str "Place " (ut/format-name name) " into any player's discard pile.")
                                                        :choice  [:move-card {:move-card-id card-id
                                                                              :from         :trash
                                                                              :to           :discard}]
                                                        :options [:players]
                                                        :min     1
                                                        :max     1}]])})))

(effects/register {::corruption-on-trash corruption-on-trash})

(defn resolve-corruption [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat (when card
                                                  [[:move-card {:card-name card-name
                                                                :from      :hand
                                                                :to        :play-area}]
                                                   [:card-effect {:card card}]])
                                                [[:give-choice {:title   :corruption
                                                                :text    "Resolve all corruptions in your hand, in any order."
                                                                :choice  ::resolve-corruption
                                                                :options [:player :hand {:type :corruption}]
                                                                :min     1
                                                                :max     1}]])})))

(effects/register {::resolve-corruption resolve-corruption})

(defn additional-rules [{:keys [difficulty]}]
  [(if (#{:beginner :normal} difficulty)
     "- When a corruption is destroyed, place it on the bottom of the corruption deck."
     "- When a corruption is destroyed, place it into any player's discard pile.")
   "- When a player would gain a corruption and the corruption deck is empty, Gravehold suffers 2 damage instead."
   "- At the start of each player's turn, before that player's casting phase, that player resolves all corruptions in their hand, in any order."
   "- At the end of each player's main phase, that player resolves all corruptions they drew during their turn."
   "- Corruptions may not be played at any other time."])

(effects/register-predicates {::additional-rules additional-rules})

(defn setup [game _]
  (let [corruption-deck (->> (get-in game [:nemesis :corruption-deck])
                             shuffle
                             (mapv ut/give-id!))]
    (assoc-in game [:nemesis :corruption-deck] corruption-deck)))

(effects/register {::setup setup})

(def blind-abandon {:name     :blind-abandon
                    :type     :corruption
                    :text     ["Suffer 1 damage."
                               "You may destroy a non-corruption card in hand."
                               "Destroy this."]
                    :effects  [[:damage-player 1]
                               [:give-choice {:title   :blind-abandon
                                              :text    "You may destroy a non-corruption card in hand."
                                              :choice  :destroy-from-hand
                                              :options [:player :hand {:not-type :corruption}]
                                              :max     1}]
                               [:destroy-this]]
                    :on-trash [[::corruption-on-trash]]})

(def contagion {:name     :contagion
                :type     :corruption
                :text     ["Suffer 1 damage."
                           "Return any card that costs 0 Aether in your discard pile to your hand."
                           "Destroy this"]
                :effects  [[:damage-player 1]
                           [:give-choice {:title   :contagion
                                          :text    "Return any card that costs 0 Aether in your discard pile to your hand."
                                          :choice  :take-from-discard
                                          :options [:player :discard {:cost 0}]
                                          :min     1
                                          :max     1}]
                           [:destroy-this]]
                :on-trash [[::corruption-on-trash]]})

(def delirium-veil {:name     :delirium-veil
                    :type     :corruption
                    :text     ["Gravehold suffers 2 damage."
                               "Focus a breach."
                               "Destroy this"]
                    :effects  [[:damage-gravehold 2]
                               [:give-choice {:title   :delirium-veil
                                              :text    "Focus a breach."
                                              :choice  :focus-breach
                                              :options [:player :breaches {:stati #{:closed :focused}}]
                                              :min     1
                                              :max     1}]
                               [:destroy-this]]
                    :on-trash [[::corruption-on-trash]]})

(def dire-wisdom {:name     :dire-wisdom
                  :type     :corruption
                  :text     ["Gain a spell from any spell supply pile."
                             "Gain three corruptions and place them on top of your deck."
                             "Destroy this"]
                  :effects  [[:give-choice {:title   :dire-wisdom
                                            :text    "Gain a spell from any spell supply pile."
                                            :choice  :gain
                                            :options [:supply {:type :spell}]
                                            :min     1
                                            :max     1}]
                             [::gain-corruption {:to :deck}]
                             [::gain-corruption {:to :deck}]
                             [::gain-corruption {:to :deck}]
                             [:destroy-this]]
                  :on-trash [[::corruption-on-trash]]})

(def endless-hunger {:name     :endless-hunger
                     :type     :corruption
                     :text     ["Gravehold suffers 3 damage."
                                "Gain 2 life."
                                "Destroy this"]
                     :effects  [[:damage-gravehold 3]
                                [:heal {:life 2}]
                                [:destroy-this]]
                     :on-trash [[::corruption-on-trash]]})

(def fever-of-war {:name     :fever-of-war
                   :type     :corruption
                   :text     ["Suffer 1 damage."
                              "Deal 2 damage."
                              "Destroy this"]
                   :effects  [[:damage-player 1]
                              [:deal-damage 2]
                              [:destroy-this]]
                   :on-trash [[::corruption-on-trash]]})

(def grim-sight {:name     :grim-sight
                 :type     :corruption
                 :text     ["Gravehold suffers 2 damage."
                            "Look at the top card of your deck. You may destroy it."
                            "Destroy this"]
                 :effects  [[:damage-gravehold 2]
                            [:reveal-from-deck 1]
                            [:give-choice {:title   :grim-sight
                                           :text    "Look at the top card of your deck. You may destroy it."
                                           :choice  :trash-from-revealed
                                           :options [:player :revealed]
                                           :max     1}]
                            [:topdeck-all-revealed]
                            [:destroy-this]]
                 :on-trash [[::corruption-on-trash]]})

(def lust-for-power {:name     :lust-for-power
                     :type     :corruption
                     :text     ["Suffer 1 damage."
                                "Gain 1 charge."
                                "Destroy this"]
                     :effects  [[:damage-player 1]
                                [:gain-charge]
                                [:destroy-this]]
                     :on-trash [[::corruption-on-trash]]})

(def generic-corruption-card {:name     :corruption
                              :type     :corruption
                              :text     ["Destroy this"]
                              :effects  [[:destroy-this]]
                              :on-trash [[::corruption-on-trash]]})

(defn twisting-madness-discard [game {:keys [player-no] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:discard-from-hand args]
                                       [:draw 1]]}))

(effects/register {::twisting-madness-discard twisting-madness-discard})

(def twisting-madness {:name       :twisting-madness
                       :type       :power
                       :tier       2
                       :to-discard {:text      "Discard four cards in hand and draw one card."
                                    :predicate [::power/cards-in-hand? {:amount 4}]
                                    :effects   [[:give-choice {:title   :twisting-madness
                                                               :text    "Discard four cards in hand and draw one card."
                                                               :choice  ::twisting-madness-discard
                                                               :options [:player :hand]
                                                               :min     4
                                                               :max     4}]]}
                       :power      {:power   2
                                    :text    ["Gravehold gain 3 life."
                                              "Crooked Mask gain 13 life."]
                                    :effects [[:heal-gravehold 3]
                                              [:heal-nemesis 13]]}
                       :quote      "'Is it laughing at us?' Nym, Breach Mage Apprentice"})

(def crooked-mask {:name             :crooked-mask
                   :level            5
                   :life             70
                   :setup            [[::setup]]
                   :unleash          [[::unleash]]
                   :unleash-text     "Any player gains a corruption and places it on top of their deck. That player shuffles their discard pile into their deck."
                   :at-start-casting [[::resolve-corruption]]
                   :at-end-main      [[::resolve-corruption]]
                   :additional-rules ::additional-rules
                   :cards            [(attack/generic 1 1) (attack/generic 1 2) (minion/generic 1)
                                      (attack/generic 2) (minion/generic 2) twisting-madness
                                      (attack/generic 3) (minion/generic 3 1) (minion/generic 3 2)]
                   :corruption-deck  (concat [blind-abandon
                                              contagion
                                              delirium-veil
                                              dire-wisdom
                                              endless-hunger
                                              fever-of-war
                                              grim-sight
                                              lust-for-power]
                                             (repeat 3 generic-corruption-card))})
