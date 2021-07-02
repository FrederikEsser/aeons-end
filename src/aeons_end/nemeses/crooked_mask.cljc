(ns aeons-end.nemeses.crooked-mask
  (:require [aeons-end.operations :refer [push-effect-stack add-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.power :as power]))

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

(def generic-corruption-card {:name     :corruption
                              :type     :corruption
                              ;:cost    -1
                              :text     "Destroy this"
                              :effects  [[:destroy-this]]
                              :on-trash [[::corruption-on-trash]]})

(def crooked-mask {:name             :crooked-mask
                   :level            5
                   :life             70
                   :setup            [[::setup]]
                   :unleash          [[::unleash]]
                   :unleash-text     "Any player gains a corruption and places it on top of their deck. That player shuffles their discard pile into their deck."
                   :at-start-casting [[::resolve-corruption]]
                   :at-end-main      [[::resolve-corruption]]
                   :additional-rules ::additional-rules
                   :cards            []
                   :corruption-deck  (repeat 11 generic-corruption-card)})
