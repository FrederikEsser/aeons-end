(ns aeons-end.nemeses.prince-of-gluttons
  (:require [aeons-end.operations :refer [push-effect-stack move-card add-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack]
            [aeons-end.cards.power :as power]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.attack :as attack]))

(defn devour [game {:keys [number-of-cards card-name]
                    :or   {number-of-cards 1}}]
  (let [{:keys [pile-size] :or {pile-size 0}} (ut/get-pile-idx game card-name)]
    (push-effect-stack game {:effects (concat (repeat (min number-of-cards pile-size)
                                                      [:move-card {:card-name card-name
                                                                   :from      :supply
                                                                   :to        :devoured}])
                                              (when (> number-of-cards pile-size)
                                                [[:damage-gravehold (* 2 (- number-of-cards pile-size))]]))})))

(effects/register {::devour devour})

(defn do-unleash [{:keys [players] :as game} args]
  (let [title          (keyword (or (:resolving args)
                                    (:resolving game))
                                "unleash")
        cards-devoured (if (<= (count players) 2) 3 2)]
    (push-effect-stack game {:effects [[:give-choice {:title      title
                                                      :text       (str "Devour " (ut/number->text cards-devoured) " cards from the least expensive supply pile.")
                                                      :choice     [::devour {:number-of-cards cards-devoured}]
                                                      :options    [:supply {:least-expensive true
                                                                            :devoured        false}]
                                                      :min        1
                                                      :max        1
                                                      :mandatory? true}]]})))

(effects/register {::unleash do-unleash})

(defn setup [{:keys [supply difficulty] :as game} _]
  (let [gems (->> supply
                  (map :card)
                  (filter (comp #{:gem} :type))
                  (sort-by :cost >)
                  (map :name))]
    (push-effect-stack game {:effects (->> gems
                                           (mapv (fn [card-name]
                                                   [::devour {:card-name card-name}])))})))

(effects/register {::setup setup})

(defn victory-condition [{:keys [supply] :as game}]
  (when (->> supply (every? (comp zero? :pile-size)))
    {:conclusion :defeat
     :text       "All supply piles are empty. All memories of the history of Gravehold have been erased."}))

(effects/register-predicates {::victory-condition victory-condition})

(defn unleash-text [{:keys [players]}]
  (let [cards-devoured (if (<= (count players) 2) 3 2)]
    (str "Devour " (ut/number->text cards-devoured) " cards from the least expensive supply pile.")))

(effects/register-predicates {::unleash-text unleash-text})

(defn lobotomize-choice [game {:keys [area player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :players [[:damage-player 3]]
                                        :supply [[::devour {:number-of-cards 3
                                                            :card-name       card-name}]])}))

(effects/register {::lobotomize-choice lobotomize-choice})

(def lobotomize {:name    :lobotomize
                 :type    :attack
                 :tier    1
                 :text    ["Any player suffers 3 damage."
                           "OR"
                           "Devour three spells from any spell supply pile."]
                 :effects [[:give-choice {:title   :lobotomize
                                          :text    ["Any player suffers 3 damage."
                                                    "OR"
                                                    "Devour three spells from any spell supply pile."]
                                          :choice  ::lobotomize-choice
                                          :options [:mixed
                                                    [:players]
                                                    [:supply {:type     :spell
                                                              :devoured false}]]
                                          :min     1
                                          :max     1}]]
                 :quote   "'I saw the woman's eyes empty and everything she was fell away into nothing.' Nerva, Survivor"})

(defn mindguzzler-set-life [{:keys [supply] :as game} _]
  (let [empty-supply-piles (->> supply
                                (filter (comp zero? :pile-size))
                                count)]
    (ut/update-in-vec game [:nemesis :play-area] {:name :mindguzzler} assoc :life (+ 8 empty-supply-piles))))

(effects/register {::mindguzzler-set-life mindguzzler-set-life})

(def mindguzzler {:name        :mindguzzler
                  :type        :minion
                  :tier        2
                  :immediately {:text    ["Set this minion's life equal to 8 plus the number of empty supply piles."]
                                :effects [[::mindguzzler-set-life]]}
                  :persistent  {:text    "Unleash"
                                :effects [[:unleash]]}})

(defn thought-biter-choice [game {:keys [area player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :players [[:damage-player 2]]
                                        :supply [[::devour {:number-of-cards 2
                                                            :card-name       card-name}]])}))

(effects/register {::thought-biter-choice thought-biter-choice})

(def thought-biter {:name       :thought-biter
                    :type       :minion
                    :tier       1
                    :life       6
                    :persistent {:text    ["Any player suffers 2 damage."
                                           "OR"
                                           "Devour two relics from the least expensive relic supply pile."]
                                 :effects [[:give-choice {:title   :thought-biter
                                                          :text    ["Any player suffers 2 damage."
                                                                    "OR"
                                                                    "Devour two relics from the least expensive relic supply pile."]
                                                          :choice  ::thought-biter-choice
                                                          :options [:mixed
                                                                    [:players]
                                                                    [:supply {:type            :relic
                                                                              :least-expensive true
                                                                              :devoured        false}]]
                                                          :min     1
                                                          :max     1}]]}
                    :quote      "'Is it eating the cave wall?' Nym, Breach Mage Apprentice"})

(def prince-of-gluttons {:name              :prince-of-gluttons
                         :level             5
                         :life              70
                         :setup             [[::setup]]
                         :unleash           [[::unleash]]
                         :unleash-text      ::unleash-text
                         :additional-rules  ["- When a card is Devoured, place it on top of the devoured pile."
                                             "- Cards may be gained from the top of the devoured pile as if it were a supply pile."
                                             "- The players may look through the devoured pile at any time."
                                             "- When Prince of Gluttons would Devour a card from a supply pile that is empty Gravehold suffers 2 damage per card instead."
                                             "- If all supply piles are empty, the players lose."]
                         :victory-condition ::victory-condition
                         :cards             [(attack/generic 1) lobotomize thought-biter
                                             (minion/generic 2 1) (minion/generic 2 2) mindguzzler
                                             (attack/generic 3) (power/generic 3) (minion/generic 3)]})
