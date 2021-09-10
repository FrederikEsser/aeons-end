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
                         :cards             [(attack/generic 1 1) (attack/generic 1 2) (minion/generic 1)
                                             (minion/generic 2 1) (minion/generic 2 2) (minion/generic 2 3)
                                             (attack/generic 3) (power/generic 3) (minion/generic 3)]})
