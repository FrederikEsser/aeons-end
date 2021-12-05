(ns aeons-end.nemeses.gate-witch
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack :as attack]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.power :as power]))

(defn open-gate [game _]
  (update-in game [:nemesis :time-gates] ut/plus 1))

(defn close-gates [{:keys [nemesis] :as game} {:keys [arg]}]
  (let [{:keys [time-gates]} nemesis]
    (assoc-in game [:nemesis :time-gates] (max (- time-gates arg) 0))))

(defn accelerate-time [{:keys [difficulty] :as game} _]
  (push-effect-stack game {:effects [[:give-choice {:title   "Gate Witch accelerates time!"
                                                    :text    "Shuffle a nemesis turn order card into the turn order deck."
                                                    :effect  :shuffle-into-turn-order-deck
                                                    :options [:turn-order :discard {:type :nemesis}]
                                                    :min     1
                                                    :max     1}]
                                     [::close-gates (if (#{:expert :extinction} difficulty) 3 4)]]}))

(defn at-end-turn [{:keys [nemesis] :as game} _]
  (let [{:keys [draw-extra?]} nemesis
        discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)
        accelerate-time?        (and (<= 5 (get-in game [:nemesis :time-gates]))
                                     (= 1 discarded-nemesis-cards))]
    (-> game
        (update :nemesis dissoc :draw-extra?)
        (push-effect-stack {:effects (concat (when draw-extra?
                                               [[:draw-nemesis-card]])
                                             (when accelerate-time?
                                               [[::accelerate-time]]))}))))

(effects/register {::open-gate       open-gate
                   ::close-gates     close-gates
                   ::accelerate-time accelerate-time
                   ::at-end-turn     at-end-turn})

(defn return-nemesis-card [game {:keys [card-name]}]
  (push-effect-stack game {:effects [[:move-card {:card-name card-name
                                                  :from      :play-area
                                                  :to        :deck}]]}))

(effects/register {::return-nemesis-card return-nemesis-card})

(defn- get-damage-effect [{{:keys [player-no player-nos] :as type} :type}]
  (cond
    (= :wild type) [:give-choice {:title   :deep-abomination
                                  :text    "Any player suffers 1 damage."
                                  :effect  [:damage-player {:arg 1}]
                                  :options [:players]
                                  :min     1
                                  :max     1}]
    player-no [:damage-player {:player-no player-no
                               :arg       1}]
    player-nos [:give-choice {:title   :deep-abomination
                              :text    (str "Player "
                                            (case player-nos
                                              #{0 1} "1 or 2"
                                              #{2 3} "3 or 4")
                                            " suffers 1 damage.")
                              :effect  [:damage-player {:arg 1}]
                              :options [:players {:player-nos player-nos}]
                              :min     1
                              :max     1}]))

(defn deep-abomination-damage [game _]
  (let [turn-order-cards        (->> (get-in game [:turn-order :discard])
                                     (remove (comp #{:nemesis} :type)))
        duplicate-4p-player-nos (->> turn-order-cards
                                     (keep (comp :player-nos :type))
                                     frequencies
                                     (keep (fn [[k v]] (when (= 2 v) k)))
                                     set)
        damage-effects          (->> turn-order-cards
                                     (remove (comp duplicate-4p-player-nos :player-nos :type))
                                     (concat (for [player-no (apply concat duplicate-4p-player-nos)]
                                               {:type {:player-no player-no}}))
                                     (sort-by (comp #{:wild} :type))
                                     (map get-damage-effect))]
    (cond-> game
            (not-empty damage-effects) (push-effect-stack {:effects damage-effects}))))

(effects/register {::deep-abomination-damage deep-abomination-damage})

(def deep-abomination {:name       :deep-abomination
                       :type       :minion
                       :tier       1
                       :life       6
                       :persistent {:text    "Each player suffers 1 damage for each turn order card that player has in the turn order discard pile."
                                    :effects [[::deep-abomination-damage]]}
                       :quote      "'It is said that each abomination is but the same creature from across all worlds twisted into a single form.'"})

(defn hasten-damage [game _]
  (let [damage (get-in game [:nemesis :time-gates])]
    (push-effect-stack game {:effects [[:give-choice {:title   :hasten
                                                      :text    (str "Any player suffers " damage " damage.")
                                                      :effect  [:damage-player {:arg damage}]
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::hasten-damage hasten-damage})

(def hasten {:name    :hasten
             :type    :attack
             :tier    1
             :text    ["Any player suffers damage equal to the number of open time gates."
                       "Unleash."
                       "Place this card on the bottom of the nemesis deck."]
             :effects [[::hasten-damage]
                       [:unleash]
                       [::return-nemesis-card {:card-name :hasten}]]
             :quote   "'Time is The Witch's domain, the one thing we have so little left.' Gex, Breach Mage Advisor"})

(defn temporal-nimbus-draw-extra [game _]
  (assoc-in game [:nemesis :draw-extra?] true))

(effects/register {::temporal-nimbus-draw-extra temporal-nimbus-draw-extra})

(def temporal-nimbus {:name       :temporal-nimbus
                      :type       :power
                      :tier       1
                      :to-discard {:text      "Spend 6 Aether."
                                   :predicate [::power/can-afford? {:amount 6}]
                                   :effects   [[:pay {:amount 6 :type :discard-power-card}]]}
                      :power      {:power   3
                                   :text    ["Unleash."
                                             "Gate Witch draws an additional card during the nemesis draw phase this turn."]
                                   :effects [[:unleash]
                                             [::temporal-nimbus-draw-extra]]}})

(defn additional-rules [{:keys [difficulty]}]
  ["At the end of the nemesis turn, if Gate Witch has five or more open time gates and there is exactly one nemesis turn order card in the turn order discard pile, it accelerates time:"
   "- Shuffle a nemesis turn order card into the turn order deck."
   (if (#{:expert :extinction} difficulty)
     "- Gate Witch closes three time gates."
     "- Gate Witch closes four time gates.")])

(effects/register-predicates {::additional-rules additional-rules})

(def gate-witch {:name             :gate-witch
                 :level            7
                 :life             70
                 :unleash          [[::open-gate]]
                 :unleash-text     "Gate Witch opens one time gate."
                 :additional-rules ::additional-rules
                 :at-end-turn      [[::at-end-turn]]
                 :cards            [deep-abomination hasten temporal-nimbus
                                    (power/generic 2) (minion/generic 2 1) (minion/generic 2 2)
                                    (attack/generic 3) (power/generic 3) (minion/generic 3)]
                 :time-gates       1})
