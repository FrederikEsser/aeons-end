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

(defn at-end-turn [game _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (cond-> game
            (and (<= 5 (get-in game [:nemesis :time-gates]))
                 (= 1 discarded-nemesis-cards)) (accelerate-time {}))))

(effects/register {::open-gate       open-gate
                   ::close-gates     close-gates
                   ::accelerate-time accelerate-time
                   ::at-end-turn     at-end-turn})

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
                 :cards            [(minion/generic 1) (attack/generic 1) (power/generic 1)
                                    (power/generic 2) (minion/generic 2 1) (minion/generic 2 2)
                                    (attack/generic 3) (power/generic 3) (minion/generic 3)]
                 :time-gates       1})
