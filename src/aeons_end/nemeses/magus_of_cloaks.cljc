(ns aeons-end.nemeses.magus-of-cloaks
  (:require [aeons-end.operations :refer [push-effect-stack move-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack]
            [aeons-end.cards.power :as power]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.attack :as attack]))

(defn get-min-tokens [{:keys [difficulty]}]
  (if (#{:expert :extinction} difficulty) 3 2))

(defn get-max-tokens [{:keys [difficulty]}]
  (if (#{:expert :extinction} difficulty) 9 8))

(defn gain-nemesis-tokens [{:keys [nemesis] :as game} {:keys [arg]}]
  (assoc-in game [:nemesis :tokens] (min (+ (:tokens nemesis)
                                            arg)
                                         (get-max-tokens game))))

(defn lose-nemesis-token [{:keys [nemesis] :as game} _]
  (assoc-in game [:nemesis :tokens] (max (dec (:tokens nemesis))
                                         (get-min-tokens game))))

(effects/register {::gain-nemesis-tokens gain-nemesis-tokens
                   ::lose-nemesis-token  lose-nemesis-token})

(defn unleash-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :tokens [[::gain-nemesis-tokens 2]]
                                      :damage [[:damage-gravehold 2]])}))

(defn do-unleash [{:keys [nemesis] :as game} args]
  (let [title            (keyword (or (:resolving args)
                                      (:resolving game))
                                  "unleash")
        {:keys [tokens]} nemesis
        can-gain-tokens? (<= tokens (- (get-max-tokens game)
                                       2))]
    (push-effect-stack game {:effects (if can-gain-tokens?
                                        [[:give-choice {:title   title
                                                        :choice  ::unleash-choice
                                                        :options [:special
                                                                  {:option :tokens :text "Magus of Cloaks gains two nemesis tokens"}
                                                                  {:option :damage :text "Gravehold suffers 2 damage"}]
                                                        :min     1
                                                        :max     1}]]
                                        [[:damage-gravehold 2]])})))

(effects/register {::unleash-choice unleash-choice
                   ::unleash        do-unleash})

(defn modify-damage [{:keys [nemesis]} damage]
  (let [{:keys [tokens]} nemesis]
    (max 0 (- damage tokens))))

(effects/register-predicates {::modify-damage modify-damage})

(defn additional-rules [{:keys [difficulty]}]
  ["- When Magus of Cloaks would be dealt damage, reduce that damage by the number of nemesis tokens he has. Then, Magus of Cloaks loses one nemesis token."
   (if (#{:beginner :normal} difficulty)
     "- Magus of Cloaks can never have less than two nemesis tokens or more than eight nemesis tokens."
     "- Magus of Cloaks can never have less than three nemesis tokens or more than nine nemesis tokens.")])

(effects/register-predicates {::additional-rules additional-rules})

(def magus-of-cloaks {:name             :magus-of-cloaks
                      :level            7
                      :life             35
                      :tokens           4
                      :unleash          [[::unleash]]
                      :unleash-text     ["Magus of Cloaks gains two nemesis tokens."
                                         "OR"
                                         "Gravehold suffers 2 damage."]
                      :additional-rules ::additional-rules
                      :modify-damage    ::modify-damage
                      :when-hit         [[::lose-nemesis-token]]
                      :cards            [(minion/generic 1) (power/generic 1 1) (power/generic 1 2)
                                         (minion/generic 2) (power/generic 2) (attack/generic 2)
                                         (attack/generic 3) (power/generic 3) (minion/generic 3)]})
