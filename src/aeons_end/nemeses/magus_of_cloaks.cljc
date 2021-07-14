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

(defn when-hit [game {:keys [damage]}]
  (let [discard-powers (->> (get-in game [:nemesis :play-area])
                            (filter (comp #{::nemesis-damaged} :to-discard))
                            (map :name))]
    (push-effect-stack game {:effects (concat [[::lose-nemesis-token]]
                                              (when (pos? damage)
                                                (->> discard-powers
                                                     (map (fn [card-name]
                                                            [:discard-nemesis-card {:card-name card-name}])))))})))

(effects/register {::when-hit when-hit})

(defn additional-rules [{:keys [difficulty]}]
  ["- When Magus of Cloaks would be dealt damage, reduce that damage by the number of nemesis tokens he has. Then, Magus of Cloaks loses one nemesis token."
   (if (#{:beginner :normal} difficulty)
     "- Magus of Cloaks can never have less than two nemesis tokens or more than eight nemesis tokens."
     "- Magus of Cloaks can never have less than three nemesis tokens or more than nine nemesis tokens.")])

(effects/register-predicates {::additional-rules additional-rules})

(defn ashen-haruspex-modify-damage [_ damage]
  (max (- damage 2) 0))

(effects/register-predicates {::ashen-haruspex-modify-damage ashen-haruspex-modify-damage})

(defn ashen-haruspex-damage [game _]
  (let [damage (-> (get-in game [:nemesis :tokens])
                   inc
                   (quot 2))]
    (push-effect-stack game {:effects [[:give-choice {:title   :ashen-haruspex
                                                      :text    (str "Any player suffers " damage " damage.")
                                                      :choice  [:damage-player {:arg damage}]
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::ashen-haruspex-damage ashen-haruspex-damage})

(def ashen-haruspex {:name          :ashen-haruspex
                     :type          :minion
                     :tier          2
                     :life          5
                     :text          "When this minion is dealt damage, prevent 2 of that damage."
                     :modify-damage ::ashen-haruspex-modify-damage
                     :persistent    {:text    "Any player suffers damage equal to half the number of nemesis tokens Magus of Cloaks has, rounded up."
                                     :effects [[::ashen-haruspex-damage]]}
                     :quote         "'If any among us can kill the dark itself, it is Quilius.' Garu, Oathsworn Protector"})

(def rising-dark {:name       :rising-dark
                  :type       :power
                  :tier       1
                  :text       "When Magus of Cloaks is dealt damage, discard this."
                  :to-discard ::nemesis-damaged
                  :power      {:power   3
                               :text    ["Unleash."
                                         "Any player suffers 3 damage."
                                         "Any player destroys a prepped spell that costs 0."]
                               :effects [[:unleash]
                                         [:give-choice {:title   :rising-dark
                                                        :text    "Any player suffers 3 damage."
                                                        :choice  [:damage-player {:arg 3}]
                                                        :options [:players]
                                                        :min     1
                                                        :max     1}]
                                         [:give-choice {:title   :rising-dark
                                                        :text    "Any player destroys a prepped spell that costs 0."
                                                        :choice  :destroy-prepped-spells
                                                        :options [:players :prepped-spells {:cost 0}]
                                                        :min     1
                                                        :max     1}]]}
                  :quote      "'Worlds are born and die in the dark.' Yan Magda, Enlightened Exile"})

(defn twilight-empire-damage [game _]
  (let [damage (get-in game [:nemesis :tokens])]
    (push-effect-stack game {:effects [[:damage-gravehold damage]]})))

(effects/register {::twilight-empire-damage twilight-empire-damage})

(def twilight-empire {:name  :twilight-empire
                      :type  :power
                      :tier  1
                      :power {:power   3
                              :text    "Gravehold suffers damage equal to the number of nemesis tokens Magus of  Cloaks has. Then, Magus of Cloaks gains one nemesis token."
                              :effects [[::twilight-empire-damage]
                                        [::gain-nemesis-tokens {:arg 1}]]}})

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
                      :when-hit         [[::when-hit]]
                      :cards            [(minion/generic 1) rising-dark twilight-empire
                                         ashen-haruspex (power/generic 2) (attack/generic 2)
                                         (attack/generic 3) (power/generic 3) (minion/generic 3)]})
