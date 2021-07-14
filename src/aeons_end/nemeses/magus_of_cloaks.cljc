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

(defn black-solstice-damage [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:give-choice {:title   :black-solstice
                                                      :text    "Discard a prepped spell."
                                                      :choice  :discard-prepped-spells
                                                      :options [:player :prepped-spells]
                                                      :min     1
                                                      :max     1}]]}))

(defn black-solstice-give-choice [{:keys [players] :as game} _]
  (let [prepped-spells? (->> players
                             (mapcat :breaches)
                             (mapcat :prepped-spells)
                             not-empty)]
    (push-effect-stack game {:effects [[:give-choice {:title   :black-solstice
                                                      :text    "Any player suffers 3 damage and discards a prepped spell."
                                                      :choice  ::black-solstice-damage
                                                      :options [:players (when prepped-spells?
                                                                           {:min-number-of-prepped-spells 1})]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::black-solstice-damage      black-solstice-damage
                   ::black-solstice-give-choice black-solstice-give-choice})

(def black-solstice {:name       :black-solstice
                     :type       :power
                     :tier       2
                     :text       "When Magus of Cloaks is dealt damage, discard this."
                     :to-discard ::nemesis-damaged
                     :power      {:power   2
                                  :text    ["Unleash twice."
                                            "Any player suffers 3 damage and discards a prepped spell."]
                                  :effects [[:unleash]
                                            [:unleash]
                                            [::black-solstice-give-choice]]}
                     :quote      "'I could not tell if Ulgimor was afraid or simply angry. Can a shadow feel such things?' Ohat, Dirt Merchant"})

(defn eclipse-damage [game {:keys [player-no breach-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (concat (when card-name
                                                [[:destroy-prepped-spells {:breach-no breach-no
                                                                           :card-name card-name}]])
                                              [[:damage-player 2]])}))

(defn eclipse-give-choice [{:keys [players] :as game} _]
  (let [prepped-spells? (->> players
                             (mapcat :breaches)
                             (mapcat :prepped-spells)
                             not-empty)]
    (push-effect-stack game {:effects [[:give-choice {:title   :eclipse
                                                      :text    "The player with the most expensive prepped spell destroys that spell and suffers 2 damage."
                                                      :choice  ::eclipse-damage
                                                      :options (if prepped-spells?
                                                                 [:players :prepped-spells {:most-expensive true}]
                                                                 [:players])
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::eclipse-damage      eclipse-damage
                   ::eclipse-give-choice eclipse-give-choice})

(def eclipse {:name    :eclipse
              :type    :attack
              :tier    3
              :text    ["Magus of Cloaks gains five nemesis tokens."
                        "The player with the most expensive prepped spell destroys that spell and suffers 2 damage."]
              :effects [[::gain-nemesis-tokens 5]
                        [::eclipse-give-choice]]
              :quote   "'We live in darkness, but they thrive in it.' Mist, Voidwalker"})

(def enshroud {:name    :enshroud
               :type    :attack
               :tier    2
               :text    ["Magus of Cloaks gains four nemesis tokens."
                         "The players collectively destroy two prepped spells that cost 0 Aether."]
               :effects [[::gain-nemesis-tokens 4]
                         [:give-choice {:title   :enshroud
                                        :text    "The players collectively destroy two prepped spells that cost 0 Aether."
                                        :choice  :destroy-prepped-spells
                                        :options [:players :prepped-spells {:cost 0}]
                                        :min     2
                                        :max     2}]]
               :quote   "'The shadows rose and swallowed what little light there was.' Ohat, Dirt Merchant"})

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
                              :text    ["Gravehold suffers damage equal to the number of nemesis tokens Magus of Cloaks has."
                                        "Then, Magus of Cloaks gains one nemesis token."]
                              :effects [[::twilight-empire-damage]
                                        [::gain-nemesis-tokens {:arg 1}]]}})

(def veil-daughter {:name          :veil-daughter
                    :type          :minion
                    :tier          3
                    :life          11
                    :text          "When this minion is dealt damage, reduce that damage by the number of nemesis tokens Magus of Cloaks has."
                    :modify-damage ::modify-damage
                    :persistent    {:text    "The player with the most charges suffers 4 damage."
                                    :effects [[:give-choice {:title   :veil-daughter
                                                             :text    "The player with the most charges suffers 4 damage."
                                                             :choice  [:damage-player {:arg 4}]
                                                             :options [:players {:most-charges true}]
                                                             :min     1
                                                             :max     1}]]}
                    :quote         "'Where Ulgimor is merely a beast of shadow, the Veil Daughter is a darkling goddess.' Dezmodia, Voidborn Prodigy"})

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
                                         ashen-haruspex black-solstice enshroud
                                         eclipse (power/generic 3) veil-daughter]})