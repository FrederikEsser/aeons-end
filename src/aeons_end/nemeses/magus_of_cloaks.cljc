(ns aeons-end.nemeses.magus-of-cloaks
  (:require [aeons-end.operations :refer [push-effect-stack move-card]]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack]))

(defn get-min-cloaks [{:keys [difficulty]}]
  (if (#{:expert :extinction} difficulty) 3 2))

(defn get-max-cloaks [{:keys [difficulty]}]
  (if (#{:expert :extinction} difficulty) 9 8))

(defn gain-cloaks [{:keys [nemesis] :as game} {:keys [arg]}]
  (assoc-in game [:nemesis :cloaks] (min (+ (:cloaks nemesis)
                                            arg)
                                         (get-max-cloaks game))))

(defn lose-cloak [{:keys [nemesis] :as game} _]
  (assoc-in game [:nemesis :cloaks] (max (dec (:cloaks nemesis))
                                         (get-min-cloaks game))))

(effects/register {::gain-cloaks gain-cloaks
                   ::lose-cloak  lose-cloak})

(defn unleash-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :cloaks [[::gain-cloaks 2]]
                                      :damage [[:damage-gravehold 2]])}))

(defn do-unleash [{:keys [nemesis] :as game} args]
  (let [title            (keyword (or (:resolving args)
                                      (:resolving game))
                                  "unleash")
        {:keys [cloaks]} nemesis
        can-gain-cloaks? (<= cloaks (- (get-max-cloaks game)
                                       2))]
    (push-effect-stack game {:effects (if can-gain-cloaks?
                                        [[:give-choice {:title   title
                                                        :effect  ::unleash-choice
                                                        :options [:special
                                                                  {:option :cloaks :text "Magus of Cloaks gains two cloaks"}
                                                                  {:option :damage :text "Gravehold suffers 2 damage"}]
                                                        :min     1
                                                        :max     1}]]
                                        [[:damage-gravehold 2]])})))

(effects/register {::unleash-choice unleash-choice
                   ::unleash        do-unleash})

(defn shield [{:keys [nemesis]}]
  (let [{:keys [cloaks]} nemesis]
    cloaks))

(effects/register-predicates {::shield shield})

(defn when-hit [game {:keys [damage]}]
  (let [discard-powers (->> (get-in game [:nemesis :play-area])
                            (filter (comp #{::nemesis-damaged} :to-discard))
                            (map :name))]
    (push-effect-stack game {:effects (concat [[::lose-cloak]]
                                              (when (pos? damage)
                                                (->> discard-powers
                                                     (map (fn [card-name]
                                                            [:discard-nemesis-card {:card-name card-name}])))))})))

(effects/register {::when-hit when-hit})

(defn additional-rules [{:keys [difficulty]}]
  ["- When Magus of Cloaks would be dealt damage, reduce that damage by the number of cloaks he has. Then, Magus of Cloaks loses one cloak."
   (if (#{:beginner :normal} difficulty)
     "- Magus of Cloaks can never have less than two cloaks or more than eight cloaks."
     "- Magus of Cloaks can never have less than three cloaks or more than nine cloaks.")])

(effects/register-predicates {::additional-rules additional-rules})

(defn victory-condition [{:keys [nemesis]}]
  (let [{:keys [final-form?]} nemesis]
    (when final-form?
      {:conclusion :defeat
       :text       "Magus of Cloaks reached its final form. Humanity is wiped out."})))

(effects/register-predicates {::victory-condition victory-condition})

(defn ashen-haruspex-damage [game _]
  (let [damage (-> (get-in game [:nemesis :cloaks])
                   inc
                   (quot 2))]
    (push-effect-stack game {:effects [[:give-choice {:title   :ashen-haruspex
                                                      :text    (str "Any player suffers " damage " damage.")
                                                      :effect  [:damage-player {:arg damage}]
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::ashen-haruspex-damage ashen-haruspex-damage})

(def ashen-haruspex {:name       :ashen-haruspex
                     :type       :minion
                     :tier       2
                     :life       5
                     :text       "When this minion is dealt damage, prevent 2 of that damage."
                     :shield     2
                     :persistent {:text    "Any player suffers damage equal to half the number of cloaks Magus of Cloaks has, rounded up."
                                  :effects [[::ashen-haruspex-damage]]}
                     :quote      "'If any among us can kill the dark itself, it is Quilius.' Garu, Oathsworn Protector"})

(defn black-solstice-damage [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:give-choice {:title   :black-solstice
                                                      :text    "Discard a prepped spell."
                                                      :effect  :discard-prepped-spells
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
                                                      :effect  ::black-solstice-damage
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

(defn dusk-spawn-damage [game {:keys [area player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :ability [[:spend-charges 1]]
                                        :players [[:damage-player 2]])}))

(effects/register {::dusk-spawn-damage dusk-spawn-damage})

(def dusk-spawn {:name       :dusk-spawn
                 :type       :minion
                 :tier       1
                 :life       5
                 :text       "When this minion is dealt damage, Magus of Cloaks gains one cloak."
                 :when-hit   [[::gain-cloaks 1]]
                 :persistent {:text    "Any player loses 1 charge or suffers 2 damage."
                              :effects [[:give-choice {:title   :dusk-spawn
                                                       :text    "Any player loses 1 charge or suffers 2 damage."
                                                       :effect  ::dusk-spawn-damage
                                                       :options [:mixed
                                                                 [:players :ability {:min-charges 1}]
                                                                 [:players]]
                                                       :min     1
                                                       :max     1}]]}
                 :quote      "'It is shadow, given form and cruelty.' Mazahaedron, Henge Mystic"})

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
                                                      :effect  ::eclipse-damage
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
              :text    ["Magus of Cloaks gains five cloaks."
                        "The player with the most expensive prepped spell destroys that spell and suffers 2 damage."]
              :effects [[::gain-cloaks 5]
                        [::eclipse-give-choice]]
              :quote   "'We live in darkness, but they thrive in it.' Mist, Voidwalker"})

(def enshroud {:name    :enshroud
               :type    :attack
               :tier    2
               :text    ["Magus of Cloaks gains four cloaks."
                         "The players collectively destroy two prepped spells that cost 0 Aether."]
               :effects [[::gain-cloaks 4]
                         [:give-choice {:title   :enshroud
                                        :text    "The players collectively destroy two prepped spells that cost 0 Aether."
                                        :effect  :destroy-prepped-spells
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
                                                        :effect  [:damage-player {:arg 3}]
                                                        :options [:players]
                                                        :min     1
                                                        :max     1}]
                                         [:give-choice {:title   :rising-dark
                                                        :text    "Any player destroys a prepped spell that costs 0."
                                                        :effect  :destroy-prepped-spells
                                                        :options [:players :prepped-spells {:cost 0}]
                                                        :min     1
                                                        :max     1}]]}
                  :quote      "'Worlds are born and die in the dark.' Yan Magda, Enlightened Exile"})

(defn reach-final-form [game _]
  (assoc-in game [:nemesis :final-form?] true))

(effects/register {::reach-final-form reach-final-form})

(def shadows-reach {:name        :shadow's-reach
                    :type        :power
                    :tier        3
                    :immediately {:text    "Magus of Cloaks gain four cloaks."
                                  :effects [[::gain-cloaks 4]]}
                    :power       {:power   9
                                  :text    ["Magus of Cloaks reaches its final form. Humanity is wiped out. The players lose the game."]
                                  :effects [[::reach-final-form]]}
                    :quote       "'What little we have shall be smothered in the end.' Nerva, Survivor"})

(defn twilight-empire-damage [game _]
  (let [damage (get-in game [:nemesis :cloaks])]
    (push-effect-stack game {:effects [[:damage-gravehold damage]]})))

(effects/register {::twilight-empire-damage twilight-empire-damage})

(def twilight-empire {:name  :twilight-empire
                      :type  :power
                      :tier  1
                      :power {:power   3
                              :text    ["Gravehold suffers damage equal to the number of cloaks Magus of Cloaks has."
                                        "Then, Magus of Cloaks gains one cloak."]
                              :effects [[::twilight-empire-damage]
                                        [::gain-cloaks {:arg 1}]]}})

(def veil-daughter {:name       :veil-daughter
                    :type       :minion
                    :tier       3
                    :life       11
                    :text       "When this minion is dealt damage, reduce that damage by the number of cloaks Magus of Cloaks has."
                    :shield     ::shield
                    :persistent {:text    "The player with the most charges suffers 4 damage."
                                 :effects [[:give-choice {:title   :veil-daughter
                                                          :text    "The player with the most charges suffers 4 damage."
                                                          :effect  [:damage-player {:arg 4}]
                                                          :options [:players {:most-charges true}]
                                                          :min     1
                                                          :max     1}]]}
                    :quote      "'Where Ulgimor is merely a beast of shadow, the Veil Daughter is a darkling goddess.' Dezmodia, Voidborn Prodigy"})

(def magus-of-cloaks {:name              :magus-of-cloaks
                      :level             6
                      :life              35
                      :cloaks            4
                      :unleash           [[::unleash]]
                      :unleash-text      ["Magus of Cloaks gains two cloaks."
                                          "OR"
                                          "Gravehold suffers 2 damage."]
                      :additional-rules  ::additional-rules
                      :shield            ::shield
                      :when-hit          [[::when-hit]]
                      :victory-condition ::victory-condition
                      :cards             [dusk-spawn rising-dark twilight-empire
                                          ashen-haruspex black-solstice enshroud
                                          eclipse shadows-reach veil-daughter]})
