(ns aeons-end.nemeses.umbra-titan
  (:require [aeons-end.operations :refer [push-effect-stack move-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack]
            [aeons-end.cards.power :as power]))

(defn destroy-gravehold-pillars [game {:keys [arg]}]
  (update-in game [:gravehold :pillars] - arg))

(effects/register {::destroy-gravehold-pillars destroy-gravehold-pillars})

(defn crumble-revive-minion [game {:keys [card-name]}]
  (-> game
      (push-effect-stack {:effects [[:revive-minion {:card-name card-name}]
                                    [:unleash]]})))

(effects/register {::crumble-revive-minion crumble-revive-minion})

(def crumble {:name    :crumble
              :type    :attack
              :tier    3
              :text    ["Place the most recently discarded minion in the nemesis discard pile back into play."
                        "Unleash."
                        "OR"
                        "Umbra Titan destroys three pillars of Gravehold."]
              :effects [[:give-choice {:title     :crumble
                                       :text      "Place the most recently discarded minion in the nemesis discard pile back into play.\nUnleash."
                                       :choice    ::crumble-revive-minion
                                       :or-choice {:text    "Umbra Titan destroys three pillars of Gravehold."
                                                   :effects [[::destroy-gravehold-pillars 3]]}
                                       :options   [:nemesis :discard {:type :minion :most-recent true}]
                                       :max       1}]]})

(def cryptid {:name       :cryptid
              :type       :minion
              :tier       1
              :life       6
              :persistent {:text    ["The player with the most expensive prepped spell discards that spell."
                                     "OR"
                                     "The Cryptid destroy one pillar of Gravehold."]
                           :effects [[:give-choice {:title     :cryptid
                                                    :text      "The player with the most expensive prepped spell discards that spell."
                                                    :choice    :discard-prepped-spells
                                                    :or-choice {:text    "The Cryptid destroy one pillar of Gravehold"
                                                                :effects [[::destroy-gravehold-pillars 1]]}
                                                    :options   [:players :prepped-spells {:most-expensive true}]
                                                    :max       1}]]}
              :quote      "'The beasts of this cave seem to revere the Titan as though it were some ancient god.' Mazhaedron, Henge Mystic"})

(defn grubber-persistent [game _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (push-effect-stack game {:effects (if (= 2 discarded-nemesis-cards)
                                        [[::destroy-gravehold-pillars 1]]
                                        [[:damage-gravehold 2]])})))

(effects/register {::grubber-persistent grubber-persistent})

(def grubber {:name       :grubber
              :type       :minion
              :tier       1
              :life       5
              :persistent {:text    ["If the nemesis has two turn order cards in the turn order discard pile, Grubber destroys one pillar of Gravehold."
                                     "Otherwise, Gravehold suffers 2 damage."]
                           :effects [[::grubber-persistent]]}
              :quote      "'Kick it, stab it, burn it... the thing just keeps grinning.' Sparrow, Breach Mage Soldier"})

(defn maul-choice [{:keys [players] :as game} {:keys [choice]}]
  (case choice
    :pillars (push-effect-stack game {:effects [[::destroy-gravehold-pillars 2]]})
    :spells (let [sorted-spells (->> players
                                     (map-indexed (fn [player-no {:keys [breaches]}]
                                                    (->> breaches
                                                         (map-indexed (fn [breach-no breach]
                                                                        (->> (:prepped-spells breach)
                                                                             (map (fn [{:keys [name cost]}]
                                                                                    {:player-no player-no
                                                                                     :breach-no breach-no
                                                                                     :card-name name
                                                                                     :cost      cost})))))
                                                         (apply concat))))
                                     (apply concat)
                                     (sort-by :cost >))
                  [_ cost-2] (map :cost sorted-spells)]
              (push-effect-stack game {:effects (concat
                                                  [[:destroy-prepped-spells (first sorted-spells)]
                                                   [:give-choice {:title   :maul
                                                                  :text    "The players collectively destroy the most expensive prepped spell."
                                                                  :choice  :destroy-prepped-spells
                                                                  :options [:players :prepped-spells {:min-cost cost-2}]
                                                                  :min     1
                                                                  :max     1}]])}))))

(defn maul-give-choice [{:keys [players] :as game} _]
  (let [[cost-1 cost-2 cost-3] (->> players
                                    (mapcat :breaches)
                                    (mapcat :prepped-spells)
                                    (map :cost)
                                    (sort >))]
    (push-effect-stack game {:effects (cond
                                        (nil? cost-2) [[::destroy-gravehold-pillars 2]]
                                        (or (= cost-1 cost-2)
                                            (not= cost-2 cost-3)) [[:give-choice {:title     :maul
                                                                                  :text      "The players collectively destroy the two most expensive prepped spells."
                                                                                  :choice    :destroy-prepped-spells
                                                                                  :or-choice {:text    "Umbra Titan destroys two pillars of Gravehold."
                                                                                              :effects [[::destroy-gravehold-pillars 2]]}
                                                                                  :options   [:players :prepped-spells {:min-cost cost-2}]
                                                                                  :min       2
                                                                                  :max       2
                                                                                  :optional? true}]]
                                        :else [[:give-choice {:title   :maul
                                                              :choice  ::maul-choice
                                                              :options [:special
                                                                        {:option :spells :text "The players collectively destroy the two most expensive prepped spells"}
                                                                        {:option :pillars :text "Umbra Titan destroys two pillars of Gravehold"}]
                                                              :min     1
                                                              :max     1}]])})))

(effects/register {::maul-choice      maul-choice
                   ::maul-give-choice maul-give-choice})

(def maul {:name    :maul
           :type    :attack
           :tier    2
           :text    ["The players collectively destroy the two most expensive prepped spells."
                     "OR"
                     "Umbra Titan destroys two pillars of Gravehold."]
           :effects [[::maul-give-choice]]})

(def seismic-roar {:name       :seismic-roar
                   :type       :power
                   :tier       1
                   :to-discard {:text      "Spend 6 Aether."
                                :predicate [::power/can-afford? {:amount 6}]
                                :effects   [[:pay {:amount 6
                                                   :type   :discard-power-card}]]}
                   :power      {:power   3
                                :text    "Umbra Titan destroys two pillars of Gravehold."
                                :effects [[::destroy-gravehold-pillars 2]]}
                   :quote      "'I roared as it bore through the rock. And Gravehold shuddered like those within its walls.' Nerva, Survivor"})

(defn tombfright-persistent [game _]
  (let [pillars (get-in game [:gravehold :pillars])]
    (push-effect-stack game {:effects (if (>= pillars 5)
                                        [[::destroy-gravehold-pillars 1]]
                                        [[:damage-gravehold 3]])})))

(effects/register {::tombfright-persistent tombfright-persistent})

(def tombfright {:name       :tombfright
                 :type       :minion
                 :tier       2
                 :life       8
                 :persistent {:text    ["If Gravehold has 5 or more pillars, Tombfright destroys one pillar."
                                        "Otherwise, Gravehold suffers 3 damage."]
                              :effects [[::tombfright-persistent]]}
                 :quote      "'With two heads, it's twice as eager to make a meal of a mage.' Sparrow, Breach Mage Soldier"})

(defn vault-behemoth-destroy-pillar [game _]
  (let [{{:keys [life]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name :vault-behemoth})]
    (cond-> game
            (<= life 8) (push-effect-stack {:effects [[::destroy-gravehold-pillars 1]]}))))

(effects/register {::vault-behemoth-destroy-pillar vault-behemoth-destroy-pillar})

(def vault-behemoth {:name       :vault-behemoth
                     :type       :minion
                     :tier       2
                     :life       9
                     :persistent {:text    ["Any player suffers 2 damage."
                                            "If this minion has 8 or less life, Vault Behemoth destroys one pillar of Gravehold."]
                                  :effects [[:give-choice {:title   :vault-behemoth
                                                           :text    "Any player suffers 2 damage."
                                                           :choice  [:damage-player {:arg 2}]
                                                           :options [:players]
                                                           :min     1
                                                           :max     1}]
                                            [::vault-behemoth-destroy-pillar]]}
                     :quote      "'The air rasping in its massive lungs is enough to burst your eardrums.'"})

(def demi-ancient {:name       :demi-ancient
                   :type       :minion
                   :tier       3
                   :life       18
                   :persistent {:text    "Demi-Ancient destroys one pillar of Gravehold."
                                :effects [[::destroy-gravehold-pillars 1]]}
                   :quote      "'In the time before ours, their kind thrived in the tumult of the fledgling world. Now, they seek a new home among The Nameless.' Mazahaedron, Henge Mystic"})

(def yawning-black {:name       :yawning-black
                    :type       :power
                    :tier       3
                    :to-discard {:text      "Spend 8 Aether."
                                 :predicate [::power/can-afford? {:amount 8}]
                                 :effects   [[:pay {:amount 8
                                                    :type   :discard-power-card}]]}
                    :power      {:power   2
                                 :text    ["Any player suffers 6 damage."
                                           "OR"
                                           "Umbra Titan destroys three pillars of Gravehold."]
                                 :effects [[:give-choice {:title     :yawning-black
                                                          :text      "Any player suffers 6 damage."
                                                          :choice    [:damage-player {:arg 6}]
                                                          :or-choice {:text    "Umbra Titan destroys three pillars of Gravehold."
                                                                      :effects [[::destroy-gravehold-pillars 3]]}
                                                          :options   [:players]
                                                          :max       1}]]}
                    :quote      "The Titan hides beneath the skin of the cave, emerging only to strike."})

(defn setup [{:keys [difficulty] :as game} _]
  (assoc-in game [:gravehold :pillars] (if (#{:expert :extinction} difficulty) 5 8)))

(effects/register {::setup setup})

(defn unleash-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :damage [[:damage-gravehold 2]]
                                      :pillar [[::destroy-gravehold-pillars 1]])}))

(defn do-unleash [game args]
  (let [title                   (keyword (or (:resolving args)
                                             (:resolving game))
                                         "unleash")
        discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (assert (<= 1 discarded-nemesis-cards 2) (str "Turn order error: There are " discarded-nemesis-cards " nemesis turn order cards in the turn order discard pile."))
    (push-effect-stack game {:effects [[:give-choice (case discarded-nemesis-cards
                                                       1 {:title     title
                                                          :text      "Any player suffers 2 damage."
                                                          :choice    [:damage-player {:arg 2}]
                                                          :or-choice {:text    "Umbra Titan destroys one pillar of Gravehold"
                                                                      :effects [[::destroy-gravehold-pillars 1]]}
                                                          :options   [:players]
                                                          :max       1}
                                                       2 {:title   title
                                                          :choice  ::unleash-choice
                                                          :options [:special
                                                                    {:option :damage :text "Gravehold suffers 2 damage"}
                                                                    {:option :pillar :text "Umbra Titan destroys one pillar of Gravehold"}]
                                                          :min     1
                                                          :max     1})]]})))

(effects/register {::unleash-choice unleash-choice
                   ::unleash        do-unleash})

(defn victory-condition [{:keys [real-game?] :as game}]
  (let [pillars (get-in game [:gravehold :pillars])]
    (when (and real-game?
               pillars
               (<= pillars 0))
      {:conclusion :defeat
       :text       "Gravehold's foundation is undermined. Gravehold collapses into rubble."})))

(effects/register-predicates {::victory-condition victory-condition})

(defn unleash-text [{:keys [current-player] :as game}]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    [(case (-> discarded-nemesis-cards
               (cond-> (int? current-player) inc)
               (mod 2))
       0 "Gravehold [alt. any player] suffers 2 damage"
       1 "Any player [alt. Gravehold] suffers 2 damage.")
     "OR"
     "Umbra Titan destroys one pillar of Gravehold."]))

(effects/register-predicates {::unleash-text unleash-text})

(def umbra-titan {:name              :umbra-titan
                  :level             3
                  :life              70
                  :setup             [[::setup]]
                  :unleash           [[::unleash]]
                  :unleash-text      ::unleash-text
                  :additional-rules  ["When Gravehold has zero pillars left, its foundation is undermined. Gravehold collapses into rubble and the players lose."]
                  :victory-condition ::victory-condition
                  :cards             [cryptid grubber seismic-roar
                                      maul tombfright vault-behemoth
                                      crumble demi-ancient yawning-black]})
