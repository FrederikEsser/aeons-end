(ns aeons-end.nemeses.knight-of-shackles
  (:require [aeons-end.operations :refer [push-effect-stack move-card add-card]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn open-breach [game {:keys [breach-no]}]
  (let [{:keys [status effects]} (get-in game [:nemesis :breaches breach-no])]
    (if (= :opened status)
      game
      (-> game
          (assoc-in [:nemesis :breaches breach-no :status] :opened)
          (update-in [:nemesis :breaches breach-no] dissoc :cost :stage)
          (push-effect-stack {:effects effects})))))

(defn focus-breach [game {:keys [breach-no]}]
  (let [{:keys [status stage]} (get-in game [:nemesis :breaches breach-no])]
    (if (= :opened status)
      game
      (if (= 3 stage)
        (push-effect-stack game {:effects [[::open-breach {:breach-no breach-no}]]})
        (update-in game [:nemesis :breaches breach-no :stage] inc)))))

(defn can-unfocus-breach? [game breach-no]
  (let [{:keys [status stage]} (get-in game [:nemesis :breaches breach-no])]
    (and (= :closed status)
         (pos? stage)
         (->> (get-in game [:nemesis :play-area])
              (not-any? (comp #{:deathless-legion} :name))))))

(defn unfocus-breach [game {:keys [breach-no]}]
  (assert (can-unfocus-breach? game breach-no) (str "Unfocus error: Breach " (ut/format-breach-no breach-no) " can't be unfocused."))
  (update-in game [:nemesis :breaches breach-no :stage] dec))

(defn do-unleash [{:keys [nemesis] :as game} _]
  (let [breach-no (->> (:breaches nemesis)
                       (keep-indexed (fn [breach-no {:keys [status]}]
                                       (when (= :closed status)
                                         breach-no)))
                       first)]
    (push-effect-stack game {:effects [[::focus-breach {:breach-no breach-no}]]})))

(effects/register {::open-breach    open-breach
                   ::focus-breach   focus-breach
                   ::unfocus-breach unfocus-breach
                   ::unleash        do-unleash})

(defn chainsworn-damage [game _]
  (let [opened-breaches (->> (get-in game [:nemesis :breaches])
                             (filter (comp #{:opened} :status))
                             count)
        damage          (+ 1 opened-breaches)]
    (push-effect-stack game {:effects [[:give-choice {:title   :chainsworn
                                                      :text    (str "Any player suffers " damage " damage.")
                                                      :effect  [:damage-player {:arg damage}]
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::chainsworn-damage chainsworn-damage})

(def chainsworn {:name        :chainsworn
                 :type        :minion
                 :tier        2
                 :life        10
                 :immediately {:text    "Knight of Shackles focuses its breach III."
                               :effects [[::focus-breach {:breach-no 2}]]}
                 :persistent  {:text    "Any player suffers 1 damage. That player suffers 1 additional damage for each opened breach Knight of Shackles has."
                               :effects [[::chainsworn-damage]]}
                 :quote       "'It strode confidently among the dead upon its blighted steed, the glow of the breach radiating from it as they sieged the city.'"})

(defn deathless-legion-damage [game _]
  (let [opened-breaches (->> (get-in game [:nemesis :breaches])
                             (filter (comp #{:opened} :status))
                             count)
        damage          (+ 1 opened-breaches)]
    (push-effect-stack game {:effects [[:damage-gravehold damage]]})))

(effects/register {::deathless-legion-damage deathless-legion-damage})

(def deathless-legion {:name       :deathless-legion
                       :type       :minion
                       :tier       3
                       :life       18
                       :text       "Players cannot rotate Knight of Shackles's breaches."
                       :persistent {:text    "Gravehold suffers 1 damage. Gravehold suffers 1 additional damage for each opened breach Knight of Shackles has."
                                    :effects [[::deathless-legion-damage]]}})

(defn end-of-all-can-discard? [game {:keys [player-no]}]
  (->> (get-in game [:players player-no :hand])
       (filter (comp #(>= % 3) :cost))
       count
       (<= 3)))

(effects/register-predicates {::end-of-all-can-discard? end-of-all-can-discard?})

(defn end-of-all-open-breach [{:keys [nemesis] :as game} _]
  (let [breach-no (->> (:breaches nemesis)
                       (keep-indexed (fn [breach-no {:keys [status]}]
                                       (when (= :closed status)
                                         breach-no)))
                       first)]
    (push-effect-stack game {:effects [[::open-breach {:breach-no breach-no}]]})))

(effects/register {::end-of-all-open-breach end-of-all-open-breach})

(def end-of-all {:name       :end-of-all
                 :type       :power
                 :tier       3
                 :to-discard {:text      "Destroy three cards in hand that each cost 3 Aether or more."
                              :predicate ::end-of-all-can-discard?
                              :effects   [[:give-choice {:title   :end-of-all
                                                         :text    "Destroy three cards in hand that each cost 3 Aether or more."
                                                         :effect  :destroy-from-hand
                                                         :options [:player :hand {:min-cost 3}]
                                                         :min     3
                                                         :max     3}]]}
                 :power      {:power   2
                              :text    "Knight of Shackles opens its closed breach with the lowest focus cost."
                              :effects [[::end-of-all-open-breach]]}
                 :quote      "'Thraxir was once a breach mage commander, torn asunder by the one called Rageborne. Now he is a harbinger for The Nameless, the end of all.'"})

(defn engine-of-war-can-discard? [game {:keys [player-no]}]
  (let [prepped-spells (->> (get-in game [:players player-no :breaches])
                            (mapcat :prepped-spells)
                            count)]
    (>= prepped-spells 3)))

(effects/register-predicates {::engine-of-war-can-discard? engine-of-war-can-discard?})

(defn engine-of-war-damage [game _]
  (let [opened-breaches (->> (get-in game [:nemesis :breaches])
                             (filter (comp #{:opened} :status))
                             count)
        damage          (+ 1 (* 2 opened-breaches))]
    (push-effect-stack game {:effects [[:give-choice {:title   :engine-of-war
                                                      :text    (str "Any player suffers " damage " damage.")
                                                      :effect  [:damage-player {:arg damage}]
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::engine-of-war-damage engine-of-war-damage})

(def engine-of-war {:name       :engine-of-war
                    :type       :power
                    :tier       2
                    :to-discard {:text      "Discard three prepped spells."
                                 :predicate ::engine-of-war-can-discard?
                                 :effects   [[:give-choice {:title   :engine-of-war
                                                            :text    "Discard three prepped spells."
                                                            :effect  :discard-prepped-spells
                                                            :options [:player :prepped-spells]
                                                            :min     3
                                                            :max     3}]]}
                    :power      {:power   2
                                 :text    ["Any player suffers 1 damage. That player suffers 2 additional damage for each opened breach Knight of Shackles has."
                                           "Unleash."]
                                 :effects [[::engine-of-war-damage]
                                           [:unleash]]}
                    :quote      "'If Thraxir can become one of them, so can I.' Xaxos, Voidbringer"})

(def fellblade {:name       :fellblade
                :type       :minion
                :tier       1
                :life       6
                :persistent {:text    ["Gravehold suffers 3 damage."
                                       "OR"
                                       "Knight of Shackles focuses its breach II."]
                             :effects [[:give-choice {:title   :fellblade
                                                      :text    "Knight of Shackles focuses its breach II."
                                                      :effect  ::focus-breach
                                                      :options [:nemesis :breaches {:breach-no 1 :opened false}]
                                                      :or      {:text    "Gravehold suffers 3 damage."
                                                                :effects [[:damage-gravehold 3]]}
                                                      :max     1}]]}
                :quote      "'Dirt is precious here, so we bury our dead in the darkness of The Depths. Or so we thought...' Ohat, Dirt Merchant"})

(defn invade-reuse-power [game {:keys [arg]}]
  (push-effect-stack game {:effects [[:give-choice {:title   :invade
                                                    :text    (str "Place the "
                                                                  (when (> arg 1)
                                                                    (ut/number->text arg))
                                                                  " most recently discarded power card"
                                                                  (when (> arg 1)
                                                                    "s")
                                                                  " into play.")
                                                    :effect  :reactivate-nemesis-card
                                                    :options [:nemesis :discard {:type :power :most-recent true}]
                                                    :min     1
                                                    :max     1}]]}))
(defn invade-effect [game _]
  (let [{:keys [status]} (get-in game [:nemesis :breaches 1])
        opened-breaches (->> (get-in game [:nemesis :breaches])
                             (filter (comp #{:opened} :status))
                             count)]
    (push-effect-stack game {:effects (if (= :closed status)
                                        [[::open-breach {:breach-no 1}]]
                                        (->> (range 1 (inc opened-breaches))
                                             reverse
                                             (map (fn [n]
                                                    [::invade-reuse-power n]))))})))

(effects/register {::invade-reuse-power invade-reuse-power
                   ::invade-effect      invade-effect})

(def invade {:name    :invade
             :type    :attack
             :tier    3
             :text    ["If Knight of Shackles's breach II is closed, Knight of Shackles opens that breach."
                       "Otherwise, for each opened breach Knight of Shackles has, place the most recently discarded power card into play with the number of power tokens indicated on the card."]
             :effects [[::invade-effect]]})

(defn march-on-gravehold-can-discard? [game {:keys [player-no]}]
  (let [charges (get-in game [:players player-no :ability :charges])]
    (<= 4 charges)))

(effects/register-predicates {::march-on-gravehold-can-discard? march-on-gravehold-can-discard?})

(defn march-on-gravehold-damage [game _]
  (let [{:keys [status]} (get-in game [:nemesis :breaches 0])]
    (push-effect-stack game {:effects (if (= :opened status)
                                        [[:give-choice {:title   :march-on-gravehold
                                                        :text    "Any player suffers 4 damage."
                                                        :effect  [:damage-player {:arg 4}]
                                                        :options [:players]
                                                        :min     1
                                                        :max     1}]]
                                        [[:unleash]
                                         [:unleash]])})))

(effects/register {::march-on-gravehold-damage march-on-gravehold-damage})

(def march-on-gravehold {:name       :march-on-gravehold
                         :type       :power
                         :tier       1
                         :to-discard {:text      "Lose 4 charges."
                                      :predicate ::march-on-gravehold-can-discard?
                                      :effects   [[:spend-charges 4]]}
                         :power      {:power   2
                                      :text    ["If Knight of Shackles's breach I is open, any player suffers 4 damage."
                                                "Otherwise, Unleash twice."]
                                      :effects [[::march-on-gravehold-damage]]}})

(defn rout-damage [game _]
  (let [{:keys [status]} (get-in game [:nemesis :breaches 0])]
    (push-effect-stack game {:effects (if (= :opened status)
                                        [[:damage-gravehold 6]]
                                        [[::open-breach {:breach-no 0}]])})))

(effects/register {::rout-damage rout-damage})

(def rout {:name    :rout
           :type    :attack
           :tier    2
           :text    ["If Knight of Shackles's breach I is open, Gravehold suffers 6 damage."
                     "Otherwise, Knight of Shackles opens its breach I."]
           :effects [[::rout-damage]]
           :quote   "'Mist refuses to see Thraxir within this thing, but its quite clear to the rest of us.' Gex, Breach Mage Advisor"})

(def siege {:name    :siege
            :type    :attack
            :tier    1
            :text    ["Unleash twice."
                      "Any player focuses their breach III."]
            :effects [[:unleash]
                      [:unleash]
                      [:give-choice {:title   :siege
                                     :text    "Any player focuses their breach III."
                                     :effect  :focus-breach
                                     :options [:players :breaches {:breach-no 2 :opened false}]
                                     :min     1
                                     :max     1}]]
            :quote   "'Only a breach mage would know Gravehold so well.' Gex, Breach Mage Advisor"})

(defn setup [{:keys [difficulty] :as game} _]
  (cond-> game
          (#{:expert :extinction} difficulty) (update-in [:nemesis :breaches] (partial mapv (fn [breach]
                                                                                              (update breach :cost inc))))))

(effects/register {::setup setup})

(defn breach-1-damage [game {:keys [player-no player-card-names]}]
  (let [player-numbers (or (some->> player-card-names
                                    (map :player-no))
                           (when player-no
                             [player-no player-no]))]
    (push-effect-stack game {:effects (->> player-numbers
                                           (map (fn [player-no]
                                                  [:damage-player {:player-no player-no :arg 2}])))})))

(effects/register {::breach-1-damage breach-1-damage})

(def breach-1 {:status  :closed
               :cost    3
               :stage   0
               :text    "Two different players each suffer 2 damage."
               :effects [[:give-choice {:title   "Breach I"
                                        :text    "Two different players each suffer 2 damage."
                                        :effect  ::breach-1-damage
                                        :options [:players]
                                        :min     2
                                        :max     2}]]})

(def breach-2 {:status  :closed
               :cost    4
               :stage   0
               :text    "Place the most recently discarded minion in the nemesis discard pile back into play."
               :effects [[:give-choice {:title   "Breach II"
                                        :text    "Place the most recently discarded minion in the nemesis discard pile back into play."
                                        :effect  :reactivate-nemesis-card
                                        :options [:nemesis :discard {:type :minion :most-recent true}]
                                        :min     1
                                        :max     1}]]})

(def breach-3 {:status  :closed
               :cost    5
               :stage   0
               :text    "Gravehold suffers 7 damage."
               :effects [[:damage-gravehold 7]]})

(def breach-4 {:status  :closed
               :cost    6
               :stage   0
               :text    "Knight of Shackles succeeds in summoning its full skeletal legion. Gravehold is overrun and the players lose the game."
               :effects []})

(defn victory-condition [game]
  (let [{:keys [status]} (get-in game [:nemesis :breaches 3])]
    (when (= :opened status)
      {:conclusion :defeat
       :text       "Knight of Shackles succeeds in summoning its full skeletal legion. Gravehold is overrun."})))

(effects/register-predicates {::victory-condition victory-condition})

(def knight-of-shackles {:name              :knight-of-shackles
                         :level             4
                         :life              70
                         :breaches          [breach-1
                                             breach-2
                                             breach-3
                                             breach-4]
                         :setup             [[::setup]]
                         :unleash           [[::unleash]]
                         :unleash-text      "Knight of Shackles focuses its closed breach with the lowest focus cost."
                         :additional-rules  ["- During any player's main phase, that player may spend Aether equal to the focus cost of one of the Knight of Shackles' closed breaches to turn that breach 90?? counterclockwise. Once one of Knight of Shackle's breaches is opened, it will remain open for the rest of the game."
                                             "- When Knight of Shackles opens a breach, resolve the effect listed on that breach."]
                         :victory-condition ::victory-condition
                         :cards             [fellblade march-on-gravehold siege
                                             chainsworn engine-of-war rout
                                             deathless-legion end-of-all invade]})
