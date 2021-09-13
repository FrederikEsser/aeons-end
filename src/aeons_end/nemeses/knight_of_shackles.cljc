(ns aeons-end.nemeses.knight-of-shackles
  (:require [aeons-end.operations :refer [push-effect-stack move-card add-card]]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack :as attack]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.power :as power]
            [aeons-end.utils :as ut]))

(defn focus-breach [game {:keys [breach-no]}]
  (let [{:keys [stage effects] :as breach} (get-in game [:nemesis :breaches breach-no])
        breach-opens? (= 3 stage)
        new-breach    (if breach-opens?
                        (-> breach
                            (assoc :status :opened)
                            (dissoc :cost :stage))
                        (-> breach
                            (update :stage inc)))]
    (cond-> (assoc-in game [:nemesis :breaches breach-no] new-breach)
            breach-opens? (push-effect-stack {:effects effects}))))

(defn unfocus-breach [game {:keys [breach-no]}]
  (let [{:keys [status stage]} (get-in game [:nemesis :breaches breach-no])]
    (assert (= :closed status) (str "Unfocus error: Breach " (ut/format-breach-no breach-no) " has status " (ut/format-name status) "."))
    (assert (pos? stage) (str "Unfocus error: Breach " (ut/format-breach-no breach-no) " is in stage " stage "."))
    (update-in game [:nemesis :breaches breach-no :stage] dec)))

(defn do-unleash [{:keys [nemesis] :as game} _]
  (let [breach-no (->> (:breaches nemesis)
                       (keep-indexed (fn [breach-no {:keys [status]}]
                                       (when (= :closed status)
                                         breach-no)))
                       first)]
    (push-effect-stack game {:effects [[::focus-breach {:breach-no breach-no}]]})))

(effects/register {::focus-breach   focus-breach
                   ::unfocus-breach unfocus-breach
                   ::unleash        do-unleash})

(def fellblade {:name       :fellblade
                :type       :minion
                :tier       1
                :life       6
                :persistent {:text    ["Gravehold suffers 3 damage."
                                       "OR"
                                       "Knight of Shackles focuses its breach II."]
                             :effects [[:give-choice {:title     :fellblade
                                                      :text      "Knight of Shackles focuses its breach II."
                                                      :choice    ::focus-breach
                                                      :options   [:nemesis :breaches {:breach-no 1 :opened false}]
                                                      :or-choice {:text    "Gravehold suffers 3 damage."
                                                                  :effects [[:damage-gravehold 3]]}
                                                      :max       1}]]}
                :quote      "'Dirt is precious here, so we bury our dead in the darkness of The Depths. Or so we thought...' Ohat, Dirt Merchant"})

(defn march-on-gravehold-can-discard? [game {:keys [player-no]}]
  (let [charges (get-in game [:players player-no :ability :charges])]
    (<= 4 charges)))

(effects/register-predicates {::march-on-gravehold-can-discard? march-on-gravehold-can-discard?})

(defn march-on-gravehold-damage [game _]
  (let [{:keys [status]} (get-in game [:nemesis :breaches 0])]
    (push-effect-stack game {:effects (if (= :opened status)
                                        [[:give-choice {:title   :march-on-gravehold
                                                        :text    "Any player suffers 4 damage."
                                                        :choice  [:damage-player {:arg 4}]
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

(def siege {:name    :siege
            :type    :attack
            :tier    1
            :text    ["Unleash twice."
                      "Any player focuses their breach III."]
            :effects [[:unleash]
                      [:unleash]
                      [:give-choice {:title   :siege
                                     :text    "Any player focuses their breach III."
                                     :choice  :focus-breach
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
                                        :choice  ::breach-1-damage
                                        :options [:players]
                                        :min     2
                                        :max     2}]]})

(def breach-2 {:status  :closed
               :cost    4
               :stage   0
               :text    "Place the most recently discarded minion in the nemesis discard pile back into play."
               :effects [[:give-choice {:title   "Breach II"
                                        :text    "Place the most recently discarded minion in the nemesis discard pile back into play."
                                        :choice  :revive-minion
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
                         :additional-rules  ["- During any player's main phase, that player may spend Aether equal to the focus cost of one of the Knight of Shackles' closed breaches to turn that breach 90° counterclockwise. Once one of Knight of Shackle's breaches is opened, it will remain open for the rest of the game."
                                             "- When Knight of Shackles opens a breach, resolve the effect listed on that breach."]
                         :victory-condition ::victory-condition
                         :cards             [fellblade march-on-gravehold siege
                                             (minion/generic 2) (power/generic 2) (attack/generic 2)
                                             (minion/generic 3) (power/generic 3) (attack/generic 3)]})
