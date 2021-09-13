(ns aeons-end.nemeses.knight-of-shackles
  (:require [aeons-end.operations :refer [push-effect-stack move-card add-card]]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack :as attack]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.power :as power]
            [aeons-end.utils :as ut]))

(defn do-unleash [{:keys [nemesis] :as game} _]
  (let [{:keys [breach-no breach]} (->> (:breaches nemesis)
                                        (keep-indexed (fn [breach-no {:keys [status] :as breach}]
                                                        (when (= :closed status)
                                                          {:breach-no breach-no
                                                           :breach    breach})))
                                        first)
        {:keys [stage effects]} breach
        breach-opens? (= 3 stage)
        new-breach    (if breach-opens?
                        (-> breach
                            (assoc :status :opened)
                            (dissoc :cost :stage))
                        (-> breach
                            (update :stage inc)))]
    (cond-> (assoc-in game [:nemesis :breaches breach-no] new-breach)
            breach-opens? (push-effect-stack {:effects effects}))))

(effects/register {::unleash do-unleash})

(defn unfocus-breach [game {:keys [breach-no]}]
  (let [{:keys [status stage]} (get-in game [:nemesis :breaches breach-no])]
    (assert (= :closed status) (str "Unfocus error: Breach " (ut/format-breach-no breach-no) " has status " (ut/format-name status) "."))
    (assert (pos? stage) (str "Unfocus error: Breach " (ut/format-breach-no breach-no) " is in stage " stage "."))
    (update-in game [:nemesis :breaches breach-no :stage] dec)))

(effects/register {::unfocus-breach unfocus-breach})

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
                         :cards             [(minion/generic 1) (power/generic 1) (attack/generic 1)
                                             (minion/generic 2) (power/generic 2) (attack/generic 2)
                                             (minion/generic 3) (power/generic 3) (attack/generic 3)]})
