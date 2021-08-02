(ns aeons-end.nemeses.carapace-queen
  (:require [aeons-end.operations :refer [push-effect-stack move-card]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn spawn-husks [game {:keys [arg]}]
  (let [{:keys [number-of-husks]} (get-in game [:nemesis :husks])
        new-number-of-husks (+ number-of-husks arg)]
    (-> game
        (assoc-in [:nemesis :husks :number-of-husks] (min 15 new-number-of-husks))
        (cond-> (> new-number-of-husks 15) (push-effect-stack {:effects [[:damage-gravehold (- new-number-of-husks 15)]]})))))

(defn do-unleash [{:keys [difficulty] :as game} _]
  (push-effect-stack game {:effects [[::spawn-husks (if (#{:expert :extinction} difficulty) 3 2)]]}))

(effects/register {::spawn-husks spawn-husks
                   ::unleash     do-unleash})

(defn kill-husks [game {:keys [arg]}]
  (update-in game [:nemesis :husks :number-of-husks] - arg))

(defn get-husk-life [game]
  (if (->> (get-in game [:nemesis :play-area])
           (some (comp #{:foul-multitudes} :name)))
    2
    1))

(defn damage-husks [game {:keys [arg]}]
  (let [{:keys [number-of-husks damaged-husk?]} (get-in game [:nemesis :husks])
        husk-life        (get-husk-life game)
        final-husks-life (-> (* number-of-husks husk-life)
                             (cond-> damaged-husk? (- 1))
                             (- arg)
                             (max 0))]
    (-> game
        (assoc-in [:nemesis :husks :number-of-husks] (+ (quot final-husks-life husk-life)
                                                        (mod final-husks-life husk-life)))
        (update-in [:nemesis :husks] (fn [husks]
                                       (if (pos? (mod final-husks-life husk-life))
                                         (assoc husks :damaged-husk? true)
                                         (dissoc husks :damaged-husk?)))))))

(defn distribute-damage [game {:keys [player-no card-name damage]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:damage-player 1]
                                                    [::damage-husks damage]]})))

(effects/register {::kill-husks        kill-husks
                   ::damage-husks      damage-husks
                   ::distribute-damage distribute-damage})

(defn deal-damage-to-husks [game {:keys [player-no damage]}]
  (let [{:keys [number-of-husks]} (get-in game [:nemesis :husks])
        husk-life (get-husk-life game)]
    (assert (pos? number-of-husks) "Husk error: There are no husks to damage.")
    (let [surplus-damage   (max (- damage husk-life) 0)
          extra-husk-kills (min (+ (quot surplus-damage husk-life)
                                   (mod surplus-damage husk-life))
                                (dec number-of-husks))]
      (cond-> game
              (pos? damage) (push-effect-stack {:player-no player-no
                                                :effects   (concat [[::damage-husks (min damage husk-life)]]
                                                                   (when (pos? extra-husk-kills)
                                                                     [[:give-choice {:title   "Husk damage"
                                                                                     :text    (str "You can kill"
                                                                                                   (when (> husk-life 1)
                                                                                                     "/damage")
                                                                                                   " " extra-husk-kills
                                                                                                   " additional husk" (when (< 1 extra-husk-kills) "s")
                                                                                                   " if you suffer 1 damage.")
                                                                                     :choice  [::distribute-damage {:damage surplus-damage}]
                                                                                     :options [:nemesis :husks]
                                                                                     :max     1}]]))})))))

(defn lookup-swarm-effects [{:keys [number-of-husks swarm-effects]}]
  (->> swarm-effects
       (filter (fn [{:keys [min-husks max-husks]
                     :or   {max-husks 15}}]
                 (<= min-husks number-of-husks max-husks)))
       first))

(defn swarm [game _]
  (let [{:keys [effects]} (-> (get-in game [:nemesis :husks])
                              lookup-swarm-effects)]
    (push-effect-stack game {:effects effects})))

(defn overswarm [game _]
  (assoc-in game [:nemesis :husks :overswarming?] true))

(effects/register {::swarm     swarm
                   ::overswarm overswarm})

(def swarm-effects [{:min-husks 0
                     :max-husks 3
                     :text      "Place four husks into play."
                     :effects   [[::spawn-husks 4]]}
                    {:min-husks 4
                     :max-husks 6
                     :text      "Any player discards a prepped spell. Place two husks into play."
                     :effects   [[:give-choice {:title   :swarm
                                                :text    "Any player discards a prepped spell."
                                                :choice  :discard-prepped-spells
                                                :options [:players :prepped-spells]
                                                :min     1
                                                :max     1}]
                                 [::spawn-husks 2]]}
                    {:min-husks 7
                     :max-husks 10
                     :text      "Gravehold suffers 5 damage."
                     :effects   [[:damage-gravehold 5]]}
                    {:min-husks 11
                     :max-husks 12
                     :text      "The player with the lowest life suffers 3 damage. Discard a husk."
                     :effects   [[:give-choice {:title   :swarm
                                                :text    "The player with the lowest life suffers 3 damage."
                                                :choice  [:damage-player {:arg 3}]
                                                :options [:players {:lowest-life true}]
                                                :min     1
                                                :max     1}]
                                 [::kill-husks 1]]}
                    {:min-husks 13
                     :text      "The players lose."
                     :effects   [[::overswarm]]}])

(defn victory-condition [game]
  (let [{:keys [overswarming?]} (get-in game [:nemesis :husks])]
    (when overswarming?
      {:conclusion :defeat
       :text       "You're overswarmed by the Carapace Queen's hordes of husks."})))

(effects/register-predicates {::victory-condition victory-condition})

(defn blot-the-sun-can-discard? [game {:keys [player-no]}]
  (let [charges (get-in game [:players player-no :ability :charges])]
    (<= 4 charges)))

(effects/register-predicates {::blot-the-sun-can-discard? blot-the-sun-can-discard?})

(def blot-the-sun {:name       :blot-the-sun
                   :type       :power
                   :tier       2
                   :to-discard {:text      "Lose 4 charges."
                                :predicate ::blot-the-sun-can-discard?
                                :effects   [[:spend-charges 4]]}
                   :power      {:power   2
                                :text    ["Carapace Queen Swarms."
                                          "Unleash."
                                          "Carapace Queen Swarms."]
                                :effects [[::swarm]
                                          [:unleash]
                                          [::swarm]]}
                   :quote      "'Were we still in The World That Was, these wretched things would envelop the very sky.' Malastar, Breach Mage Mentor"})

(defn broodwomb-modify-damage [game damage]
  (let [{:keys [number-of-husks]} (get-in game [:nemesis :husks])]
    (if (pos? number-of-husks)
      0
      damage)))

(effects/register-predicates {::broodwomb-modify-damage broodwomb-modify-damage})

(def broodwomb {:name          :broodwomb
                :type          :minion
                :tier          1
                :life          5
                :text          "If there is at least one husk in play, prevent all damage dealt to this minion."
                :modify-damage ::broodwomb-modify-damage
                :persistent    {:text    "Place 1 husk into play."
                                :effects [[::spawn-husks 1]]}
                :quote         "'It is the husk-mother, spewing multitudes through the void into Gravehold.'"})

(def endless-throng {:name    :endless-throng
                     :type    :attack
                     :tier    1
                     :text    "Carapace Queen Swarms."
                     :effects [[::swarm]]
                     :quote   "'The Queen, she is the worst of their foul lot, for her young are without number.' Mist, Breach Mage Dagger Captain"})

(defn clear-damaged-husk [game _]
  (update-in game [:nemesis :husks] dissoc :damaged-husk?))

(effects/register {::clear-damaged-husk clear-damaged-husk})

(def foul-multitudes {:name        :foul-multitudes
                      :type        :power
                      :tier        2
                      :text        ["Husks have 2 life."
                                    "Damaged husks heal to full life at the end of each turn."]
                      :immediately {:text    "Unleash."
                                    :effects [[:unleash]]}
                      :power       {:power   3
                                    :text    "Carapace Queen Swarm."
                                    :effects [[::swarm]]}
                      :at-end-draw [[::clear-damaged-husk]]
                      :quote       "'It took the survivors of Gravehold half-a-moon to shovel up the dead husks and carry them into the depths, where they now lie in piles high enough to touch the top of the cave vaults.'"})

(defn hatch-husks [{:keys [turn-order] :as game} _]
  (let [turn-order-cards (-> turn-order :discard count)]
    (push-effect-stack game {:effects [[::spawn-husks turn-order-cards]]})))

(effects/register {::hatch-husks hatch-husks})

(def hatch {:name    :hatch
            :type    :attack
            :tier    1
            :text    "Place a husk into play for each turn order card in the turn order discard pile."
            :effects [[::hatch-husks]]
            :quote   "'The Queen wears her offspring like some writhing armor'"})

(def infest {:name    :infest
             :type    :attack
             :tier    2
             :text    ["Any player suffers 4 damage."
                       "OR"
                       "Carapace Queen Swarms twice."]
             :effects [[:give-choice {:title     :infest
                                      :text      "Any player suffers 4 damage."
                                      :choice    [:damage-player {:arg 4}]
                                      :options   [:players]
                                      :or-choice {:text    "Carapace Queen Swarms twice."
                                                  :effects [[::swarm]
                                                            [::swarm]]}
                                      :max       1}]]
             :quote   "'Husks are easy enough to dispatch... one on one, that is.' Lash, Breach Mage Scout"})

(defn legion-beacon-damage [game _]
  (let [{:keys [number-of-husks]} (get-in game [:nemesis :husks])
        damage (-> (inc number-of-husks)
                   (quot 2))]
    (push-effect-stack game {:effects [[::kill-husks damage]
                                       [:damage-gravehold (* 2 damage)]]})))

(effects/register {::legion-beacon-damage legion-beacon-damage})

(def legion-beacon {:name        :legion-beacon
                    :type        :power
                    :tier        3
                    :immediately {:text    "Unleash twice."
                                  :effects [[:unleash]
                                            [:unleash]]}
                    :power       {:power   2
                                  :text    ["Discard half of the husks in play, rounded up. Gravehold suffers 2 damage for each husk discarded this way."
                                            "Carapace Queen Swarms."]
                                  :effects [[::legion-beacon-damage]
                                            [::swarm]]}
                    :quote       "'Gravehold knows when the Queen is near, for the claxon call of her legions echoes throughout the cave.'"})

(defn maggot-engine-set-life [game _]
  (let [{:keys [number-of-husks]} (get-in game [:nemesis :husks])]
    (ut/update-in-vec game [:nemesis :play-area] {:name :maggot-engine} assoc :life (* 2 number-of-husks))))

(effects/register {::maggot-engine-set-life maggot-engine-set-life})

(def maggot-engine {:name        :maggot-engine
                    :type        :minion
                    :tier        3
                    :immediately {:text    ["Unleash three times."
                                            "Then, set this minion's life equal to twice the number if husks in play."]
                                  :effects [[:unleash]
                                            [:unleash]
                                            [:unleash]
                                            [::maggot-engine-set-life]]}
                    :persistent  {:text    "Swarm"
                                  :effects [[::swarm]]}
                    :quote       "'Which end is the head?' Nym, Breach Mage Apprentice"})

(def spawn {:name    :spawn
            :type    :attack
            :tier    3
            :text    ["Carapace Queen Swarms."
                      "Unleash."
                      "Carapace Queen Swarms."]
            :effects [[::swarm]
                      [:unleash]
                      [::swarm]]
            :quote   "'The sound of the things, skittering across the cave walls, is loud enough to drown our screams.' Nerva, Survivor"})

(defn unleash-text [{:keys [difficulty]}]
  (if (#{:beginner :normal} difficulty)
    "Place two husks into play."
    "Place three husks into play."))

(effects/register-predicates {::unleash-text unleash-text})

(def carapace-queen {:name              :carapace-queen
                     :level             4
                     :life              60
                     :unleash           [[::unleash]]
                     :unleash-text      ::unleash-text
                     :additional-rules  ["- Each husk is a minion that has 1 life."
                                         "- When a player deals damage to a husk, that player may suffer 1 damage to distribute the damage dealt over multiple husks."
                                         "- When Carapace Queen has fifteen husks in play and must place another into play, Gravehold suffers 1 damage instead."]
                     :victory-condition ::victory-condition
                     :cards             [broodwomb endless-throng hatch
                                         blot-the-sun foul-multitudes infest
                                         legion-beacon maggot-engine spawn]
                     :husks             {:number-of-husks 2
                                         :swarm-effects   swarm-effects}})
