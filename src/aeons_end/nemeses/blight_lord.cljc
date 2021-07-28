(ns aeons-end.nemeses.blight-lord
  (:require [aeons-end.operations :refer [push-effect-stack move-card add-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack]
            [aeons-end.cards.power :as power]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.attack :as attack]))

(defn tainted-jade-on-trash [game {:keys [player-no card-id destroyed-by]}]
  (push-effect-stack game {:effects (concat [[:move-card {:move-card-id card-id
                                                          :from         :trash
                                                          :to           :tainted-jades}]]
                                            (when (not= destroyed-by :tainted-jade)
                                              [[:damage-player {:player-no player-no
                                                                :arg       1}]]))}))

(defn tainted-jade-choice [game {:keys [player-no card-id choice]}]
  (push-effect-stack game {:player-no player-no
                           :card-id   card-id
                           :effects   (case choice
                                        :aether [[:gain-aether 2]
                                                 [:damage-player 1]]
                                        :destroy [[:pay {:amount 2
                                                         :type   :tainted-jade}]
                                                  [:destroy-this {:destroyed-by :tainted-jade}]])}))

(defn tainted-jade-give-choice [game {:keys [player-no card-id]}]
  (let [player (get-in game [:players player-no])]
    (if (ut/can-afford? player 2 :tainted-jade)
      (push-effect-stack game {:player-no player-no
                               :card-id   card-id
                               :effects   [[:give-choice {:title   :tainted-jade
                                                          :choice  ::tainted-jade-choice
                                                          :options [:special
                                                                    {:option :aether :text "Gain 2 Aether. Suffer 1 damage."}
                                                                    {:option :destroy :text "Spend 2 Aether to destroy this."}]
                                                          :min     1
                                                          :max     1}]]})
      (tainted-jade-choice game {:player-no player-no
                                 :choice    :aether}))))

(effects/register {::tainted-jade-on-trash    tainted-jade-on-trash
                   ::tainted-jade-choice      tainted-jade-choice
                   ::tainted-jade-give-choice tainted-jade-give-choice})

(def tainted-jade {:name            :tainted-jade
                   :type            :gem
                   :auto-play-index 2
                   :text            ["Gain 2 Aether. Suffer 1 damage."
                                     "OR"
                                     "Spend 2 Aether to destroy this."
                                     "If you destroy this card in any other way, suffer 1 damage."]
                   :effects         [[::tainted-jade-give-choice]]
                   :on-trash        [[::tainted-jade-on-trash]]})

(defn gain-tainted-jade [game {:keys [player-no to]
                               :or   {to :discard}}]
  (let [[card & tainted-jades] (get-in game [:nemesis :tainted-jades])
        to-position (if (= :deck to)
                      :top
                      :bottom)]
    (-> game
        (assoc-in [:nemesis :tainted-jades] (vec tainted-jades))
        (as-> game
              (if card
                (-> game
                    (add-card [:players player-no to] to-position card)
                    (cond-> (= :deck to) (update-in [:players player-no] dissoc :revealed-cards)))
                (push-effect-stack game {:player-no player-no
                                         :effects   [[:damage-player 1]]}))))))

(defn do-unleash [game args]
  (let [title (keyword (or (:resolving args)
                           (:resolving game))
                       "unleash")]
    (push-effect-stack game {:effects [[:give-choice {:title   title
                                                      :text    "Any player gains a Tainted Jade."
                                                      :choice  ::gain-tainted-jade
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::gain-tainted-jade gain-tainted-jade
                   ::unleash           do-unleash})

(defn setup [{:keys [players difficulty] :as game} _]
  (let [number-of-players (count players)
        number-of-jades   (+ number-of-players
                             (if (#{:beginner :normal} difficulty) 4 2))
        tainted-jades     (->> (repeat number-of-jades tainted-jade)
                               (mapv ut/give-id!))]
    (assoc-in game [:nemesis :tainted-jades] tainted-jades)))

(effects/register {::setup setup})

(defn lookup-next-tainted-effects [{:keys [tainted-level tainted-effects]}]
  (->> tainted-effects
       (filter (fn [{:keys [level]}]
                 (> level tainted-level)))
       first))

(defn lookup-tainted-effects [{:keys [tainted-level tainted-effects]}]
  (->> tainted-effects
       (filter (comp #{tainted-level} :level))
       first))

(defn increase-tainted-level [game _]
  (update-in game [:nemesis :tainted-track :tainted-level] inc))

(defn resolve-tainted-track [game _]
  (let [{:keys [effects]} (-> (get-in game [:nemesis :tainted-track])
                              lookup-tainted-effects)]
    (cond-> game
            effects (push-effect-stack {:effects effects}))))

(defn do-advance-tainted-track [game _]
  (push-effect-stack game {:effects [[::increase-tainted-level]
                                     [:set-resolving {:card-name :tainted-track}]
                                     [::resolve-tainted-track]
                                     [:clear-resolving]]}))

(defn advance-tainted-track [game _]
  (push-effect-stack game {:effects [[:give-choice {:title   :tainted-track
                                                    :text    "Advance the tainted track."
                                                    :choice  ::do-advance-tainted-track
                                                    :options [:nemesis :tainted-track]
                                                    :min     1
                                                    :max     1}]]}))

(defn totally-taint [game _]
  (assoc-in game [:nemesis :tainted-track :totally-tainted?] true))

(effects/register {::increase-tainted-level   increase-tainted-level
                   ::resolve-tainted-track    resolve-tainted-track
                   ::do-advance-tainted-track do-advance-tainted-track
                   ::advance-tainted-track    advance-tainted-track
                   ::totally-taint            totally-taint})

(def tainted-effects [{:level   3
                       :text    "Gravehold suffers 7 damage."
                       :effects [[:damage-gravehold 7]]}
                      {:level   5
                       :text    "The player with the lowest life suffers 4 damage."
                       :effects [[:give-choice {:title   :tainted-track
                                                :text    "The player with the lowest life suffers 4 damage."
                                                :choice  [:damage-player {:arg 4}]
                                                :options [:players {:least-life true}]
                                                :min     1
                                                :max     1}]]}
                      {:level   7
                       :text    "Blight Lord gains 10 life."
                       :effects [[:heal-nemesis 10]]}
                      {:level   9
                       :text    "The players lose."
                       :effects [[::totally-taint]]}])

(defn at-start-turn [game _]
  (let [tainted-jades (->> (get-in game [:nemesis :tainted-jades])
                           count)]
    (cond-> game
            (< tainted-jades 2) (push-effect-stack {:effects (repeat (- 2 tainted-jades) [::advance-tainted-track])}))))

(effects/register {::at-start-turn at-start-turn})

(defn victory-condition [game]
  (let [{:keys [totally-tainted?]} (get-in game [:nemesis :tainted-track])]
    (when totally-tainted?
      {:conclusion :defeat
       :text       "The city of Gravehold is reduced to glass."})))

(effects/register-predicates {::victory-condition victory-condition})

(def creeping-viridian {:name       :creeping-viridian
                        :type       :power
                        :tier       1
                        :to-discard {:text      "Spend 7 Aether."
                                     :predicate [::power/can-afford? {:amount 7}]
                                     :effects   [[:pay {:amount 7
                                                        :type   :discard-power-card}]]}
                        :power      {:power   3
                                     :text    ["Any player gains a Tainted Jade and places it in their hand."
                                               "Advance the Tainted Track."]
                                     :effects [[:give-choice {:title   :creeping-viridian
                                                              :text    "Any player gains a Tainted Jade and places it in their hand."
                                                              :choice  [::gain-tainted-jade {:to :hand}]
                                                              :options [:players]
                                                              :min     1
                                                              :max     1}]
                                               [::advance-tainted-track]]}
                        :quote      "'Just outside the city walls there are men and women, frozen in green horror, like monuments to the Blight Lord's coming.' Z'hana, Breach Mage Renegade"})

(def shard-spitter {:name       :shard-spitter
                    :type       :minion
                    :tier       1
                    :life       5
                    :persistent {:text    ["Gravehold suffers 2 damage."
                                           "Any player gains a Tainted Jade and places it on top of their deck."]
                                 :effects [[:damage-gravehold 2]
                                           [:give-choice {:title   :shard-spitter
                                                          :text    "Any player gains a Tainted Jade and places it on top of their deck."
                                                          :choice  [::gain-tainted-jade {:to :deck}]
                                                          :options [:players]
                                                          :min     1
                                                          :max     1}]]}
                    :quote      "'Renny Mumbast lost both eyes to these wretched things. So now, when the warning bells sound the Blight Lord's call, we wear goggles.' Mist, Breach Mage Dagger Captain"})

(def blight-lord {:name              :blight-lord
                  :level             5
                  :life              70
                  :setup             [[::setup]]
                  :unleash           [[::unleash]]
                  :unleash-text      "Any player gains a Tainted Jade."
                  :additional-rules  ["- When a Tainted Jade is destroyed, return it to the Tainted Jade supply pile."
                                      "- When a player would gain a Tainted Jade and that supply pile is empty, that player suffers 1 damage instead."
                                      "- At the start of each nemesis turn before resolving any other effects, count the number of Tainted Jades in the Tainted Jade supply pile. If there is only one, advance the Tainted Track once. If there are zero, advance the Tainted Track twice."
                                      "- When the Tainted Track is advanced to a space with an effect listed on it, resolve that effect immediately."]
                  :victory-condition ::victory-condition
                  :at-start-turn     [[::at-start-turn]]
                  :tainted-track     {:tainted-level   1
                                      :tainted-effects tainted-effects}
                  :cards             [creeping-viridian shard-spitter (attack/generic 1)
                                      (power/generic 2) (attack/generic 2) (minion/generic 2)
                                      (power/generic 3) (attack/generic 3) (minion/generic 3)]})
