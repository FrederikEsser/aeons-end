(ns aeons-end.nemeses.gate-witch
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.power :as power]))

(defn open-gate [game _]
  (update-in game [:nemesis :time-gates] ut/plus 1))

(defn close-gates [{:keys [nemesis] :as game} {:keys [arg]}]
  (let [{:keys [time-gates]} nemesis]
    (assoc-in game [:nemesis :time-gates] (max (- time-gates arg) 0))))

(defn accelerate-time [{:keys [difficulty] :as game} _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)
        accelerate-time?        (and (<= 5 (get-in game [:nemesis :time-gates]))
                                     (= 1 discarded-nemesis-cards))]
    (cond-> game
            accelerate-time? (push-effect-stack {:effects [[:give-choice {:title   "Gate Witch accelerates time!"
                                                                          :text    "Shuffle a nemesis turn order card into the turn order deck."
                                                                          :effect  :shuffle-into-turn-order-deck
                                                                          :options [:turn-order :discard {:type :nemesis}]
                                                                          :min     1
                                                                          :max     1}]
                                                           [::close-gates (if (#{:expert :extinction} difficulty) 3 4)]]}))))

(defn at-end-turn [{:keys [nemesis] :as game} _]
  (let [{:keys [draw-extra?]} nemesis]
    (-> game
        (update :nemesis dissoc :draw-extra?)
        (push-effect-stack {:effects (concat (when draw-extra?
                                               [[:draw-nemesis-card]])
                                             [[::accelerate-time]])}))))

(effects/register {::open-gate       open-gate
                   ::close-gates     close-gates
                   ::accelerate-time accelerate-time
                   ::at-end-turn     at-end-turn})

(defn return-nemesis-card [game {:keys [card-name]}]
  (push-effect-stack game {:effects [[:reset-nemesis-card {:card-name card-name}]
                                     [:move-card {:card-name card-name
                                                  :from      :play-area
                                                  :to        :deck}]]}))

(effects/register {::return-nemesis-card return-nemesis-card})

(defn- get-damage-effect [{{:keys [player-no player-nos] :as type} :type}]
  (cond
    (= :wild type) [:give-choice {:title   :deep-abomination
                                  :text    "Any player suffers 1 damage."
                                  :effect  [:damage-player {:arg 1}]
                                  :options [:players]
                                  :min     1
                                  :max     1}]
    player-no [:damage-player {:player-no player-no
                               :arg       1}]
    player-nos [:give-choice {:title   :deep-abomination
                              :text    (str "Player "
                                            (case player-nos
                                              #{0 1} "1 or 2"
                                              #{2 3} "3 or 4")
                                            " suffers 1 damage.")
                              :effect  [:damage-player {:arg 1}]
                              :options [:players {:player-nos player-nos}]
                              :min     1
                              :max     1}]))

(defn deep-abomination-damage [game _]
  (let [turn-order-cards        (->> (get-in game [:turn-order :discard])
                                     (remove (comp #{:nemesis} :type)))
        duplicate-4p-player-nos (->> turn-order-cards
                                     (keep (comp :player-nos :type))
                                     frequencies
                                     (keep (fn [[k v]] (when (= 2 v) k)))
                                     set)
        damage-effects          (->> turn-order-cards
                                     (remove (comp duplicate-4p-player-nos :player-nos :type))
                                     (concat (for [player-no (apply concat duplicate-4p-player-nos)]
                                               {:type {:player-no player-no}}))
                                     (sort-by (comp #{:wild} :type))
                                     (map get-damage-effect))]
    (cond-> game
            (not-empty damage-effects) (push-effect-stack {:effects damage-effects}))))

(effects/register {::deep-abomination-damage deep-abomination-damage})

(def deep-abomination {:name       :deep-abomination
                       :type       :minion
                       :tier       1
                       :life       6
                       :persistent {:text    "Each player suffers 1 damage for each turn order card that player has in the turn order discard pile."
                                    :effects [[::deep-abomination-damage]]}
                       :quote      "'It is said that each abomination is but the same creature from across all worlds twisted into a single form.'"})

(defn distort-damage [game _]
  (let [{:keys [type]} (-> game :turn-order :deck first)]
    (push-effect-stack game {:effects (if (= :nemesis type)
                                        [[:damage-gravehold 5]]
                                        [[:draw-turn-order]])})))

(effects/register {::distort-damage distort-damage})

(def distort {:name    :distort
              :type    :attack
              :tier    3
              :text    ["Unleash"
                        "Reveal the top card of the turn order deck. If a player turn order card is revealed, discard it. Otherwise, Gravehold suffers 5 damage."
                        "Place this card on the bottom of the nemesis deck."]
              :effects [[:unleash]
                        [:reveal-top-turn-order]
                        [::distort-damage]
                        [::return-nemesis-card {:card-name :distort}]]
              :quote   "'Being pushed out of time is a painful ordeal even for me.' Remnant, Aetherial Entity"})

(defn hasten-damage [game _]
  (let [damage (get-in game [:nemesis :time-gates])]
    (push-effect-stack game {:effects [[:give-choice {:title   :hasten
                                                      :text    (str "Any player suffers " damage " damage.")
                                                      :effect  [:damage-player {:arg damage}]
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::hasten-damage hasten-damage})

(def hasten {:name    :hasten
             :type    :attack
             :tier    1
             :text    ["Any player suffers damage equal to the number of open time gates."
                       "Unleash."
                       "Place this card on the bottom of the nemesis deck."]
             :effects [[::hasten-damage]
                       [:unleash]
                       [::return-nemesis-card {:card-name :hasten}]]
             :quote   "'Time is The Witch's domain, the one thing we have so little left.' Gex, Breach Mage Advisor"})

(defn infinite-enmity-unleash [game _]
  (update-in game [:nemesis :time-gates] max 6))

(effects/register {::infinite-enmity-unleash infinite-enmity-unleash})

(def infinite-enmity {:name       :infinite-enmity
                      :type       :power
                      :tier       3
                      :to-discard {:text      "Spend 8 Aether."
                                   :predicate [::power/can-afford? {:amount 8}]
                                   :effects   [[:pay {:amount 8 :type :discard-power-card}]]}
                      :power      {:power   2
                                   :text    ["Unleash until Gate Witch has six open time gates."
                                             "Place this card on the bottom of the nemesis deck."]
                                   :effects [[::infinite-enmity-unleash]
                                             [::return-nemesis-card {:card-name :infinite-enmity}]]}
                      :quote      "'Countless times we have won. And countless more we have lost.' Mazahaedron, Henge Mystic"})

(defn nether-spiral-shuffle [{:keys [turn-order] :as game} {:keys [card-name]}]
  (let [{:keys [card idx]} (ut/get-card-idx game [:turn-order :discard] {:name card-name})
        {:keys [deck discard]} turn-order]
    (-> game
        (assoc :turn-order (merge {:deck (->> (concat deck
                                                      (cond-> discard
                                                              idx (ut/vec-remove idx)))
                                              shuffle
                                              vec)}
                                  (when card
                                    {:discard [card]}))))))

(effects/register {::nether-spiral-shuffle nether-spiral-shuffle})

(def nether-spiral {:name       :nether-spiral
                    :type       :power
                    :tier       2
                    :to-discard {:text      "Spend 7 Aether."
                                 :predicate [::power/can-afford? {:amount 7}]
                                 :effects   [[:pay {:amount 7 :type :discard-power-card}]]}
                    :power      {:power   3
                                 :text    "Choose any player's turn order card in the turn order discard pile. Shuffle the rest of the turn order cards into the turn order deck."
                                 :effects [[:give-choice {:title      :nether-spiral
                                                          :text       "Choose any player's turn order card in the turn order discard pile. Shuffle the rest of the turn order cards into the turn order deck."
                                                          :effect     ::nether-spiral-shuffle
                                                          :options    [:turn-order :discard {:not-type :nemesis}]
                                                          :min        1
                                                          :max        1
                                                          :mandatory? true}]]}
                    :quote      "'Within the void, there are infinite spirals of worlds. Some are unborn, others are thriving. But there is a dead light in some that have been claimed by The Nameless.' Yan Magda, Enlightened Exile"})

(defn paradox-beast-damage [game _]
  (let [damage (get-in game [:nemesis :time-gates])]
    (push-effect-stack game {:effects [[:damage-gravehold damage]]})))

(effects/register {::paradox-beast-damage paradox-beast-damage})

(def paradox-beast {:name       :paradox-beast
                    :type       :minion
                    :tier       2
                    :life       8
                    :persistent {:text    "Gravehold suffers damage equal to the number of open time gates."
                                 :effects [[::paradox-beast-damage]]}
                    :quote      "'It moves between time and space, too quick for the eye and harder yet to strike.' Remnant, Aetherial Entity"})

(def portal-wretch {:name        :portal-wretch
                    :type        :minion
                    :tier        2
                    :life        8
                    :text        "When this minion is discarded from play, shuffle a nemesis turn order card into the turn order deck."
                    :when-killed [[:give-choice {:title   :portal-wretch
                                                 :text    "Shuffle a nemesis turn order card into the turn order deck."
                                                 :effect  :shuffle-into-turn-order-deck
                                                 :options [:turn-order :discard {:type :nemesis}]
                                                 :min     1
                                                 :max     1}]]
                    :persistent  {:text    "The player with the most charges suffers 2 damage."
                                  :effects [[:give-choice {:title   :portal-wretch
                                                           :text    "The player with the most charges suffers 2 damage."
                                                           :effect  [:damage-player {:arg 2}]
                                                           :options [:players {:most-charges true}]
                                                           :min     1
                                                           :max     1}]]}
                    :quote       "'It is said that each abomination is but the same creature from across all worlds twisted into a single form.'"})

(def rift-scourge {:name        :rift-scourge
                   :type        :minion
                   :tier        3
                   :life        13
                   :text        "When this minion is discarded from play or the nemesis deck, place it on the bottom of the nemesis deck."
                   :when-killed [[::return-nemesis-card {:card-name :rift-scourge}]]
                   :persistent  {:text    "Any player suffers 4 damage."
                                 :effects [[:give-choice {:title   :rift-scourge
                                                          :text    "Any player suffers 4 damage."
                                                          :effect  [:damage-player {:arg 4}]
                                                          :options [:players]
                                                          :min     1
                                                          :max     1}]]}
                   :quote       "'It lives outside of time and therefore cannot be slain.' Garu, Oathsworn Protector"})

(defn temporal-nimbus-draw-extra [game _]
  (assoc-in game [:nemesis :draw-extra?] true))

(effects/register {::temporal-nimbus-draw-extra temporal-nimbus-draw-extra})

(def temporal-nimbus {:name       :temporal-nimbus
                      :type       :power
                      :tier       1
                      :to-discard {:text      "Spend 6 Aether."
                                   :predicate [::power/can-afford? {:amount 6}]
                                   :effects   [[:pay {:amount 6 :type :discard-power-card}]]}
                      :power      {:power   3
                                   :text    ["Unleash."
                                             "Gate Witch draws an additional card during the nemesis draw phase this turn."]
                                   :effects [[:unleash]
                                             [::temporal-nimbus-draw-extra]]}})

(defn additional-rules [{:keys [difficulty]}]
  ["At the end of the nemesis turn, if Gate Witch has five or more open time gates and there is exactly one nemesis turn order card in the turn order discard pile, it accelerates time:"
   "- Shuffle a nemesis turn order card into the turn order deck."
   (if (#{:expert :extinction} difficulty)
     "- Gate Witch closes three time gates."
     "- Gate Witch closes four time gates.")])

(effects/register-predicates {::additional-rules additional-rules})

(def gate-witch {:name             :gate-witch
                 :level            7
                 :life             70
                 :unleash          [[::open-gate]]
                 :unleash-text     "Gate Witch opens one time gate."
                 :additional-rules ::additional-rules
                 :at-end-turn      [[::at-end-turn]]
                 :cards            [deep-abomination hasten temporal-nimbus
                                    nether-spiral paradox-beast portal-wretch
                                    distort infinite-enmity rift-scourge]
                 :time-gates       1})
