(ns aeons-end.front-end-view
  (:require [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.operations :refer [can-discard?]]
            [aeons-end.nemeses.carapace-queen :refer [lookup-swarm-effects]]
            [aeons-end.nemeses.blight-lord :refer [lookup-tainted-effects lookup-next-tainted-effects]]
            [clojure.string :as string]
            [aeons-end.operations :as op]
            [medley.core :as medley]))

(defn choice-interaction [{:keys [area player-no breach-no card-name card-id]}
                          {:keys [options max choice-opts] :as choice}]
  (let [choice-value (cond->> options
                              (not= area (:area choice)) (filter (comp #{area} :area))
                              player-no (filter (fn [option]
                                                  (if (keyword? option)
                                                    (= player-no (:player-no choice))
                                                    (= player-no (:player-no option)))))
                              breach-no (filter (comp #{breach-no} :breach-no))
                              card-id (filter (comp #{card-id} :card-id))
                              card-name (filter (fn [option]
                                                  (= card-name (or (:card-name option)
                                                                   option))))
                              :always first)]
    (when choice-value
      (merge (if (= 1 (or max (count options)))
               {:interaction :quick-choosable}
               (merge {:interaction :choosable}
                      (when choice-opts
                        {:choice-opts choice-opts})))
             (when (map? choice-value)
               {:choice-value choice-value})))))

(defn view-card [{:keys [name text cast type quote]}]
  (merge {:name    name
          :name-ui (ut/format-name name)
          :type    type}
         (when text
           {:text text})
         (when cast
           {:cast-text cast})
         (when quote
           {:quote quote})))

(defn view-supply-card [{{:keys [player-no] :as player} :player
                         choice                         :choice
                         :as                            game}
                        {{:keys [name type cost] :as card} :card
                         :keys                             [pile-size total-pile-size]
                         :or                               {pile-size 0}}]
  (merge (view-card card)
         {:cost            cost
          :number-of-cards pile-size}
         (when (and total-pile-size
                    (> total-pile-size pile-size))
           {:total-number-of-cards total-pile-size})
         (when (and (ut/can-main? game player-no)
                    (not choice)
                    (pos? pile-size)
                    (ut/can-afford? player cost type))
           {:interaction :buyable})
         (choice-interaction {:area      :supply
                              :card-name name} choice)))

(defn view-supply [{:keys [supply] :as game}]
  (->> supply
       (map (fn [pile]
              (let [top-card (view-supply-card game pile)]
                {:card top-card})))))

(defn- type-sort-order [{:keys [type]}]
  (cond (= :gem type) 1
        (= :relic type) 2
        (= :spell type) 3))

(defn view-area [area {{:keys [phase player-no] :as player} :player
                       choice                               :choice
                       active?                              :active-player?
                       :as                                  game}
                 & [position number-of-cards]]
  (let [take-fn (if (= :bottom position) take-last take)
        cards   (cond->> (get player area)
                         number-of-cards (take-fn number-of-cards))]
    (-> cards
        (->>
          (map (fn [{:keys [name type] :as card}]
                 (merge (view-card card)
                        (when (ut/stay-in-play game player-no card)
                          {:stay-in-play true})
                        (when (and active?
                                   (= :hand area)
                                   (not choice)
                                   (ut/can-main? game player-no))
                          (cond
                            (#{:gem :relic} type) {:interaction :playable}
                            (and (#{:spell} type)
                                 (ut/can-prep? game {:player-no player-no
                                                     :card      card})) {:interaction       :prepable
                                                                         :prepable-breaches (ut/prepable-breaches game {:player-no player-no
                                                                                                                        :card      card})}))
                        (when (and active?
                                   (= :play-area area)
                                   (not choice))
                          {:interaction :discardable})
                        (choice-interaction {:area      area
                                             :player-no player-no
                                             :card-name name} choice))))
          frequencies
          (map (fn [[card number-of-cards]]
                 (cond-> card
                         (< 1 number-of-cards) (assoc :number-of-cards number-of-cards)))))
        (cond->> (not number-of-cards) (sort-by (juxt type-sort-order :name))))))

(defn view-deck [{{:keys [deck
                          revealed
                          revealed-cards]} :player
                  :as                      data}]
  (let [full-deck (concat revealed deck)]
    (if (empty? full-deck)
      {}
      (merge {:number-of-cards (count full-deck)}
             (when (or (not-empty revealed)
                       revealed-cards)
               {:visible-cards (concat (view-area :revealed data)
                                       (when revealed-cards
                                         (view-area :deck data :top revealed-cards)))})))))

(defn view-discard [{{:keys [player-no discard gaining]} :player
                     choice                              :choice}]
  (let [cards      (concat (->> discard
                                (map #(assoc % :area :discard)))
                           (->> gaining
                                (map #(assoc % :area :gaining))))
        choice-key (fn choice-key [{:keys [id name area]}]
                     (merge {:area      area
                             :player-no player-no}
                            (case area
                              :discard {:card-id id}
                              :gaining {:card-name name})))]
    (merge
      (when (not-empty cards)
        {:card (let [card (last cards)]
                 (merge (view-card card)
                        (choice-interaction (choice-key card) choice)))})
      {:cards           (if (empty? cards)
                          []
                          (->> cards
                               (map (fn [card]
                                      (merge (view-card card)
                                             (choice-interaction (choice-key card) choice))))))
       :number-of-cards (count cards)})))

(defn view-purchased [{:keys [hand play-area breaches deck revealed discard gaining] :as player}]
  (->> breaches
       (mapcat :prepped-spells)
       (concat hand play-area deck revealed discard gaining)
       (filter (fn [{:keys [cost]}]
                 (and cost
                      (>= cost 2))))
       (map view-card)
       frequencies
       (map (fn [[card number-of-cards]]
              (cond-> card
                      (< 1 number-of-cards) (assoc :number-of-cards number-of-cards))))
       (sort-by (juxt type-sort-order :cost :name))))

(defn view-options [options]
  (->> options
       (map (fn [option] (select-keys option [:option :text])))))

(defn view-choice [{:keys [title text or-choice source options min max optional? repeatable?] :as choice}]
  (->> (merge (when title
                {:choice-title (ut/format-name title)})
              {:text          text
               :min           (or min 0)
               :max           (or max (count options))
               :quick-choice? (and (= 1 min (or max (count options)))
                                   (not optional?))}
              (when or-choice
                {:or-text (:text or-choice)})
              (case source
                :special {:options (view-options options)}
                :deck-position {:interval {:from (first options)
                                           :to   (last options)}}
                {})
              (when-not (nil? optional?)
                {:optional? optional?})
              (when repeatable?
                {:repeatable? repeatable?}))))

(defn view-breaches [{{:keys [player-no
                              breaches
                              breach-cost-reduction
                              phase] :as player} :player
                      choice                     :choice
                      active?                    :active-player?
                      :as                        game}]
  (->> breaches
       (map-indexed (fn view-breach [breach-no {:keys [status prepped-spells focus-cost open-costs stage bonus-damage type]}]
                      (let [focus-cost (when focus-cost
                                         (ut/minus-cost focus-cost breach-cost-reduction))
                            open-cost  (when (and open-costs stage)
                                         (ut/minus-cost (get open-costs stage) breach-cost-reduction))]
                        (merge {:name-ui   (if (= :destroyed status)
                                             "X"
                                             (ut/format-breach-no breach-no))
                                :breach-no breach-no
                                :status    status}
                               (when (not-empty prepped-spells)
                                 {:prepped-spells (->> prepped-spells
                                                       (map (fn [{:keys [name type] :as card}]
                                                              (merge (view-card card)
                                                                     (when (and active?
                                                                                (not choice)
                                                                                (#{:casting} phase)
                                                                                (#{:spell} type))
                                                                       {:interaction :castable})
                                                                     (when (and active?
                                                                                (not choice)
                                                                                (op/can-use-while-prepped? game {:player-no player-no
                                                                                                                 :breach-no breach-no
                                                                                                                 :card      card}))
                                                                       {:interaction :while-preppedable})
                                                                     (choice-interaction {:area      :prepped-spells
                                                                                          :player-no player-no
                                                                                          :breach-no breach-no
                                                                                          :card-name name} choice)))))})
                               (when (and (= :opened status)
                                          bonus-damage)
                                 {:bonus-damage bonus-damage})
                               (when focus-cost
                                 {:focus-cost focus-cost})
                               (when (and open-costs stage)
                                 {:open-cost open-cost})
                               (when type
                                 {:type type})
                               (when (and active?
                                          (not choice)
                                          (ut/can-main? game player-no)
                                          focus-cost
                                          open-cost)
                                 {:interactions (cond-> #{}
                                                        (ut/can-afford? player focus-cost :breach) (conj :focusable)
                                                        (ut/can-afford? player open-cost :breach) (conj :openable))})
                               (choice-interaction {:area      :breaches
                                                    :player-no player-no
                                                    :breach-no breach-no} choice)))))))

(defn view-ability [{:keys [player current-player choice] :as game}]
  (let [{:keys [player-no ability]} player
        {:keys [name
                text
                activation
                charges
                charge-cost]} ability]
    (merge {:name-ui         (ut/format-name name)
            :text            text
            :charges         charges
            :charge-cost     charge-cost
            :activation-text (case activation
                               :your-main-phase "Activate during your main phase:"
                               :any-main-phase "Activate during any player's main phase:"
                               :nemesis-draw "Activate during the nemesis draw phase:")}
           (cond
             (and (= current-player player-no)
                  (not choice)
                  (ut/can-main? game player-no)
                  (ut/can-afford? player 2 :charge-ability)
                  (not (>= charges charge-cost))) {:interaction :chargeable}
             (and (or (and (= current-player player-no)
                           (= :your-main-phase activation))
                      (and (int? current-player)
                           (= :any-main-phase activation)))
                  (not choice)
                  (ut/can-main? game current-player)
                  (>= charges charge-cost)) {:interaction :activatable})
           (choice-interaction {:area      :ability
                                :player-no player-no}
                               choice))))

(defn view-player [{{:keys [player-no name turn-order-token title life trophies]
                     :as   player} :player
                    :keys          [choice
                                    active-player?]
                    :as            data}]
  (merge {:active?   active-player?
          :name      name
          :name-ui   (ut/format-name name)
          :title     title
          :type      {:player-no player-no}
          :ability   (view-ability data)
          :breaches  (view-breaches data)
          :hand      (view-area :hand data)
          :play-area (view-area :play-area data)
          :deck      (view-deck data)
          :discard   (view-discard data)
          :purchased (view-purchased player)
          :life      life
          :aether    (ut/format-aether player)}
         (when turn-order-token
           {:turn-order-token turn-order-token})
         (when trophies
           {:trophies trophies})
         (choice-interaction {:area      :players
                              :player-no player-no} choice)))

(defn format-text [text & [title]]
  (cond
    (coll? text) (->> text
                      (map-indexed (fn [idx paragraph]
                                     (str (when (and (zero? idx) title)
                                            (str title ": "))
                                          paragraph))))
    text [(str (when title
                 (str title ": "))
               text)]))

(defn get-value [game val]
  (if (keyword? val)
    (let [val-fn (effects/get-predicate val)]
      (when val-fn
        (val-fn game)))
    val))

(defn view-nemesis [{{:keys [name life play-area deck discard
                             unleash-text additional-rules
                             tokens fury husks tainted-jades tainted-track
                             corruption-deck]} :nemesis
                     {:keys [player-no phase]} :player
                     choice                    :choice
                     resolving                 :resolving
                     :as                       game}]
  (merge {:name-ui          (ut/format-name name)
          :life             life
          :deck             {:number-of-cards (count deck)}
          :unleash-text     (get-value game unleash-text)
          :additional-rules (str "Additional Rules:\n"
                                 (->> (get-value game additional-rules)
                                      (string/join "\n")))}
         (when (not-empty play-area)
           {:play-area (->> play-area
                            (map (fn [{:keys [name immediately to-discard power persistent life] :as card}]
                                   (let [can-discard? (can-discard? game {:player-no player-no
                                                                          :card      card})]
                                     (merge (view-card card)
                                            (when (= resolving name)
                                              {:status :resolving})
                                            (when immediately
                                              {:immediately-text (:text immediately)})
                                            (when to-discard
                                              {:to-discard-text (:text to-discard)})
                                            (when power
                                              {:power      (:power power)
                                               :power-text (:text power)})
                                            (when persistent
                                              {:persistent-text (:text persistent)})
                                            (when life
                                              {:life life})
                                            (when (and can-discard?
                                                       (not choice)
                                                       (ut/can-main? game player-no))
                                              {:interaction :discardable})
                                            (choice-interaction {:area      :play-area
                                                                 :card-name name} choice)
                                            (choice-interaction {:area      :minions
                                                                 :card-name name} choice))))))})
         {:discard (merge
                     (when (not-empty discard)
                       {:card (let [{:keys [name immediately to-discard power persistent life] :as card} (last discard)]
                                (merge (view-card card)
                                       (when (= resolving name)
                                         {:status :resolving})
                                       (when immediately
                                         {:immediately-text (:text immediately)})
                                       (when to-discard
                                         {:to-discard-text (:text to-discard)})
                                       (when power
                                         {:power-text (:text power)})
                                       (when persistent
                                         {:persistent-text (:text persistent)})
                                       (when life
                                         {:life life})
                                       (choice-interaction {:area      :discard
                                                            :card-name name} choice)))})
                     {:cards           (if (empty? discard)
                                         []
                                         (->> discard
                                              (map (fn [{:keys [name text immediately to-discard power persistent life] :as card}]
                                                     (merge (view-card card)
                                                            {:text (concat (when life
                                                                             [(str "Life: " life)])
                                                                           (when text
                                                                             (format-text text))
                                                                           (when immediately
                                                                             (format-text (:text immediately) "IMMEDIATELY"))
                                                                           (when to-discard
                                                                             (format-text (:text to-discard) "TO DISCARD"))
                                                                           (when power
                                                                             (format-text (:text power) "POWER"))
                                                                           (when persistent
                                                                             (format-text (:text persistent) "PERSISTENT")))}
                                                            (when immediately
                                                              {:immediately-text (:text immediately)})
                                                            (when to-discard
                                                              {:to-discard-text (:text to-discard)})
                                                            (when power
                                                              {:power-text (:text power)})
                                                            (when persistent
                                                              {:persistent-text (:text persistent)})
                                                            (when life
                                                              {:life life})
                                                            (choice-interaction {:area      :discard
                                                                                 :card-name name} choice))))))
                      :number-of-cards (count discard)})}
         (when tokens
           {:tokens tokens})
         (when fury
           {:fury fury})
         (when husks
           (let [{:keys [number-of-husks swarm-effects]} husks
                 {:keys [text min-husks max-husks]} (lookup-swarm-effects husks)]
             {:husks (merge {:number-of-husks number-of-husks
                             :swarm-interval  (str min-husks (if max-husks
                                                               (str "-" max-husks)
                                                               "+"))
                             :swarm-text      text
                             :title           (concat ["When Carapace Queen Swarms, count the number of husks in play and resolve the following:"]
                                                      (->> swarm-effects
                                                           (map (fn [{:keys [min-husks max-husks text]}]
                                                                  (str min-husks (if max-husks (str "-" max-husks) "+")
                                                                       " husks: " text)))))}
                            (choice-interaction {:area      :minions
                                                 :card-name :husks} choice)
                            (choice-interaction {:area :husks} choice))}))
         (when tainted-jades
           {:tainted-jades (count tainted-jades)})
         (when tainted-track
           (let [{:keys [tainted-level tainted-effects]} tainted-track
                 {:keys [text level]} (if (= :tainted-track resolving)
                                        (lookup-tainted-effects tainted-track)
                                        (lookup-next-tainted-effects tainted-track))]
             {:tainted-track (merge {:tainted-level      tainted-level
                                     :next-tainted-level level
                                     :next-tainted-text  text
                                     :title              (concat ["When the Tainted Track is advanced to a space with an effect listed on it, resolve that effect immediately."]
                                                                 (->> tainted-effects
                                                                      (map (fn [{:keys [level text]}]
                                                                             (str "Space " level ": " text)))))}
                                    (when (= :tainted-track resolving)
                                      {:status :resolving})
                                    (choice-interaction {:area :tainted-track} choice))}))
         (when corruption-deck
           {:corruptions (count corruption-deck)})
         (choice-interaction {:area :nemesis} choice)))

(defn view-trash [{:keys [trash]}]
  (merge
    (when (not-empty trash)
      {:card (let [{:keys [name type]} (last trash)]
               {:name    name
                :name-ui (ut/format-name name)
                :type    type})})
    {:cards           (if (empty? trash)
                        []
                        (->> trash
                             (map (fn [{:keys [name type]}]
                                    (merge {:name    name
                                            :name-ui (ut/format-name name)
                                            :type    type})))
                             frequencies
                             (map (fn [[card number-of-cards]]
                                    (cond-> card
                                            (< 1 number-of-cards) (assoc :number-of-cards number-of-cards))))))
     :number-of-cards (count trash)}))

(defn view-commands [{:keys [players effect-stack current-player can-undo?] :as game}]
  (let [{:keys [hand play-area aether phase]
         :or   {aether 0}} (get players current-player)
        [choice] effect-stack]
    {:can-undo?          (boolean can-undo?)
     :can-discard-all?   (boolean (and (not choice)
                                       (not-empty play-area)))
     :can-play-all-gems? (boolean (and (not choice)
                                       (ut/can-main? game current-player)
                                       (some (comp #{:gem} :type) hand)))
     :can-end-turn?      (and (not choice)
                              (empty? play-area)
                              (or (ut/can-main? game current-player)
                                  (= :draw phase)))
     :confirm-end-turn   (cond (<= 2 aether) "You can spend aether."
                               (and (ut/can-main? game current-player)
                                    (not-empty hand)) "You still have cards in your hand.")}))

(defn view-turn-order [{:keys [deck discard revealed revealed-cards]} {:keys [source area options max] :as choice}]
  {:deck    (if (empty? (concat revealed deck))
              {}
              (merge {:number-of-cards (+ (count deck) (count revealed))}
                     (when (or revealed revealed-cards)
                       {:visible-cards (->> (if revealed
                                              revealed
                                              (take revealed-cards deck))
                                            (map (fn [{:keys [name type]}]
                                                   (merge {:name    name
                                                           :name-ui (ut/format-name name)
                                                           :type    type}
                                                          (when (and (= :turn-order source)
                                                                     (= :revealed area)
                                                                     (some #{name} options))
                                                            (if (= 1 (or max (count options)))
                                                              {:interaction :quick-choosable}
                                                              {:interaction :choosable}))))))})))
   :discard (merge
              (when (not-empty discard)
                {:card (let [{:keys [name type]} (last discard)]
                         (merge {:name    name
                                 :name-ui (ut/format-name name)
                                 :type    type}
                                (choice-interaction {:area      :discard
                                                     :card-name name} choice)))})
              {:cards           (if (empty? discard)
                                  []
                                  (->> discard
                                       (map (fn [{:keys [name type]}]
                                              (merge {:name    name
                                                      :name-ui (ut/format-name name)
                                                      :type    type}
                                                     (choice-interaction {:area      :discard
                                                                          :card-name name} choice))))))
               :number-of-cards (count discard)})})

(defn view-game [{:keys [mode difficulty gravehold turn-order players effect-stack current-player game-over] :as game}]
  (let [[{:keys [player-no] :as choice}] effect-stack
        {:keys [phase] :as player} (get players current-player)]
    (merge
      {:mode       mode
       :nemesis    (view-nemesis (merge game
                                        (when player
                                          {:player (-> player
                                                       (assoc :player-no current-player))})
                                        {:choice choice}))
       :difficulty difficulty
       :gravehold  gravehold
       :supply     (view-supply (merge game
                                       {:player (assoc player :player-no current-player)
                                        :choice choice}))
       :turn-order (view-turn-order turn-order choice)
       :players    (->> players
                        (map-indexed (fn [idx player]
                                       (let [active-player? (and (= idx current-player)
                                                                 (or (nil? choice)
                                                                     (= idx player-no))
                                                                 (not= phase :end-of-game))]
                                         (view-player (merge game
                                                             {:active-player? active-player?
                                                              :player         (assoc player :player-no idx)
                                                              :choice         choice}))))))
       :trash      (view-trash game)
       :commands   (view-commands game)}
      (cond
        game-over {:game-over game-over}
        choice {:choice (view-choice choice)}))))
