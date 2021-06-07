(ns aeons-end.front-end-view
  (:require [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]))

(defn- choice-interaction-simple [{:keys [area card-name] :as args}
                                  {:keys [source options max choice-opts] :as choice}]
  (let [interaction (if (= 1 (or max (count options)))
                      {:interaction :quick-choosable}
                      (merge {:interaction :choosable}
                             (when choice-opts
                               {:choice-opts choice-opts})))]
    (cond
      (and (= area (:area choice))
           (contains? (set options) card-name)) interaction
      (= :mixed source) (if card-name
                          (let [card-names (->> options
                                                (filter (comp #{area} :area))
                                                (map :card-name)
                                                set)]
                            (when (contains? card-names card-name)
                              (merge interaction
                                     {:choice-value {:area area :card-name card-name}})))
                          (when (contains? (set options) {:area area})
                            (merge interaction
                                   {:choice-value {:area area}}))))))

(defn- choice-interaction-multi [{:keys [area player-no breach-no card-name card-id] :as args}
                                 {:keys [options max choice-opts] :as choice}]
  (let [interaction  (if (= 1 (or max (count options)))
                       {:interaction :quick-choosable}
                       (merge {:interaction :choosable}
                              (when choice-opts
                                {:choice-opts choice-opts})))
        choice-value (when (= area (:area choice))
                       (cond->> options
                                player-no (filter (comp #{player-no} :player-no))
                                breach-no (filter (comp #{breach-no} :breach-no))
                                card-name (filter (comp #{card-name} :card-name))
                                card-id (filter (comp #{card-id} :card-id))
                                :always first))]
    (when choice-value
      (merge interaction
             {:choice-value choice-value}))))

(defn- choice-interaction [identifier {:keys [source] :as choice}]
  (if (= :players source)
    (choice-interaction-multi identifier choice)
    (choice-interaction-simple identifier choice)))

(defn view-supply-card [{{:keys [phase] :as player} :player
                         choice                     :choice
                         :as                        game}
                        {{:keys [name text type cost] :as card} :card
                         :keys                                  [pile-size total-pile-size]
                         :or                                    {pile-size 0}}]
  (merge {:name            name
          :name-ui         (ut/format-name name)
          :text            text
          :type            type
          :cost            cost
          :number-of-cards pile-size}
         (when (and total-pile-size
                    (> total-pile-size pile-size))
           {:total-number-of-cards total-pile-size})
         (when (and (#{:casting :main} phase)
                    (not choice)
                    (pos? pile-size)
                    (ut/can-afford? player cost type))
           {:interaction :buyable})
         (choice-interaction {:area      :supply
                              :card-name name} choice)))

(defn view-supply [{:keys [supply choice] :as game}]
  (->> supply
       (map (fn [pile]
              (let [top-card (view-supply-card game pile)]
                {:card top-card})))))

(defn- type-sort-order [type]
  (cond (= :gem type) 1
        (= :relic type) 2
        (= :spell type) 3))

(defn view-area [area {{:keys [phase player-no breaches] :as player} :player
                       choice                                        :choice
                       active?                                       :active-player?
                       :as                                           game}
                 & [position number-of-cards]]
  (let [take-fn      (if (= :bottom position) take-last take)
        cards        (cond->> (get player area)
                              number-of-cards (take-fn number-of-cards))
        open-breach? (->> breaches
                          (filter (comp #{:opened :focused} :status))
                          (some (comp empty? :prepped-spells)))]
    (-> cards
        (->>
          (map (fn [{:keys [id name text type] :as card}]
                 (merge {:name    name
                         :name-ui (ut/format-name name)
                         :text    text
                         :type    type}
                        (when (ut/stay-in-play game player-no card)
                          {:stay-in-play true})
                        (when (and active?
                                   (= :hand area)
                                   (not choice)
                                   (#{:casting :main} phase))
                          (if (#{:gem :relic} type)
                            {:interaction :playable}
                            (when open-breach?
                              {:interaction :prepable})))
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
        (cond->> (not number-of-cards) (sort-by (juxt (comp type-sort-order :type)
                                                      :name))))))

(defn view-deck [{{:keys [deck
                          revealed
                          revealed-cards]} :player
                  :as                      data}]
  (let [full-deck              (concat revealed deck)
        revealed-cards-in-deck (:deck revealed-cards)]
    (if (empty? full-deck)
      {}
      (merge {:number-of-cards (count full-deck)}
             (when (or (not-empty revealed)
                       revealed-cards-in-deck)
               {:visible-cards (concat (view-area :revealed data)
                                       (when revealed-cards-in-deck
                                         (view-area :deck data :top revealed-cards-in-deck)))})))))

(defn view-discard [{{:keys [player-no discard]} :player
                     choice                      :choice}]
  (merge
    (when (not-empty discard)
      {:card (let [{:keys [id name text type]} (last discard)]
               (merge {:name    name
                       :name-ui (ut/format-name name)
                       :text    text
                       :type    type}
                      (choice-interaction-multi {:area    :discard
                                                 :card-id id} choice)))})
    {:cards           (if (empty? discard)
                        []
                        (->> discard
                             (map (fn [{:keys [id name text type]}]
                                    (merge {:name    name
                                            :name-ui (ut/format-name name)
                                            :text    text
                                            :type    type}
                                           (choice-interaction-multi {:area      :discard
                                                                      :player-no player-no
                                                                      :card-id   id} choice))))))
     :number-of-cards (count discard)}))

(defn view-options [options]
  (->> options
       (map (fn [option] (select-keys option [:option :text])))))

(defn view-choice [{:keys [title text or-choice source options min max optional?] :as choice}]
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
                {:optional? optional?}))))

(defn view-breaches [{{:keys [player-no
                              breaches
                              phase] :as player} :player
                      choice                     :choice
                      active?                    :active-player?
                      :as                        game}]
  (->> breaches
       (map-indexed (fn view-breach [idx {:keys [status prepped-spells focus-cost open-costs stage bonus-damage]}]
                      (let [open-cost (when (and open-costs stage) (get open-costs stage))]
                        (merge {:name-ui   (if (= :destroyed status)
                                             "X"
                                             (ut/format-breach-no idx))
                                :breach-no idx
                                :status    status}
                               (when (not-empty prepped-spells)
                                 {:prepped-spells (->> prepped-spells
                                                       (map (fn [{:keys [id name text type] :as card}]
                                                              (merge {:name    name
                                                                      :name-ui (ut/format-name name)
                                                                      :text    text
                                                                      :type    type}
                                                                     (when (and active?
                                                                                (not choice)
                                                                                (#{:casting} phase)
                                                                                (#{:spell} type))
                                                                       {:interaction :castable})
                                                                     (choice-interaction-multi {:area      :prepped-spells
                                                                                                :player-no player-no
                                                                                                :breach-no idx
                                                                                                :card-name name} choice)))))})
                               (when (and (= :opened status)
                                          bonus-damage)
                                 {:bonus-damage bonus-damage})
                               (when focus-cost
                                 {:focus-cost focus-cost})
                               (when (and open-costs stage)
                                 {:open-cost open-cost})
                               (when (and active?
                                          (not choice)
                                          (#{:casting :main} phase)
                                          focus-cost
                                          open-cost)
                                 {:interactions (cond-> #{}
                                                        (ut/can-afford? player focus-cost :focus-breach) (conj :focusable)
                                                        (ut/can-afford? player open-cost :open-breach) (conj :openable))})
                               (choice-interaction-multi {:area      :breaches
                                                          :breach-no idx} choice)))))))

(defn view-ability [{{:keys [ability
                             phase] :as player} :player
                     choice                     :choice
                     active?                    :active-player?
                     :as                        game}]
  (let [{:keys [name
                text
                charges
                charge-cost]} ability
        {:keys [area options]} choice]
    (merge {:name-ui     (ut/format-name name)
            :text        text
            :charges     charges
            :charge-cost charge-cost}
           (cond
             (and active?
                  (not choice)
                  (#{:casting :main} phase)
                  (ut/can-afford? player 2 :charge-ability)
                  (not (>= charges charge-cost))) {:interaction :chargeable}
             (and active?
                  (not choice)
                  (#{:casting :main} phase)
                  (>= charges charge-cost)) {:interaction :activatable})
           (when (= :charges area)
             {:interaction  :quick-choosable
              :choice-value :charges})
           (when (some (comp #{:charges} :area) options)
             {:interaction  :quick-choosable
              :choice-value {:area :charges}}))))

(defn view-player [{{:keys [player-no name title life]
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
          :life      life
          :aether    (ut/format-aether player)}
         (when (and (:player-no choice)
                    (or active-player?
                        (not= :players (:source choice))))
           {:choice (view-choice choice)})
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

(defn view-nemesis [{{:keys [name life tokens deck play-area discard]} :nemesis
                     {:keys [player-no phase] :as player}              :player
                     choice                                            :choice
                     resolving                                         :resolving
                     :as                                               game}]
  (merge {:name-ui (ut/format-name name)
          :life    life
          :tokens  tokens
          :deck    (if (empty? deck)
                     {}
                     {:number-of-cards (count deck)})}
         (when (not-empty play-area)
           {:play-area (->> play-area
                            (map (fn [{:keys [name text quote type to-discard power persistent life]}]
                                   (let [can-discard-fn (when (and to-discard
                                                                   (:predicate to-discard))
                                                          (effects/get-predicate (:predicate to-discard)))
                                         can-discard?   (and can-discard-fn
                                                             (can-discard-fn game {:player-no player-no}))]
                                     (merge {:name    name
                                             :name-ui (ut/format-name name)
                                             :text    text
                                             :quote   quote
                                             :type    type}
                                            (when (= resolving name)
                                              {:status :resolving})
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
                                                       (not (:choice player))
                                                       (#{:casting :main} phase))
                                              {:interaction :discardable})
                                            (choice-interaction {:area      :minions
                                                                 :card-name name} choice))))))})
         {:discard (merge
                     (when (not-empty discard)
                       {:card (let [{:keys [name text quote type to-discard power persistent life]} (last discard)]
                                (merge {:name    name
                                        :name-ui (ut/format-name name)
                                        :text    text
                                        :quote   quote
                                        :type    type}
                                       (when (= resolving name)
                                         {:status :resolving})
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
                                              (map (fn [{:keys [name text quote type to-discard power persistent life]}]
                                                     (merge {:name    name
                                                             :name-ui (ut/format-name name)
                                                             :text    (concat (when life
                                                                                [(str "Life: " life)])
                                                                              (when text
                                                                                (format-text text))
                                                                              (when to-discard
                                                                                (format-text (:text to-discard) "TO DISCARD"))
                                                                              (when power
                                                                                (format-text (:text power) "POWER"))
                                                                              (when persistent
                                                                                (format-text (:text persistent) "PERSISTENT")))
                                                             :quote   quote
                                                             :type    type}
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
         (choice-interaction {:area :nemesis} choice)
         (when (and choice
                    (nil? (:player-no choice)))
           {:choice (view-choice choice)})))

(defn view-trash [{:keys [trash choice] :as game}]
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
                                            :type    type}
                                           (choice-interaction {:area      :trash
                                                                :card-name name} choice))))
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
                                       (#{:casting :main} phase)
                                       (some (comp #{:gem} :type) hand)))
     :can-end-turn?      (and (not choice)
                              (empty? play-area)
                              (not= phase :end-of-game))
     :confirm-end-turn   (cond (<= 2 aether) "You can spend aether."
                               (and (#{:casting :main} phase)
                                    (not-empty hand)) "You still have cards in your hand.")}))

(defn view-turn-order [{:keys [deck discard]}]
  {:deck    (if (empty? deck)
              {}
              {:number-of-cards (count deck)})
   :discard (merge
              (when (not-empty discard)
                {:card (let [{:keys [name type]} (last discard)]
                         {:name    name
                          :name-ui (ut/format-name name)
                          :type    type})})
              {:cards           (if (empty? discard)
                                  []
                                  (->> discard
                                       (map (fn [{:keys [name type]}]
                                              (merge {:name    name
                                                      :name-ui (ut/format-name name)
                                                      :type    type})))))
               :number-of-cards (count discard)})})

(defn view-game [{:keys [gravehold turn-order players effect-stack current-player game-over] :as game}]
  (let [[{:keys [player-no source] :as choice}] effect-stack
        {:keys [phase] :as player} (get players current-player)]
    (merge
      {:nemesis    (view-nemesis (merge game
                                        (when player
                                          {:player (-> player
                                                       (assoc :player-no current-player)
                                                       (cond-> player-no (assoc :choice choice)))})
                                        {:choice choice}))
       :gravehold  gravehold
       :supply     (view-supply (merge game {:player (assoc player :player-no current-player)
                                             :choice choice}))
       :turn-order (view-turn-order turn-order)
       :players    (->> players
                        (map-indexed (fn [idx player]
                                       (let [active-player? (and (= idx current-player)
                                                                 (or (nil? choice)
                                                                     (= idx player-no))
                                                                 (not= phase :end-of-game))]
                                         (view-player (merge game
                                                             {:active-player? active-player?
                                                              :player         (assoc player :player-no idx)}
                                                             (when (or (= idx player-no)
                                                                       (= :players source))
                                                               {:choice choice})))))))
       :trash      (view-trash (merge game {:choice choice}))
       :commands   (view-commands game)}
      (when game-over
        {:game-over game-over}))))
