(ns aeons-end.front-end-view
  (:require [aeons-end.utils :as ut]))

(defn- choice-interaction [{:keys [area card-name]}
                           {:keys [source options max choice-opts]}]
  (let [interaction (if (= 1 (or max (count options)))
                      {:interaction :quick-choosable}
                      (merge {:interaction :choosable}
                             (when choice-opts
                               {:choice-opts choice-opts})))]
    (cond
      (and (= area source)
           (contains? (set options) card-name)) interaction
      (= :mixed source) (let [card-names (->> options
                                              (filter (comp #{area} :area))
                                              (map :card-name)
                                              set)]
                          (when (contains? card-names card-name)
                            (merge interaction
                                   {:choice-value {:area area :card-name card-name}}))))))

(defn- choice-interaction-player [{:keys [area player-no breach-no card-name]}
                                  {:keys [source options max choice-opts]}]
  (let [interaction  (if (= 1 (or max (count options)))
                       {:interaction :quick-choosable}
                       (merge {:interaction :choosable}
                              (when choice-opts
                                {:choice-opts choice-opts})))
        choice-value (when (= area source)
                       (cond->> options
                                player-no (filter (comp #{player-no} :player-no))
                                breach-no (filter (comp #{breach-no} :breach-no))
                                card-name (filter (comp #{card-name} :card-name))
                                :always first))]
    (when choice-value
      (merge interaction
             {:choice-value choice-value}))))

(defn view-supply-card [{{:keys [aether phase]
                          :or   {aether 0}} :player
                         choice             :choice
                         :as                game}
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
                    aether (>= aether cost))
           {:interaction :buyable})
         (choice-interaction {:area      :supply
                              :card-name name} choice)))

(defn view-supply [{:keys [supply choice] :as game}]
  (->> supply
       (map (fn [pile]
              (let [top-card (view-supply-card game (ut/access-top-card pile))]
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
                          (remove (comp #{:closed} :status))
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

(defn view-discard [{{:keys [discard
                             gaining]} :player
                     choice            :choice}]
  (merge
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
                                            :type    type}
                                           (choice-interaction {:area      :discard
                                                                :card-name name} choice))))))
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
                              phase
                              aether] :as player} :player
                      choice                      :choice
                      active?                     :active-player?
                      :as                         game}]
  (->> breaches
       (map-indexed (fn view-breach [idx {:keys [status prepped-spells focus-cost open-costs stage bonus-damage]}]
                      (let [open-cost (when (and open-costs stage) (get open-costs stage))]
                        (merge {:name-ui   (case idx
                                             0 "I"
                                             1 "II"
                                             2 "III"
                                             3 "IV")
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
                                                                     (choice-interaction-player {:area      :prepped-spells
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
                                          aether
                                          focus-cost
                                          open-cost)
                                 {:interactions (cond-> #{}
                                                        (>= aether focus-cost) (conj :focusable)
                                                        (>= aether open-cost) (conj :openable))})))))))

(defn view-ability [{{:keys [ability
                             phase
                             aether]
                      :or   {aether 0} :as player} :player
                     choice                        :choice
                     active?                       :active-player?
                     :as                           game}]
  (let [{:keys [name
                text
                charges
                charge-cost]} ability]
    (merge {:name-ui     (ut/format-name name)
            :text        text
            :charges     charges
            :charge-cost charge-cost}
           (cond
             (and active?
                  (not choice)
                  (#{:casting :main} phase)
                  (>= aether 2)
                  (not (>= charges charge-cost))) {:interaction :chargeable}
             (and active?
                  (not choice)
                  (#{:casting :main} phase)
                  (>= charges charge-cost)) {:interaction :activatable}))))

(defn view-player [{{:keys [player-no name title life aether]
                     :or   {aether 0}} :player
                    :keys              [choice
                                        active-player?]
                    :as                data}]
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
          :aether    aether}
         (when (and (:player-no choice)
                    (or (not (#{:players :prepped-spells} (:source choice)))
                        active-player?))
           {:choice (view-choice choice)})
         (choice-interaction-player {:area      :players
                                     :player-no player-no} choice)))

(defn view-nemesis [{:keys [name life tokens deck play-area discard]} choice]
  (merge {:name-ui (ut/format-name name)
          :life    life
          :tokens  tokens
          :deck    (if (empty? deck)
                     {}
                     {:number-of-cards (count deck)})}
         (when (not-empty play-area)
           {:play-area (->> play-area
                            (map (fn [{:keys [name text quote type tier]}]
                                   (merge {:name    name
                                           :name-ui (ut/format-name name)
                                           :text    text
                                           :quote   quote
                                           :type    type
                                           :tier    tier}
                                          (choice-interaction {:area      :nemesis
                                                               :card-name name} choice)))))})
         (when (not-empty discard)
           (let [{:keys [name text quote type]} (last discard)]
             {:discard {:name    name
                        :name-ui (ut/format-name name)
                        :text    text
                        :quote   quote
                        :type    type}}))
         (when choice
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

(defn view-game [{:keys [nemesis gravehold turn-order players effect-stack current-player] :as game}]
  (let [[{:keys [player-no source] :as choice}] effect-stack
        {:keys [phase] :as player} (get players current-player)]
    (->> (merge
           {:nemesis    (view-nemesis nemesis (when (nil? player-no)
                                                choice))
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
                                              (view-player (merge game {:active-player? active-player?
                                                                        :player         (assoc player :player-no idx)}
                                                                  (when (or (= idx player-no)
                                                                            (#{:players :prepped-spells} source))
                                                                    {:choice choice})))))))
            :trash      (view-trash (merge game {:choice choice}))
            :commands   (view-commands game)}))))
