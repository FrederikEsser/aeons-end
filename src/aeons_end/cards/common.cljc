(ns aeons-end.cards.common
  (:require [aeons-end.operations :refer [move-cards push-effect-stack]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [medley.core :as medley]))

(defn player-starting-life [difficulty]
  (case difficulty
    :beginner 12
    :extinction 8
    10))

(defn gravehold-starting-life [difficulty]
  (case difficulty
    :beginner 35
    :extinction 25
    30))

(defn gain-aether [{:keys [current-player] :as game} {:keys [player-no arg earmark restrict]}]
  (let [current-player? (or (nil? current-player)
                            (= current-player player-no))]
    (cond
      earmark (cond-> game
                      (and (pos? arg)
                           current-player?) (update-in [:players player-no :earmarked-aether earmark] ut/plus arg))
      restrict (cond-> game
                       (and (pos? arg)
                            current-player?) (update-in [:players player-no :restricted-aether restrict] ut/plus arg))
      :else (cond-> game
                    (and (pos? arg)
                         current-player?) (update-in [:players player-no :aether] ut/plus arg)))))

(effects/register {:gain-aether gain-aether})

(defn heal [{:keys [difficulty] :as game} {:keys [player-no life]}]
  (let [player-starting-life (player-starting-life difficulty)
        current-life         (get-in game [:players player-no :life])]
    (cond-> game
            (pos? current-life) (assoc-in [:players player-no :life] (min (+ current-life life)
                                                                          player-starting-life)))))

(defn heal-gravehold [{:keys [difficulty] :as game} {:keys [arg]}]
  (let [gravehold-starting-life (gravehold-starting-life difficulty)
        current-life            (get-in game [:gravehold :life])]
    (-> game
        (assoc-in [:gravehold :life] (min (+ current-life arg)
                                          gravehold-starting-life)))))

(effects/register {:heal           heal
                   :heal-gravehold heal-gravehold})

(defn kill-player [game {:keys [player-no]}]
  (let [{:keys [life]} (get-in game [:players player-no])]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:damage-player life]]})))

(effects/register {:kill-player kill-player})

(defn reveal-from-deck [game {:keys [player-no arg]}]
  (move-cards game {:player-no       player-no
                    :number-of-cards arg
                    :from            :deck
                    :from-position   :top
                    :to              :revealed}))

(effects/register {:reveal-from-deck reveal-from-deck})

(defn discard-from-revealed [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name
              (not-empty card-names)) (move-cards (merge args {:from :revealed
                                                               :to   :discard}))))

(effects/register {:discard-from-revealed discard-from-revealed})

(defn trash-from-revealed [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (move-cards (merge args {:from :revealed
                                                             :to   :trash}))))

(effects/register {:trash-from-revealed trash-from-revealed})

(defn topdeck-all-revealed [game {:keys [player-no]}]
  (let [revealed (get-in game [:players player-no :revealed])]
    (move-cards game {:player-no   player-no
                      :card-names  (map :name revealed)
                      :from        :revealed
                      :to          :deck
                      :to-position :top})))

(effects/register {:topdeck-all-revealed topdeck-all-revealed})

(defn discard-from-hand [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (move-cards (merge args {:from :hand
                                                             :to   :discard}))))

(effects/register {:discard-from-hand discard-from-hand})

(defn collective-discard-from-hand [game {:keys [player-no card-name player-card-names]}]
  (->> (or player-card-names
           (when (and player-no card-name)
             [{:player-no player-no
               :card-name card-name}]))
       (group-by :player-no)
       (medley/map-vals #(map :card-name %))
       (reduce (fn [game [player-no card-names]]
                 (push-effect-stack game {:player-no player-no
                                          :effects   [[:discard-from-hand {:card-names card-names}]]}))
               game)))

(effects/register {:collective-discard-from-hand collective-discard-from-hand})

(defn collective-discard-prepped-spells [game {:keys [player-no breach-no card-name spells] :as args}]
  (->> (or spells
           (when (and player-no breach-no card-name)
             [{:player-no player-no
               :breach-no breach-no
               :card-name card-name}]))
       (group-by :player-no)
       (reduce (fn [game [player-no spells]]
                 (push-effect-stack game {:player-no player-no
                                          :args      args
                                          :effects   [[:discard-prepped-spells {:spells spells}]]}))
               game)))

(effects/register {:collective-discard-prepped-spells collective-discard-prepped-spells})

(defn take-from-discard [game {:keys [player-no card-id]}]
  (cond-> game
          card-id (push-effect-stack {:player-no player-no
                                      :effects   [[:move-card {:move-card-id card-id
                                                               :from         :discard
                                                               :to           :hand}]]})))

(effects/register {:take-from-discard take-from-discard})

(defn shuffle-discard-into-deck [game {:keys [player-no]}]
  (let [{:keys [deck discard]} (get-in game [:players player-no])
        new-deck (->> deck
                      (concat discard)
                      shuffle
                      vec)]
    (-> game
        (assoc-in [:players player-no :deck] new-deck)
        (update-in [:players player-no] dissoc :discard :revealed-cards))))

(effects/register {:shuffle-discard-into-deck shuffle-discard-into-deck})

(defn discard-prepped-spells [game {:keys [player-no spells] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   (if spells
                                        (->> spells
                                             (map (fn [args]
                                                    [:move-card (merge args
                                                                       {:from :breach
                                                                        :to   :discard})])))
                                        [[:move-card (merge args
                                                            {:from :breach
                                                             :to   :discard})]])}))

(effects/register {:discard-prepped-spells discard-prepped-spells})

(defn topdeck-prepped-spell [game {:keys [player-no spells] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   (if spells
                                        (->> spells
                                             (map (fn [args]
                                                    [:move-card (merge args
                                                                       {:from        :breach
                                                                        :to          :deck
                                                                        :to-position :top})])))
                                        [[:move-card (merge args
                                                            {:from        :breach
                                                             :to          :deck
                                                             :to-position :top})]])}))

(effects/register {:topdeck-prepped-spell topdeck-prepped-spell})

(defn topdeck-from-gained [game {:keys [player-no card-id card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:move-card {:card-name   card-name
                                                                 :from        :gaining
                                                                 :to          :deck
                                                                 :to-position :top}]]})))

(effects/register {:topdeck-from-gained topdeck-from-gained})

(defn destroy-prepped-spells [game {:keys [spells] :as args}]
  (push-effect-stack game {:effects (if spells
                                      (->> spells
                                           (map (fn [spell]
                                                  [:move-card (merge spell
                                                                     {:from :breach
                                                                      :to   :trash})])))
                                      [[:move-card (merge args
                                                          {:from :breach
                                                           :to   :trash})]])}))

(effects/register {:destroy-prepped-spells destroy-prepped-spells})

(defn destroy-breach [game {:keys [player-no breach-no put-prepped-spells-in]}]
  (let [{:keys [prepped-spells]} (get-in game [:players player-no :breaches breach-no])]
    (-> game
        (assoc-in [:players player-no :breaches breach-no] {:status :destroyed})
        (cond-> (not-empty prepped-spells) (update-in [:players player-no put-prepped-spells-in] concat prepped-spells)))))

(effects/register {:destroy-breach destroy-breach})

(defn gain-breach [game {:keys [player-no breach-no breach]}]
  (assoc-in game [:players player-no :breaches breach-no] breach))

(effects/register {:gain-breach gain-breach})

(defn destroy-from-hand [game {:keys [card-name card-names] :as args}]
  (cond-> game
          (or card-name card-names) (move-cards (merge args {:from :hand
                                                             :to   :trash}))))

(effects/register {:destroy-from-hand destroy-from-hand})

(defn destroy-from-discard [game {:keys [player-no card-id card-ids]}]
  (let [card-ids (if card-id
                   [{:player-no player-no :card-id card-id}]
                   card-ids)]
    (cond-> game
            (not-empty card-ids) (push-effect-stack {:player-no player-no
                                                     :effects   (->> card-ids
                                                                     (mapv (fn [{:keys [card-id]}]
                                                                             [:move-card {:move-card-id card-id
                                                                                          :from         :discard
                                                                                          :to           :trash}])))}))))

(effects/register {:destroy-from-discard destroy-from-discard})

(defn destroy-from-area [game {:keys [player-no area card-name card-id choices]}]
  (let [choices (or choices
                    (when (and area (or card-name card-id))
                      [{:area      area
                        :card-name card-name
                        :card-id   card-id}]))]
    (cond-> game
            (not-empty choices) (push-effect-stack {:player-no player-no
                                                    :effects   (->> choices
                                                                    (map (fn [{:keys [area card-name card-id]}]
                                                                           [:move-card (medley/assoc-some {:from area
                                                                                                           :to   :trash}
                                                                                                          :card-name card-name
                                                                                                          :move-card-id card-id)])))}))))

(effects/register {:destroy-from-area destroy-from-area})

(defn destroy-this [game {:keys [player-no card-id]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :play-area] {:id card-id})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:move-card {:move-card-id card-id
                                                              :from         :play-area
                                                              :to           :trash}]]}))))

(effects/register {:destroy-this destroy-this})

(defn play-twice [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:move-card {:card-name card-name
                                                              :from      :hand
                                                              :to        :play-area}]
                                                 [:card-effect {:card card}]
                                                 [:card-effect {:card card}]]}))))

(effects/register {:play-twice play-twice})

(defn prep-from-discard [game {:keys [player-no card-id closed-breaches?]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :discard] {:id card-id})
        breaches     (->> (get-in game [:players player-no :breaches])
                          (map-indexed (fn [breach-no breach]
                                         (assoc breach :breach-no breach-no))))
        breach-stati (cond-> #{:opened :focused}
                             closed-breaches? (conj :closed))
        breach-no    (->> breaches
                          (filter (comp breach-stati :status))
                          (filter (comp empty? :prepped-spells))
                          (sort-by (juxt :status :bonus-damage))
                          last
                          :breach-no)]
    (cond-> game
            (and card-id
                 breach-no) (push-effect-stack {:player-no player-no
                                                :effects   [[:move-card {:move-card-id card-id
                                                                         :from         :discard
                                                                         :to           :breach
                                                                         :breach-no    breach-no}]
                                                            [:on-prep-spell {:card card}]]}))))

(effects/register {:prep-from-discard prep-from-discard})

(defn focus-lowest-cost-breach [game {:keys [player-no]}]
  (let [breach-no (->> (get-in game [:players player-no :breaches])
                       (keep-indexed (fn [idx {:keys [status focus-cost]}]
                                       (when (#{:closed :focused} status)
                                         {:breach-no  idx
                                          :focus-cost focus-cost})))
                       (sort-by :focus-cost)
                       (map :breach-no)
                       first)]
    (cond-> game
            breach-no (push-effect-stack {:player-no player-no
                                          :effects   [[:focus-breach {:breach-no breach-no}]]}))))

(effects/register {:focus-lowest-cost-breach focus-lowest-cost-breach})
