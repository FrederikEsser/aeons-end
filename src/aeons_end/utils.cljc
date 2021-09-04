(ns aeons-end.utils
  (:require [clojure.string :as s]
            [clojure.set :refer [intersection]]
            [aeons-end.effects :as effects]
            [clojure.string :as string]
            [medley.core :as medley]))

(defonce id-state (atom 0))

(defn reset-ids! [& [last-id]]
  (reset! id-state (or last-id 0)))

(defn next-id! []
  (swap! id-state inc))

(defn give-id! [{:keys [id] :as card}]
  (cond-> card
          (nil? id) (assoc :id (next-id!))))

(defn peek-next-id []
  (inc @id-state))

(defn format-name [{:keys [card-name] :as n}]
  (cond
    card-name (format-name card-name)
    (and (keyword? n)
         (namespace n)) (str (format-name (namespace n))
                             " - "
                             (format-name (name n)))
    (keyword? n) (format-name (name n))
    (string? n) (-> n
                    (s/split #"[- ]")
                    (->> (map s/capitalize)
                         (s/join " ")))))

(defn format-name-short [n]
  (-> n
      name
      (s/split #"[- ]")
      (->> (map (comp first s/capitalize))
           s/join)))

(defn format-types [types]
  (->> types
       (map format-name)
       (s/join "/")))

(defn format-aether [{:keys [aether earmarked-aether restricted-aether]
                      :or   {aether 0}}]
  (let [extra-aether (->> earmarked-aether
                          (map (fn [[types val]]
                                 (str val (->> types
                                               (map (comp first name))
                                               sort
                                               (clojure.string/join)))))
                          sort
                          (clojure.string/join ","))
        minus-aether (->> restricted-aether
                          (map (fn [[types val]]
                                 (str val (->> types
                                               (map (comp first name))
                                               sort
                                               (clojure.string/join)))))
                          sort
                          (clojure.string/join ","))
        aether       (->> restricted-aether
                          vals
                          (apply + aether))]
    (str aether
         (when (not-empty extra-aether)
           (str "(+" extra-aether ")"))
         (when (not-empty minus-aether)
           (str "(-" minus-aether ")")))))

(defn format-cost [cost]
  (str "$" cost))

(defn format-breach-no [no]
  (case no
    0 "I"
    1 "II"
    2 "III"
    3 "IV"))

(defn number->text [n]
  (case n
    1 "one"
    2 "two"
    3 "three"
    4 "four"
    5 "five"
    6 "six"
    7 "seven"
    8 "eight"
    9 "nine"
    10 "ten"
    11 "eleven"
    12 "twelve"
    (str n)))

(defn redupeat [val n f & args]
  (loop [acc val n n]
    (if (> n 0)
      (recur (apply f acc args) (dec n))
      acc)))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (let [vcoll (vec coll)]
    (vec (concat (subvec vcoll 0 pos) (subvec vcoll (inc pos))))))

(defn coll-diff [coll1 coll2]
  (->>
    [coll1 coll2]
    (map frequencies)
    (apply merge-with -)
    (mapcat (fn [[x n]] (repeat n x)))))

(defn frequencies-of [coll key]
  (->> coll
       (map key)
       frequencies
       (into (sorted-map))))

(defn dissoc-if-empty [map key]
  (cond-> map
          (empty? (get map key)) (dissoc key)))

(defn match [data1]
  (fn [data2]
    (->> data1
         (every? (fn [[key val]]
                   (let [values (if (set? val) val #{val})]
                     (contains? values (get data2 key))))))))

(defn ensure-coll [data]
  (cond
    (coll? data) data
    data [data]
    :else []))

(defn count-as-coll [data]
  (-> data ensure-coll count))

(defn remove-top-card [pile]
  (update pile :pile-size dec))

(defn add-top-card [pile card]
  (update pile :pile-size inc))

(defn get-pile-idx [game card-name]
  (->> game
       :supply
       (keep-indexed (fn [idx pile]
                       (when ((comp #{card-name} :name :card) pile) (merge pile {:idx idx}))))
       first))

(defn get-card-idx [game path criteria]
  (let [select-fn (if (= :discard (last path))
                    last
                    first)]
    (->> (get-in game path)
         (keep-indexed (fn [idx card]
                         (when ((match criteria) card) {:idx idx :card card})))
         select-fn)))

(defn get-trigger-idx [game path criteria]
  (->> (get-in game path)
       (keep-indexed (fn [idx trigger]
                       (when ((match criteria) trigger) {:idx idx :trigger trigger})))
       first))

(defn update-in-vec [game path criteria f & args]
  (let [{:keys [idx]} (get-card-idx game path criteria)]
    (-> game
        (update-in path vec)
        (as-> game (apply update-in game (concat path [idx]) f args)))))

(defn plus [n m]
  (if n (+ n m) m))

(defn gt [a b]
  (> (or a -1) (or b -1)))

(defn and' [a b]
  (and a b))

(defn max' [& coll]
  (when (not-empty coll)
    (apply max coll)))

(defn minus-cost [cost reduction]
  (let [reduction (or reduction 0)]
    (if (< cost reduction) 0 (- cost reduction))))

(defn capitalism-get-types [{:keys [name types effects trigger] :as card}]
  (if (and
        (:action types)
        (or (some (fn [[effect {:keys [text options]}]]
                    (or (= :give-coins effect)
                        (and text (re-find #"\+\$" text))
                        (some (fn [{:keys [text]}]
                                (and text (re-find #"\+\$" text)))
                              options)))
                  (concat effects (:effects trigger)))
            (contains? #{:merchant
                         :baron :ironworks :courtier
                         :pirate-ship :salvager
                         :trade-route :city
                         :harvest :tournament :trusty-steed
                         :forager :storeroom :ironmonger :mercenary
                         :giant :miser :teacher
                         :chariot-race :farmers'-market :sacrifice
                         :scrap :bounty-hunter} name)))
    (conj types :treasure)
    types))

(defn get-types [{:keys [current-player] :as game} {:keys [types] :as card}]
  (let [player-no (or current-player 0)]
    (if (->> (get-in game [:projects :capitalism :participants])
             (some (comp #{player-no} :player-no)))
      (capitalism-get-types card)
      types)))

(defn- reduction-matches-card-types [{reduction-type :type} card-types]
  (or (nil? reduction-type) (reduction-type card-types)))

(defn- get-cost-with-reduction [game player-no {{:keys [coin-cost debt-cost] :as cost} :cost :as card}]
  (let [cost-reductions (->> (get-in game [:players player-no :play-area])
                             (mapcat (comp :cost-reductions :while-in-play))
                             (concat (:cost-reductions game)
                                     (get-in game [:players player-no :cost-reductions])))
        card-types      (get-types game card)
        base-cost       (if (int? cost)
                          {:coin-cost cost}
                          (merge {:coin-cost (or coin-cost 0)}
                                 (when debt-cost
                                   {:debt-cost debt-cost})))]
    (reduce (fn [card-cost {:keys [reduction] :as reduction-data}]
              (cond-> card-cost
                      (reduction-matches-card-types reduction-data card-types) (update :coin-cost minus-cost reduction)))
            base-cost
            cost-reductions)))

(defn get-buy-cost [game player-no {:keys [buy-cost] :as card}]
  (let [buy-cost-fn (when buy-cost (effects/get-effect buy-cost))]
    (get-cost-with-reduction game player-no (cond-> card
                                                    buy-cost-fn (assoc :cost (buy-cost-fn game {:player-no player-no}))))))

(defn get-cost [{:keys [current-player] :as game} card]
  (let [player-no (or current-player 0)
        {:keys [phase]} (get-in game [:players player-no])]
    (if (#{:pay :buy} phase)
      (get-buy-cost game player-no card)
      (get-cost-with-reduction game player-no card))))

(defn stay-in-play [game player-no {:keys [id]}]
  (let [{:keys [play-area triggers repeated-play]} (get-in game [:players player-no])
        card-ids-in-play      (->> play-area (keep :id) set)
        repeated-card-ids     (->> repeated-play
                                   (filter (comp #{id} :source))
                                   #_(filter (comp card-ids-in-play :target))
                                   (map :target)
                                   set)
        stay-in-play-triggers (filter (comp #{:at-start-turn :at-end-turn :play-action} :event) triggers)]
    (or (some (comp #{id} :card-id) stay-in-play-triggers)
        (some (comp repeated-card-ids :card-id) stay-in-play-triggers))))

(defn- can-react? [game player-no {:keys [react-pred]}]
  (if react-pred
    (let [can-react-fn (effects/get-effect react-pred)]
      (can-react-fn game player-no))
    true))

(defn types-match [game types card]
  (->> card
       (get-types game)
       (intersection types)
       not-empty))

(defn normalize-cost [{:keys [coin-cost debt-cost] :as cost}]
  (if (int? cost)
    {:coin-cost cost
     :debt-cost 0}
    {:coin-cost (or coin-cost 0)
     :debt-cost (or debt-cost 0)}))

(defn costs-up-to [max-cost card-cost]
  (let [{max-coin-cost :coin-cost
         max-debt-cost :debt-cost} (normalize-cost max-cost)
        {card-coin-cost :coin-cost
         card-debt-cost :debt-cost} (normalize-cost card-cost)]
    (and (<= card-coin-cost max-coin-cost)
         (<= card-debt-cost max-debt-cost))))

(defn costs-at-least [min-cost card-cost]
  (costs-up-to card-cost min-cost))

(defn costs-exactly [cost card-cost]
  (= (normalize-cost cost) (normalize-cost card-cost)))

(defn costs-less [max-cost+ card-cost]
  (and (costs-up-to max-cost+ card-cost)
       (not (costs-exactly max-cost+ card-cost))))

(defn add-to-cost [card-cost cost]
  (let [added-cost (if (int? cost)
                     {:coin-cost cost}
                     cost)]
    (merge-with + card-cost added-cost)))

(defn can-afford? [{:keys [aether earmarked-aether restricted-aether]
                    :or   {aether 0}}
                   cost type]
  (let [valid-aether (+ aether
                        (->> earmarked-aether
                             (keep (fn [[types aether]]
                                     (when (contains? types type)
                                       aether)))
                             (apply +))
                        (->> restricted-aether
                             (keep (fn [[types aether]]
                                     (when-not (contains? types type)
                                       aether)))
                             (apply +)))]
    (>= valid-aether cost)))

(defn get-prepped-spells [game {:keys [player-no breach-no]}]
  (concat (get-in game [:players player-no :breaches breach-no :prepped-spells])
          (when (pos? breach-no)
            (->> (get-in game [:players player-no :breaches (dec breach-no) :prepped-spells])
                 (filter :dual-breach)))))

(defn get-spells-in-closed-breaches [game player-no]
  (->> (get-in game [:players player-no :breaches])
       (keep-indexed (fn [breach-no {:keys [status]}]
                       (when (#{:closed :focused} status)
                         (get-prepped-spells game {:player-no player-no
                                                   :breach-no breach-no}))))
       (apply concat)))

(defn can-prep? [game {:keys [player-no breach-no card closed-breaches? opened-breaches?] :as args}]
  (if breach-no
    (let [{:keys [breach-capacity] :or {breach-capacity 1}} (get-in game [:players player-no])
          {:keys [status]} (get-in game [:players player-no :breaches breach-no])
          {:keys [dual-breach may-prep-to-closed-breach]} card
          may-prep-to-closed-breach (and (or may-prep-to-closed-breach
                                             closed-breaches?)
                                         (not opened-breaches?))
          breach-stati              (cond-> #{:opened}
                                            (not opened-breaches?) (conj :focused)
                                            may-prep-to-closed-breach (conj :closed))
          prepped-spells            (get-prepped-spells game {:player-no player-no
                                                              :breach-no breach-no})]
      (cond-> (or (and (contains? breach-stati status)
                       (empty? prepped-spells))
                  (and (= :opened status)
                       (< (count prepped-spells) breach-capacity)))
              dual-breach (and' (can-prep? game {:player-no        player-no
                                                 :breach-no        (inc breach-no)
                                                 :card             {}
                                                 :closed-breaches? may-prep-to-closed-breach
                                                 :opened-breaches? opened-breaches?}))))
    (let [number-of-breaches (->> (get-in game [:players player-no :breaches])
                                  count)]
      (->> (range number-of-breaches)
           (some (fn [breach-no]
                   (can-prep? game (assoc args :breach-no breach-no))))))))

(defn can-main? [game player-no]
  (let [{:keys [phase]} (get-in game [:players player-no])]
    (case phase
      :main true
      :casting (empty? (get-spells-in-closed-breaches game player-no))
      false)))

(defn- status-sort-order [status]
  (case status
    :destroyed 0
    :focused 1
    :closed 2
    :opened 3))

(defn- breach-no-sort-order [breach-no]
  (case breach-no
    0 2
    1 0
    2 1
    3 2))

(defn prepable-breaches [game {:keys [player-no card closed-breaches?]}]
  (let [breach-sort-values (->> (get-in game [:players player-no :breaches])
                                (map-indexed (fn [breach-no {:keys [status bonus-damage opened-effects]
                                                             :or   {bonus-damage 0}}]
                                               [(if (= :opened status) bonus-damage 0)
                                                (if (= :opened status) (count opened-effects) 0)
                                                (status-sort-order status)
                                                (breach-no-sort-order breach-no)]))
                                vec)]
    (->> (get-in game [:players player-no :breaches])
         (keep-indexed (fn [breach-no breach]
                         (when (can-prep? game {:player-no        player-no
                                                :breach-no        breach-no
                                                :card             card
                                                :closed-breaches? closed-breaches?})
                           (assoc breach :breach-no breach-no))))
         (sort-by (fn [{:keys [breach-no]}]
                    (cond->> (get breach-sort-values breach-no)
                             (:dual-breach card) (mapv + (get breach-sort-values (inc breach-no))))))
         reverse
         (map :breach-no))))

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

(defn get-card-strength [{:keys [text cast]}]
  (let [all-text (->> (ensure-coll text)
                      (concat (ensure-coll cast))
                      (string/join))]
    (count all-text)))

(defn get-nemesis-tier [{:keys [nemesis]}]
  (let [{:keys [play-area discard]} nemesis]
    (->> (concat play-area discard)
         (map :tier)
         (apply max 0))))

(defn options-from-player [game {:keys [player-no area card-id]}
                           & [{:keys [this id type not-type cost min-cost max-cost most-expensive can-prep
                                      lowest-focus-cost opened min-charges prepped-this-turn]}]]
  (case area
    :breaches (let [options  (->> (get-in game [:players player-no :breaches])
                                  (keep-indexed (fn [breach-no {:keys [status] :as breach}]
                                                  (when (not= :destroyed status)
                                                    (assoc breach :option {:player-no player-no
                                                                           :breach-no breach-no})))))
                    low-cost (->> options
                                  (filter (comp #{:closed :focused} :status))
                                  (keep :focus-cost)
                                  (apply min 20))]
                (cond->> options
                         lowest-focus-cost (filter (comp #{low-cost} :focus-cost))
                         opened (filter (comp #{:opened} :status))
                         (false? opened) (remove (comp #{:opened} :status))
                         :always (map :option)))
    :ability (let [charges (get-in game [:players player-no :ability :charges])]
               (when (or (nil? min-charges)
                         (and charges
                              (>= charges min-charges)))
                 [{:player-no player-no}]))
    :prepped-spells (let [prepped-this-turn? (fn prepped-this-turn? [{:keys [id]}]
                                               (->> (get-in game [:players player-no :this-turn])
                                                    (filter :prep)
                                                    (filter (comp #{id} :id))
                                                    not-empty
                                                    boolean))
                          options            (->> (get-in game [:players player-no :breaches])
                                                  (map-indexed (fn [breach-no {:keys [prepped-spells]}]
                                                                 (->> prepped-spells
                                                                      (map (fn [{:keys [name] :as card}]
                                                                             (assoc card :option {:player-no player-no
                                                                                                  :breach-no breach-no
                                                                                                  :card-name name}))))))
                                                  (apply concat))]
                      (cond->> options
                               prepped-this-turn (filter prepped-this-turn?)
                               min-cost (filter (comp #(>= % min-cost) :cost))
                               :always (map :option)))
    (let [cards        (get-in game [:players player-no area])
          highest-cost (->> cards
                            (keep :cost)
                            (apply max'))]
      (cond->> cards
               this (filter (comp #{card-id} :id))
               id (filter (comp #{id} :id))
               type (filter (comp #{type} :type))
               not-type (remove (comp #{not-type} :type))
               cost (filter (comp #(= % cost) :cost))
               min-cost (filter (fn [{:keys [cost]}]
                                  (and cost
                                       (<= min-cost cost))))
               max-cost (filter (fn [{:keys [cost]}]
                                  (and cost
                                       (<= cost max-cost))))
               most-expensive (filter (comp #(= highest-cost %) :cost))
               can-prep (filter (fn [card]
                                  (can-prep? game (merge {:player-no player-no
                                                          :card      card}
                                                         can-prep))))
               (#{:discard} area) (map (fn [{:keys [id]}]
                                         {:player-no player-no
                                          :card-id   id}))
               (not (#{:discard} area)) (map :name)))))

(effects/register-options {:player options-from-player})

(defn count-prepped-spells [{:keys [breaches]}]
  (->> breaches
       (mapcat :prepped-spells)
       count))

(defn count-opened-breaches [{:keys [breaches]}]
  (->> breaches
       (filter (comp #{:opened} :status))
       count))

(defn count-cards-in-hand [{:keys [hand]}]
  (count hand))

(defn count-non-corruption [{:keys [hand]}]
  (->> hand
       (remove (comp #{:corruption} :type))
       count))

(defn count-cards-in-deck-and-discard [{:keys [deck discard]}]
  (count (concat deck discard)))

(defn count-crystals [{:keys [hand]}]
  (->> hand
       (filter (comp #{:crystal} :name))
       count))

(defn options-from-players [{:keys [players difficulty] :as game} {:keys [player-no area card-id]}
                            & [{:keys [ally player-nos most-charges min-charges activation fully-charged
                                       min-number-of-prepped-spells min-hand lowest-life most-life can-heal min-deck+discard
                                       this last type cost min-cost max-cost can-prep
                                       most-expensive most-opened-breaches most-prepped-spells lowest-focus-cost most-crystals
                                       opened max-breach-no min-non-corruption]}]]
  (let [solo-play?      (= 1 (count players))
        highest-charge  (->> players
                             (map #(get-in % [:ability :charges] 0))
                             (apply max 0))
        highest-opened  (->> players
                             (map count-opened-breaches)
                             (apply max 0))
        highest-prepped (->> players
                             (map count-prepped-spells)
                             (apply max 0))
        low-life        (if solo-play?
                          (-> players first :life)
                          (->> players
                               (keep :life)
                               (filter pos?)                ; Exhausted players are spared
                               (apply min 20)))
        high-life       (->> players
                             (keep :life)
                             (apply max 0))
        max-crystals    (->> players
                             (map count-crystals)
                             (apply max 0))
        valid-players   (cond->> (map-indexed (fn [player-no player]
                                                (assoc player :player-no player-no)) players)
                                 (and ally
                                      (not solo-play?)) (remove (comp #{player-no} :player-no))
                                 player-nos (filter (comp player-nos :player-no))
                                 min-charges (filter (comp #(>= % min-charges) :charges :ability))
                                 most-charges (filter (comp #{highest-charge} :charges :ability))
                                 activation (filter (comp #{activation} :activation :ability))
                                 fully-charged (filter (fn [{{:keys [charges charge-cost]} :ability}]
                                                         (>= charges charge-cost)))
                                 (false? fully-charged) (remove (fn [{{:keys [charges charge-cost]} :ability}]
                                                                  (>= charges charge-cost)))
                                 most-opened-breaches (filter (comp #{highest-opened} count-opened-breaches))
                                 most-prepped-spells (filter (comp #{highest-prepped} count-prepped-spells))
                                 min-number-of-prepped-spells (filter (comp #(>= % min-number-of-prepped-spells) count-prepped-spells))
                                 min-hand (filter (comp #(<= min-hand %) count-cards-in-hand))
                                 min-non-corruption (filter (comp #(<= min-non-corruption %) count-non-corruption))
                                 min-deck+discard (filter (comp #(<= min-deck+discard %) count-cards-in-deck-and-discard))
                                 lowest-life (filter (comp #{low-life} :life))
                                 most-life (filter (comp #{high-life} :life))
                                 most-crystals (filter (comp #{max-crystals} count-crystals))
                                 can-heal (filter (fn [{:keys [life]}]
                                                    (< 0 life (player-starting-life difficulty)))))]
    (cond
      (#{:players :ability} area) (->> valid-players
                                       (map #(select-keys % [:player-no])))
      (= :breaches area) (let [options (->> valid-players
                                            (mapcat (fn [{:keys [player-no breaches]}]
                                                      (let [cheapest-breach-no (->> breaches
                                                                                    (keep-indexed (fn [breach-no {:keys [status]}]
                                                                                                    (when (#{:closed :focused} status)
                                                                                                      breach-no)))
                                                                                    first)]
                                                        (cond->> breaches
                                                                 :always (keep-indexed (fn do-breach [breach-no {:keys [status] :as breach}]
                                                                                         (when (not= :destroyed status)
                                                                                           (assoc breach :option {:player-no player-no
                                                                                                                  :breach-no breach-no}))))
                                                                 lowest-focus-cost (filter (comp #{cheapest-breach-no} :breach-no :option)))))))]
                           (cond->> options
                                    opened (filter (comp #{:opened} :status))
                                    (false? opened) (remove (comp #{:opened} :status))
                                    can-prep (filter (fn [{:keys [option]}]
                                                       (can-prep? game (merge option
                                                                              can-prep))))
                                    max-breach-no (filter (comp #(<= % max-breach-no) :breach-no :option))
                                    :always (map :option)))
      :else (let [options       (case area
                                  :discard (->> valid-players
                                                (mapcat (fn [{:keys [player-no discard]}]
                                                          (->> (cond->> discard
                                                                        last (take-last 1)) ; it's important that 'last' is evaluated first
                                                               (map (fn [{:keys [id] :as card}]
                                                                      (assoc card :option {:player-no player-no
                                                                                           :card-id   id})))))))
                                  :hand (->> valid-players
                                             (mapcat (fn [{:keys [player-no hand]}]
                                                       (->> hand
                                                            (map (fn [{:keys [name] :as card}]
                                                                   (assoc card :option {:player-no player-no
                                                                                        :card-name name})))))))
                                  :prepped-spells (->> valid-players
                                                       (mapcat (fn [{:keys [player-no breaches]}]
                                                                 (->> breaches
                                                                      (map-indexed (fn [breach-no breach]
                                                                                     (->> (:prepped-spells breach)
                                                                                          (map (fn [{:keys [name] :as card}]
                                                                                                 (assoc card :option {:player-no player-no
                                                                                                                      :breach-no breach-no
                                                                                                                      :card-name name}))))))
                                                                      (apply concat))))))
                  highest-costs (->> options
                                     (group-by (comp :player-no :option))
                                     (medley/map-vals (fn [cards]
                                                        (->> cards
                                                             (keep :cost)
                                                             (apply max 0)))))]
              (cond->> options
                       this (filter (comp #{card-id} :id))
                       type (filter (comp #{type} :type))
                       cost (filter (comp #{cost} :cost))
                       min-cost (filter (comp #(>= % min-cost) :cost))
                       max-cost (filter (comp #(<= % max-cost) :cost))
                       most-expensive (filter (fn [{:keys [cost option]}]
                                                (if (= most-expensive :per-player)
                                                  (= cost (get highest-costs (:player-no option)))
                                                  (= cost (->> highest-costs vals (apply max 0))))))
                       can-prep (filter (fn [{{:keys [player-no]} :option :as card}]
                                          (can-prep? game (merge {:player-no player-no
                                                                  :card      card}
                                                                 can-prep))))
                       :always (map :option))))))

(effects/register-options {:players options-from-players})

(defn options-from-nemesis [game {:keys [area]} & [{:keys [most-recent name type]}]]
  (let [{:keys [number-of-husks]} (get-in game [:nemesis :husks])
        husks? (and number-of-husks
                    (pos? number-of-husks))]
    (case area
      :minions (concat (->> (get-in game [:nemesis :play-area])
                            (filter (comp #{:minion} :type))
                            (map :name))
                       (when husks? [:husks]))
      :husks (when husks? [:husks])
      :play-area (cond->> (get-in game [:nemesis :play-area])
                          name (filter (comp #{name} :name))
                          :always (map :name))
      :revealed (->> (get-in game [:nemesis :revealed])
                     (map :name))
      :discard (cond->> (get-in game [:nemesis :discard])
                        type (filter (comp #{type} :type))
                        most-recent (take-last 1)           ; it's important that 'most-recent' is evaluated last
                        :always (map :name))

      :nemesis [:nemesis]
      [area])))

(effects/register-options {:nemesis options-from-nemesis})

(defn options-from-turn-order [{:keys [turn-order]} {:keys [area]} & [{:keys [type not-type player-non-wild]}]]
  (let [{:keys [deck revealed-cards]} turn-order
        cards (if (and (= :revealed area)
                       revealed-cards)
                (take revealed-cards deck)
                (get turn-order area))]
    (cond->> cards
             type (filter (comp #{type} :type))
             not-type (remove (comp #{not-type} :type))
             player-non-wild (filter (comp int? :player-no :type))
             :always (map :name))))

(effects/register-options {:turn-order options-from-turn-order})

(defn options-from-supply [{:keys [supply]} _
                           & [{:keys [type max-cost least-expensive all]}]]
  (let [piles    (cond->> supply
                          type (filter (comp #{type} :type :card))
                          max-cost (filter (comp #(<= % max-cost) :cost :card)))
        min-cost (when (not-empty piles)
                   (->> piles
                        (filter (comp pos? :pile-size))
                        (map (comp :cost :card))
                        (apply min 20)))]
    (cond->> piles
             least-expensive (filter (comp #{min-cost} :cost :card))
             (not all) (filter (comp pos? :pile-size))
             :always (map (comp :name :card)))))

(effects/register-options {:supply options-from-supply})

(defn special-options [_ _ & options]
  options)

(effects/register-options {:special special-options})

(defn get-opt-args [[opt-name & opt-args]]
  (if (-> opt-args first keyword?)
    opt-args
    (conj opt-args opt-name)))

(defn mixed-options [game {:keys [player-no card-id]} & options]
  (->> options
       (mapcat (fn [[opt-name :as option]]
                 (let [opt-fn (effects/get-option opt-name)
                       [area & opt-args] (get-opt-args option)]
                   (->> (apply opt-fn game {:player-no player-no :card-id card-id :area area} opt-args)
                        (map (fn [option]
                               (merge {:area area}
                                      (when player-no
                                        {:player-no player-no})
                                      (cond
                                        (map? option) option
                                        option {:card-name option}))))))))))

(effects/register-options {:mixed mixed-options})

(defn empty-supply-piles [{:keys [supply] :as game}]
  (->> supply
       (filter (comp zero? :pile-size))
       count))

(defn add-effect-args [new-args [effect args]]
  [effect (cond
            (map? args) (merge new-args args)
            args (merge new-args {:arg args})
            :else new-args)])
