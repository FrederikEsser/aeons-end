(ns aeons-end.operations
  (:require [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [clojure.string :as string]
            [medley.core :as medley]))

(defn game-over? [{:keys                  [real-game? players]
                   {gravehold-life :life} :gravehold
                   {nemesis-life :life
                    :keys        [name
                                  deck
                                  play-area
                                  discard
                                  victory-condition]
                    :as          nemesis} :nemesis
                   :as                    game}]
  (when real-game?
    (let [nemesis-victory-fn (when victory-condition
                               (effects/get-predicate victory-condition))]
      (cond
        (and nemesis-life
             (<= nemesis-life 0)) {:conclusion :victory
                                   :text       [(str (ut/format-name name) " has been defeated and all its minions retreat.")
                                                "Gravehold is safe - for now."]}
        (and nemesis
             (not-empty discard)
             (empty? deck)
             (empty? play-area)) {:conclusion :victory
                                  :text       [(str (ut/format-name name) " is exhausted and all its minions defeated.")
                                               "Gravehold is safe - for now."]}
        (and gravehold-life
             (<= gravehold-life 0)) {:conclusion :defeat
                                     :text       ["The last defenses are destroyed and Gravehold is overrun."]}
        (and (> (count players) 1)
             (every? :life players)
             (every? (comp zero? :life) players)) {:conclusion :defeat
                                                   :text       ["All breach mages are defeated and Gravehold is defenseless."]}
        nemesis-victory-fn (nemesis-victory-fn game)))))

(defn push-effect-stack [game {:keys [player-no card-id effects choice args]}]
  (cond-> game
          (or (not-empty effects)
              choice) (update :effect-stack (partial concat (cond effects (->> effects
                                                                               (map (fn [effect]
                                                                                      (when effect
                                                                                        (merge {:player-no player-no
                                                                                                :effect    effect}
                                                                                               (when card-id
                                                                                                 {:card-id card-id})
                                                                                               (when args
                                                                                                 {:args args})))))
                                                                               (remove nil?))
                                                                  choice [(merge choice
                                                                                 (when player-no
                                                                                   {:player-no player-no})
                                                                                 (when card-id
                                                                                   {:card-id card-id}))])))))

(defn pop-effect-stack [{:keys [effect-stack] :as game}]
  (if (= 1 (count effect-stack))
    (dissoc game :effect-stack)
    (update game :effect-stack (partial drop 1))))

(defn do-effect [game {:keys              [player-no card-id args]
                       [name inline-args] :effect}]
  (let [effect-fn (effects/get-effect name)
        args      (merge {:player-no player-no}
                         args
                         (when card-id
                           {:card-id card-id})
                         (cond (map? inline-args) inline-args
                               inline-args {:arg inline-args}))]
    (effect-fn game args)))

(defn check-stack [game]
  (if (game-over? game)
    (-> game
        (dissoc :effect-stack :current-player)
        (medley/update-existing :players (partial map (fn [{:keys [deck] :as player}]
                                                        (assoc player :revealed-cards (count deck)))))
        (assoc :game-over (game-over? game)))
    (let [[{:keys [player-no card-id effect args] :as top}] (get game :effect-stack)]
      (cond-> game
              effect (-> pop-effect-stack
                         (do-effect {:player-no player-no
                                     :card-id   card-id
                                     :effect    effect
                                     :args      args})
                         check-stack)))))

(defn get-effects-from-trigger [{:keys [id effects card-id set-aside]}]
  (let [effect-args (merge {:trigger-id id
                            :card-id    card-id}
                           (when set-aside
                             {:set-aside set-aside}))]
    (concat (map (partial ut/add-effect-args effect-args) effects)
            [[:remove-trigger {:trigger-id id}]])))

(defn- get-trigger-effects [triggers]
  (let [complex?        (some (comp #{:complex} :mode) triggers)
        auto-triggers   (filter (fn [{:keys [mode]}]
                                  (or (nil? mode) (#{:auto (when-not complex? :semi)} mode))) triggers)
        manual-triggers (filter (comp #{(when complex? :semi) :manual :complex} :mode) triggers)
        trigger-names   (->> manual-triggers (map :name) set)]
    (concat (mapcat get-effects-from-trigger auto-triggers)
            (if (or (< 1 (count manual-triggers))
                    (some :optional? manual-triggers))
              (let [phase-change (->> triggers first :event)
                    text         (case phase-change
                                   :at-clean-up "You may activate cards, that do something when you discard them from play."
                                   :at-start-turn (str (-> (count manual-triggers)
                                                           ut/number->text
                                                           string/capitalize)
                                                       " things happen at the start of your turn. Select which one happens next.")
                                   "Do something!")]
                [[:give-choice (merge {:text    text
                                       :choice  [:simultaneous-effects-choice {:triggers manual-triggers}]
                                       :options [:mixed
                                                 [:player :play-area {:names trigger-names}]
                                                 [:player :tavern-mat {:names trigger-names}]
                                                 [:player :boons {:names trigger-names}]
                                                 [:artifacts {:names trigger-names}]
                                                 [:projects {:names trigger-names}]
                                                 [:events {:names trigger-names}]]
                                       :max     1}
                                      (when (not-every? :optional? manual-triggers)
                                        {:min 1}))]])
              (mapcat get-effects-from-trigger manual-triggers)))))

(defn simultaneous-effects-choice [game {:keys [player-no triggers choice]}]
  (if choice
    (let [[trigger & more-triggers] (->> triggers
                                         (sort-by (comp not #{(:card-name choice)} :name)))]
      (push-effect-stack game {:player-no player-no
                               :effects   (concat (get-effects-from-trigger trigger)
                                                  (get-trigger-effects more-triggers))}))
    game))

(effects/register {:simultaneous-effects-choice simultaneous-effects-choice})

(defn get-phase-change-effects [game {:keys [player-no phase-change]}]
  (when phase-change
    (let [while-prepped-effects (->> (get-in game [:players player-no :breaches])
                                     (mapcat :prepped-spells)
                                     (mapcat (comp phase-change :while-prepped)))
          nemesis-card-effects  (->> (get-in game [:nemesis :play-area])
                                     (mapcat phase-change))
          nemesis-effects       (get-in game [:nemesis phase-change])]
      (concat while-prepped-effects
              nemesis-card-effects
              nemesis-effects))))

(def phase-order [:out-of-turn
                  :casting
                  :main
                  :draw
                  :out-of-turn])

(defn next-phase [phase]
  (let [phase-index (->> phase-order
                         (keep-indexed (fn [idx p]
                                         (when (= p phase) idx)))
                         first)]
    (assert phase-index (str "Phase " phase " is not placed in the phase order."))
    (get phase-order (inc phase-index))))

(defn set-phase [game {:keys [player-no phase]}]
  (let [current-phase (get-in game [:players player-no :phase])]
    (if (and current-phase (not= current-phase phase))
      (let [next-phase                (next-phase current-phase)
            phase-change              (cond (#{:casting} next-phase) :at-start-casting
                                            (#{:casting} current-phase) :at-end-casting
                                            (#{:main} current-phase) :at-end-main
                                            (#{:draw} current-phase) :at-end-draw)
            spells-in-closed-breaches (ut/get-spells-in-closed-breaches game player-no)]
        (assert (-> (drop-while (comp not #(= current-phase %)) phase-order)
                    set
                    (contains? phase)) (str "Phase error: You can't go from the " (ut/format-name current-phase)
                                            " phase to the " (ut/format-name phase) " phase."))
        (when (= :casting current-phase)
          (assert (empty? spells-in-closed-breaches) (str "Phase error: You can't go to the " (ut/format-name phase)
                                                          " phase while you have prepped spells in closed breaches: "
                                                          (->> spells-in-closed-breaches
                                                               (map :name)
                                                               ut/format-types))))
        (-> game
            (assoc-in [:players player-no :phase] next-phase)
            (push-effect-stack {:player-no player-no
                                :effects   (concat (get-phase-change-effects game {:player-no    player-no
                                                                                   :phase-change phase-change})
                                                   (when (not= next-phase phase)
                                                     [[:set-phase {:phase phase}]]))})))
      game)))

(effects/register {:set-phase set-phase})

(defn remove-trigger [game {:keys [player-no trigger-id card-id]}]
  (-> game
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:id trigger-id})
                                                                            (comp #{:once :once-turn} :duration))))
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:id trigger-id})
                                                                            (comp #{:until-empty} :duration)
                                                                            (comp empty? :set-aside))))
      (cond->
        card-id (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:card-id card-id})
                                                                                      (comp #{:attack} :duration)))))
      (update-in [:players player-no] ut/dissoc-if-empty :triggers)))

(defn remove-triggers [game {:keys [player-no event]}]
  (-> game
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:event event})
                                                                            (comp #{:once :once-turn} :duration))))
      (update-in [:players player-no :triggers] (partial remove (every-pred (ut/match {:event event})
                                                                            (comp #{:until-empty} :duration)
                                                                            (comp empty? :set-aside))))
      (update-in [:players player-no] ut/dissoc-if-empty :triggers)))

(defn- apply-triggers
  ([game {:keys [player-no event] :as args}]
   (apply-triggers game player-no event args))
  ([game player-no event & [args]]
   (let [triggers          (get-in game [:players player-no :triggers])
         matching-triggers (cond->> (filter (comp #{event} :event) triggers)
                                    (= :instead-of-first-action event) (take-last 1)) ; only one effect should happen instead of "The first time you play an Action"
         apply-trigger     (fn [game {:keys [id card-id effects duration]}]
                             (push-effect-stack game {:player-no player-no
                                                      :card-id   card-id
                                                      :effects   (concat effects
                                                                         (when (#{:once :once-turn} duration)
                                                                           [[:remove-trigger {:trigger-id id}]]))
                                                      :args      args}))]
     (-> (reduce apply-trigger game (reverse matching-triggers))))))

(effects/register {:remove-trigger  remove-trigger
                   :remove-triggers remove-triggers
                   :apply-triggers  apply-triggers})

(defn increase-revealed-number-of-cards [game player-no]
  (-> game
      (update-in [:players player-no :revealed-cards] ut/plus 1)))

(defn decrease-revealed-number-of-cards [game player-no]
  (let [revealed-cards (get-in game [:players player-no :revealed-cards])]
    (if (and revealed-cards
             (< 1 revealed-cards))
      (assoc-in game [:players player-no :revealed-cards] (dec revealed-cards))
      (update-in game [:players player-no] dissoc :revealed-cards))))

(defn state-maintenance [game player-no from to]
  (let [from-cards (if player-no
                     (get-in game [:players player-no from])
                     (get-in game [:nemesis from]))]
    (cond-> game
            (and (= from :deck) (:can-undo? game)) (assoc :can-undo? false)
            player-no (cond->
                        (= to :deck) (increase-revealed-number-of-cards player-no)
                        (= from :deck) (decrease-revealed-number-of-cards player-no)
                        (empty? from-cards) (update-in [:players player-no] dissoc from)
                        (= from :breach) (update-in [:players player-no :breaches] (fn [breaches]
                                                                                     (->> breaches
                                                                                          (mapv (fn [{:keys [prepped-spells] :as breach}]
                                                                                                  (cond-> breach
                                                                                                          (empty? prepped-spells) (dissoc :prepped-spells))))))))
            (nil? player-no) (cond->
                               (empty? from-cards) (update :nemesis dissoc from))
            (and (= :trash from)
                 (-> game :trash empty?)) (dissoc :trash))))

(defn- get-card [game {:keys [player-no card-name move-card-id from from-position from-player breach-no] :as args}]
  (assert (or card-name move-card-id from-position) (str "Can't move unspecified card: " args))
  (when (= :breach from)
    (assert breach-no (str "Can't move card from breach without breach-no: " args)))
  (if (#{:supply} from)
    (let [{:keys [card idx pile-size]} (ut/get-pile-idx game card-name)]
      (when (and pile-size (pos? pile-size))
        {:card      (ut/give-id! card)
         :from-path from
         :idx       idx}))
    (let [player-no (or from-player player-no)
          from-path (cond
                      (= :trash from) [:trash]
                      (and player-no
                           (= :breach from)) [:players player-no :breaches breach-no :prepped-spells]
                      player-no [:players player-no from]
                      (nil? player-no) [:nemesis from])]
      (merge {:from-path from-path}
             (case from-position
               :bottom {:idx (dec (count (get-in game from-path))) :card (last (get-in game from-path))}
               :top {:idx 0 :card (first (get-in game from-path))}
               (cond
                 move-card-id (ut/get-card-idx game from-path {:id move-card-id})
                 card-name (ut/get-card-idx game from-path {:name card-name})
                 :else {:idx 0 :card (first (get-in game from-path))}))))))

(defn- remove-card [game from-path idx]
  (if (#{:supply} from-path)
    (update-in game [from-path idx] ut/remove-top-card)
    (update-in game from-path ut/vec-remove idx)))

(defn add-card [game to-path to-position {:keys [name] :as card}]
  (let [add-card-to-coll (fn [coll card]
                           (let [coll (vec coll)]
                             (if (empty? coll)
                               [card]
                               (cond
                                 (= :top to-position) (concat [card] coll)
                                 (integer? to-position) (concat (subvec coll 0 to-position)
                                                                [card]
                                                                (subvec coll to-position (count coll)))
                                 :else (vec (concat coll [card]))))))]
    (if (#{:supply} to-path)
      (let [{:keys [idx]} (ut/get-pile-idx game name)]
        (update-in game [to-path idx] ut/add-top-card card))
      (update-in game to-path add-card-to-coll card))))

(defn get-on-gain-effects [game player-no {:keys [name on-gain] :as card}]
  (let [{:keys [tokens]} (ut/get-pile-idx game name)
        types                 (ut/get-types game card)
        token-effects         (->> tokens
                                   vals
                                   (mapcat (fn [{:keys [number-of-tokens on-gain]}]
                                             (when on-gain
                                               (apply concat (repeat number-of-tokens on-gain))))))
        while-in-play-effects (->> (get-in game [:players player-no :play-area])
                                   (mapcat (comp :on-gain :while-in-play))
                                   (map (partial ut/add-effect-args {:card-name name})))
        trigger-effects       (->> (get-in game [:players player-no :triggers])
                                   (filter (fn [{:keys [event type]}]
                                             (and (= :on-gain event)
                                                  (or (not type)
                                                      (contains? types type)))))
                                   (mapcat (fn [{:keys [id card-id effects]}]
                                             (cond->> effects
                                                      card-id (map (partial ut/add-effect-args {:trigger-id id
                                                                                                :card-id    card-id}))))))]
    (concat on-gain while-in-play-effects trigger-effects token-effects)))

(defn track-gain [{:keys [real-game? current-player] :as game} {:keys [player-no card]}]
  (cond-> game
          (and real-game?
               (or (nil? current-player)
                   (= current-player player-no))) (update-in [:players player-no :this-turn]
                                                             concat [{:gain (:name card)}])))

(defn handle-on-gain [game {:keys [player-no gained-card-id from]
                            :or   {from :supply}
                            :as   args}]
  (let [{{:keys [name] :as card} :card} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
        on-gain-effects (->> (get-on-gain-effects game player-no card)
                             (map (partial ut/add-effect-args (merge args
                                                                     {:card-name      name
                                                                      :gained-card-id gained-card-id}))))]
    (cond-> game
            card (push-effect-stack (merge args {:effects (concat on-gain-effects
                                                                  [[:remove-triggers {:event :on-gain}]
                                                                   [:track-gain {:card card}]])})))))

(declare move-card)

(defn finalize-gain [game {:keys [player-no gained-card-id to to-position]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :gaining] {:id gained-card-id})
        to (or to (:gain-to card) :discard)]
    (cond-> game
            card (move-card {:player-no    player-no
                             :move-card-id gained-card-id
                             :from         :gaining
                             :to           to
                             :to-position  to-position}))))

(defn gain [game {:keys [player-no card-name from to no-choice?]
                  :or   {from :supply
                         to   :discard}
                  :as   args}]
  (if (and card-name
           (not no-choice?))
    (let [{:keys [card from-path idx]} (get-card game {:player-no player-no
                                                       :card-name card-name
                                                       :from      from})
          gain-args (merge args
                           {:gained-card-id (:id card)})]
      (cond-> game
              card (-> (remove-card from-path idx)
                       (add-card [:players player-no :gaining] :top (dissoc card :face))
                       (push-effect-stack (merge args {:effects [[:on-gain gain-args]
                                                                 [:finalize-gain gain-args]]}))
                       check-stack)))
    game))

(defn overpay-choice [game {:keys [player-no amount effect]}]
  (if (pos? amount)
    (-> game
        (update-in [:players player-no :coins] - amount)
        (push-effect-stack {:player-no player-no
                            :effects   [[effect {:amount amount}]]}))
    game))

(effects/register {:track-gain     track-gain
                   :on-gain        handle-on-gain
                   :finalize-gain  finalize-gain
                   :gain           gain
                   :overpay-choice overpay-choice})

(defn pay [game {:keys [player-no amount type]}]
  (let [{:keys [earmarked-aether restricted-aether] :as player} (get-in game [:players player-no])]
    (assert (ut/can-afford? player amount type) (str "Pay error: You can't pay " amount " aether, when you only have " (ut/format-aether player) "."))
    (let [{:keys [amount earmarked]} (->> earmarked-aether
                                          (sort-by (comp count first))
                                          (reduce (fn [{:keys [amount earmarked]}
                                                       [types aether]]
                                                    (if (contains? types type)
                                                      {:amount    (max 0 (- amount aether))
                                                       :earmarked (cond-> earmarked
                                                                          (> aether amount) (assoc types (- aether amount)))}
                                                      {:amount    amount
                                                       :earmarked (assoc earmarked types aether)}))
                                                  {:amount amount}))
          {:keys [amount restricted]} (->> restricted-aether
                                           (sort-by (comp count first) >)
                                           (reduce (fn [{:keys [amount restricted]}
                                                        [types aether]]
                                                     (if (contains? types type)
                                                       {:amount     amount
                                                        :restricted (assoc restricted types aether)}
                                                       {:amount     (max 0 (- amount aether))
                                                        :restricted (cond-> restricted
                                                                            (> aether amount) (assoc types (- aether amount)))}))
                                                   {:amount amount}))]
      (-> game
          (update-in [:players player-no :aether] - amount)
          (as-> game
                (if earmarked
                  (assoc-in game [:players player-no :earmarked-aether] earmarked)
                  (update-in game [:players player-no] dissoc :earmarked-aether)))
          (as-> game
                (if restricted
                  (assoc-in game [:players player-no :restricted-aether] restricted)
                  (update-in game [:players player-no] dissoc :restricted-aether)))))))

(effects/register {:pay pay})

(defn gain-charges [game {:keys [player-no arg]}]
  (let [{:keys [charges charge-cost]
         :or   {charges 0}} (get-in game [:players player-no :ability])]
    (cond-> game
            (< charges charge-cost) (assoc-in [:players player-no :ability :charges] (min (+ arg charges) charge-cost)))))

(defn gain-charge [game {:keys [player-no]}]
  (gain-charges game {:player-no player-no
                      :arg       1}))

(effects/register {:gain-charges gain-charges
                   :gain-charge  gain-charge})

(defn spend-charges [game {:keys [player-no arg]}]
  (if arg
    (update-in game [:players player-no :ability :charges] - arg)
    (assoc-in game [:players player-no :ability :charges] 0)))

(effects/register {:spend-charges spend-charges})

(defn activate-ability [{:keys [current-player nemesis] :as game} {:keys [player-no]}]
  (if player-no
    (let [{:keys [ability phase]} (get-in game [:players player-no])
          {:keys [name activation charges charge-cost effects]
           :or   {charges 0}} ability]
      (when (and phase current-player)
        (assert (case activation
                  :your-main-phase (= :main phase)
                  :any-main-phase (= :main (get-in game [:players current-player :phase]))
                  :nemesis-draw (= :draw (:phase nemesis))
                  :turn-order-drawn (and (= :no-one current-player)
                                         (= :out-of-turn phase)))
                (str "Activate error: " (ut/format-name name) " can't be activated in the " (ut/format-name phase) " phase.")))
      (assert (and charge-cost (>= charges charge-cost)) (str "Activate error: " (ut/format-name name) " is not fully charged (" charges "/" charge-cost ")"))
      (push-effect-stack game {:player-no player-no
                               :effects   (concat [[:spend-charges]]
                                                  effects)}))
    game))

(effects/register {:activate-ability activate-ability})

(defn open-breach [game {:keys [player-no breach-no]}]
  (let [{:keys [status]} (get-in game [:players player-no :breaches breach-no])
        while-prepped-effects (->> (get-in game [:players player-no :breaches])
                                   (mapcat :prepped-spells)
                                   (mapcat (comp :at-open-breach :while-prepped)))]
    (assert (#{:closed :focused} status) (str "Open error: Breach " breach-no " has status " status "."))
    (-> game
        (assoc-in [:players player-no :breaches breach-no :status] :opened)
        (update-in [:players player-no :breaches breach-no] dissoc :focus-cost :open-costs :stage)
        (update-in [:players player-no] dissoc :breach-cost-reduction)
        (cond-> while-prepped-effects (push-effect-stack {:player-no player-no
                                                          :effects   while-prepped-effects})))))

(defn focus-breach [{:keys [current-player] :as game} {:keys [player-no breach-no]}]
  (let [{:keys [status stage]} (get-in game [:players player-no :breaches breach-no])
        current-player?       (or (nil? current-player)
                                  (= current-player player-no))
        while-prepped-effects (->> (get-in game [:players player-no :breaches])
                                   (mapcat :prepped-spells)
                                   (mapcat (comp :at-focus-breach :while-prepped)))]
    (assert (#{:closed :focused} status) (str "Focus error: Breach " breach-no " has status " status "."))
    (if (< stage 3)
      (-> game
          (update-in [:players player-no :breaches breach-no :stage] ut/plus 1)
          (update-in [:players player-no] dissoc :breach-cost-reduction)
          (cond-> current-player? (assoc-in [:players player-no :breaches breach-no :status] :focused)
                  while-prepped-effects (push-effect-stack {:player-no player-no
                                                            :effects   while-prepped-effects})))
      (-> game
          (open-breach {:player-no player-no
                        :breach-no breach-no})))))

(effects/register {:focus-breach focus-breach
                   :open-breach  open-breach})

(defn flip-discard [game {:keys [player-no]}]
  (let [{:keys [discard]} (get-in game [:players player-no])]
    (-> game
        (cond-> (not-empty discard) (update-in [:players player-no :deck] concat discard))
        (update-in [:players player-no] dissoc :discard))))

(defn shuffle-deck [game {:keys [player-no]}]
  (let [deck (get-in game [:players player-no :deck])]
    (cond-> game
            deck (update-in [:players player-no :deck] shuffle))))

(effects/register {:flip-discard flip-discard
                   :shuffle-deck shuffle-deck})

(defn peek-deck [game {:keys [player-no arg]}]
  (let [{:keys [deck discard]} (get-in game [:players player-no])]
    (cond-> game
            (and (< (count deck) arg) (not-empty discard)) (flip-discard {:player-no player-no}))))

(effects/register {:peek-deck peek-deck})

(defn do-move-card [game {:keys [player-no card from-path idx card-name from from-player to to-position to-player breach-no]}]
  (let [to-path (if player-no
                  (case to
                    :breach [:players (or to-player player-no) :breaches breach-no :prepped-spells]
                    :trash [:trash]
                    :supply :supply
                    [:players (or to-player player-no) to])
                  [:nemesis to])]
    (when card-name
      (assert card (str "Move error: There is no " (ut/format-name card-name) " in " from-path ".")))
    (cond-> game
            card (-> (remove-card from-path idx)
                     (add-card to-path to-position card)
                     (state-maintenance (or from-player player-no) from to)))))

(defn handle-on-trash [game {:keys [player-no card-id card-name destroyed-by] :as args}]
  (let [{{:keys [on-trash]} :card} (ut/get-card-idx game [:trash] {:name card-name})
        on-trash-triggers (->> (get-in game [:players player-no :triggers])
                               (filter (comp #{:on-trash} :event))
                               (mapcat :effects)
                               (map (partial ut/add-effect-args args)))
        reaction-effects  (->> (get-in game [:players player-no :hand])
                               (mapcat (comp :on-trash :reaction))
                               (map (partial ut/add-effect-args args)))
        on-trash-effects  (concat on-trash-triggers reaction-effects on-trash)]
    (cond-> game
            (not-empty on-trash-effects) (push-effect-stack {:player-no player-no
                                                             :card-id   card-id
                                                             :args      (when destroyed-by
                                                                          {:destroyed-by destroyed-by})
                                                             :effects   on-trash-effects}))))

(defn move-card [game {:keys [player-no from to] :as args}]
  (let [{:keys [deck discard]} (get-in game [:players player-no])
        {:keys [card] :as card-info} (get-card game args)]
    (if (and player-no
             (= :deck from)
             (empty? deck)
             (not-empty discard))
      (push-effect-stack game {:player-no player-no
                               :effects   [[:flip-discard]
                                           [:move-card args]]})
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   [[:do-move-card (merge args card-info)]
                                          (when (= to :trash)
                                            [:on-trash (merge args {:card-name (:name card)
                                                                    :card-id   (:id card)})])]})
          check-stack))))

(defn move-cards [game {:keys [player-no card-name card-names number-of-cards from-position] :as args}]
  (assert (or card-name
              card-names
              (and number-of-cards from-position)) (str "Can't move unspecified cards: " args))
  (if number-of-cards
    (cond-> game
            (pos? number-of-cards)
            (push-effect-stack {:player-no player-no
                                :effects   (repeat number-of-cards [:move-card (dissoc args :number-of-cards)])}))
    (let [card-names (or card-names [card-name])]
      (cond-> game
              (not-empty card-names)
              (push-effect-stack {:player-no player-no
                                  :effects   (map (fn [card-name]
                                                    [:move-card (-> args
                                                                    (dissoc :card-names)
                                                                    (assoc :card-name card-name))])
                                                  card-names)})))))

(effects/register {:do-move-card do-move-card
                   :on-trash     handle-on-trash
                   :move-card    move-card
                   :move-cards   move-cards})

(defn draw [game {:keys [player-no arg]}]
  (move-cards game {:player-no       player-no
                    :number-of-cards arg
                    :from            :deck
                    :from-position   :top
                    :to              :hand}))

(effects/register {:draw draw})

(defn affect-other-players [{:keys [players] :as game} {:keys [player-no card-id effects all at-once]}]
  (if (= 1 (count players))
    (push-effect-stack game {:player-no player-no
                             :effects   (cond->> effects
                                                 card-id (map (partial ut/add-effect-args {:card-id card-id})))})
    (let [player-no  (or player-no 0)
          player-nos (cond-> (->> (range 1 (count players))
                                  (map (fn [n] (-> n (+ player-no) (mod (count players))))))
                             (not at-once) reverse
                             all (concat [player-no]))]
      (reduce (fn [game other-player-no]
                (push-effect-stack game {:player-no other-player-no
                                         :effects   (cond->> effects
                                                             card-id (map (partial ut/add-effect-args {:card-id card-id})))}))
              game
              player-nos))))

(defn affect-all-players [game args]
  (affect-other-players game (assoc args :all true)))

(effects/register {:other-players affect-other-players
                   :all-players   affect-all-players})

(defn on-prep-spell [{:keys [real-game?] :as game} {:keys [player-no card]}]
  (let [{:keys [id name]} card
        this-turn (get-in game [:players player-no :this-turn])]
    (cond-> game
            real-game? (update-in [:players player-no :this-turn] concat [{:prep name :id id}])
            this-turn (update-in [:players player-no :this-turn] (partial remove (comp #{id} :while-prepped))))))

(defn prep-spell [game {:keys [player-no breach-no card-name closed-breaches?]}]
  (if card-name
    (let [{{:keys [type] :as card} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
          breach-no (or breach-no
                        (first (ut/prepable-breaches game {:player-no        player-no
                                                           :card             card
                                                           :closed-breaches? closed-breaches?})))]
      (assert card (str "Prep error: There is no " (ut/format-name card-name) " in your Hand."))
      (assert (= :spell type) (str "Prep error: You can't prep " (ut/format-name card-name) ", which has type " (ut/format-name type) "."))
      (-> game
          (push-effect-stack {:player-no player-no
                              :effects   [[:move-card {:card-name card-name
                                                       :from      :hand
                                                       :to        :breach
                                                       :breach-no breach-no}]
                                          [:on-prep-spell {:card card}]]})))
    game))

(effects/register {:on-prep-spell on-prep-spell
                   :prep-spell    prep-spell})

(defn track-this-turn [game {:keys [player-no cast]}]
  (update-in game [:players player-no :this-turn] concat [(cond
                                                            cast {:cast cast})]))

(effects/register {:track-this-turn track-this-turn})

(defn- get-breach-effects [{:keys [status bonus-damage opened-effects]}]
  {:bonus-damage   (or (and (= :opened status)
                            bonus-damage)
                       0)
   :opened-effects (when (= :opened status)
                     opened-effects)})

(defn spell-effect [{:keys [:real-game?] :as game} {:keys [player-no breach-no card-name card caster additional-damage]}]
  (let [{:keys [id effects dual-breach]} (or card
                                             (:card (ut/get-card-idx game [:players player-no :breaches breach-no :prepped-spells] {:name card-name})))
        {:keys [bonus-damage opened-effects]} (get-breach-effects (get-in game [:players player-no :breaches breach-no]))
        adjacent-breach (when dual-breach
                          (get-breach-effects (get-in game [:players player-no :breaches (inc breach-no)])))
        bonus-damage    (cond-> bonus-damage
                                additional-damage (+ additional-damage)
                                dual-breach (+ (:bonus-damage adjacent-breach)))]
    (-> game
        (push-effect-stack {:player-no (or caster player-no)
                            :card-id   id
                            :args      {:bonus-damage     bonus-damage
                                        :breach-player-no player-no
                                        :breach-no        breach-no}
                            :effects   (concat opened-effects
                                               (when dual-breach
                                                 (:opened-effects adjacent-breach))
                                               effects
                                               (when real-game?
                                                 [[:track-this-turn {:cast card-name}]]))}))))

(defn cast-spell [game {:keys [player-no breach-no card-name] :as args}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :breaches breach-no :prepped-spells] {:name card-name})]
    (cond-> game
            card-name (spell-effect args)
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:move-card {:card-name card-name
                                                              :from      :breach
                                                              :breach-no breach-no
                                                              :to        :discard}]]}))))

(defn cast-spell-in-hand [{:keys [:real-game?] :as game} {:keys [player-no card-name additional-damage]}]
  (let [{{:keys [id effects]} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (cond-> game
            card-name (push-effect-stack {:player-no player-no
                                          :card-id   id
                                          :args      {:bonus-damage (or additional-damage 0)}
                                          :effects   (concat [[:move-card {:card-name card-name
                                                                           :from      :hand
                                                                           :to        :discard}]]
                                                             effects
                                                             (when real-game?
                                                               [[:track-this-turn {:cast card-name}]]))}))))

(effects/register {:spell-effect       spell-effect
                   :cast-spell         cast-spell
                   :cast-spell-in-hand cast-spell-in-hand})

(defn call-predicate [game {:keys [player-no breach-no predicate]}]
  (let [{:keys [pred args]} (if (vector? predicate)
                              {:pred (first predicate)
                               :args (second predicate)}
                              {:pred predicate})
        pred-fn (when pred
                  (effects/get-predicate pred))]
    (when pred-fn
      (pred-fn game (merge {:player-no player-no
                            :breach-no breach-no}
                           args)))))

(defn can-use-while-prepped? [game {:keys [player-no breach-no card]}]
  (let [{:keys [phase this-turn]} (get-in game [:players player-no])
        {:keys [id while-prepped]} card
        {:keys [once can-use?]} while-prepped]
    (cond-> while-prepped
            phase (ut/and' (= phase (:phase while-prepped)))
            once (ut/and' (not (some (comp #{id} :while-prepped) this-turn)))
            can-use? (ut/and' (call-predicate game {:player-no player-no
                                                    :breach-no breach-no
                                                    :predicate can-use?})))))

(defn use-while-prepped [game {:keys [player-no breach-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :breaches breach-no :prepped-spells] {:name card-name})
        {:keys [id while-prepped]} card
        {:keys [once effects]} while-prepped]
    (when once
      (assert (can-use-while-prepped? game {:player-no player-no
                                            :breach-no breach-no
                                            :card      card}) (str "While prepped error: " (ut/format-name card-name) " can only be used once per turn.")))
    (-> game
        (update-in [:players player-no :this-turn] concat [{:while-prepped id}])
        (push-effect-stack {:player-no player-no
                            :args      {:breach-no breach-no}
                            :effects   effects}))))

(effects/register {:use-while-prepped use-while-prepped})

(defn can-discard? [game {:keys [player-no card]}]
  (let [predicate (-> card :to-discard :predicate)]
    (call-predicate game {:player-no player-no
                          :predicate predicate})))

(defn discard-power-card [game {:keys [player-no card-name]}]
  (let [{:keys [card]} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
        {:keys [text effects]} (:to-discard card)]
    (assert (can-discard? game {:player-no player-no
                                :card      card}) (str "Resolve TO DISCARD error: " (ut/format-name card-name)
                                                       " can't be discarded, because you can't '" text "'."))
    (-> game
        (push-effect-stack {:player-no player-no
                            :effects   (concat [[:set-phase {:phase :main}]]
                                               effects
                                               [[:discard-nemesis-card {:card-name card-name}]])}))))

(effects/register {:discard-power-card discard-power-card})

(defn play-all-gems [game {:keys [player-no]}]
  (let [{:keys [name] :as card} (->> (get-in game [:players player-no :hand])
                                     (filter (comp #{:gem} :type))
                                     (sort-by (fn [{:keys [auto-play-index]}] (or auto-play-index 0)))
                                     first)]
    (cond-> game
            card (push-effect-stack {:player-no player-no
                                     :effects   [[:move-card {:card-name name
                                                              :from      :hand
                                                              :to        :play-area}]
                                                 [:card-effect {:card card}]
                                                 [:play-all-gems]]}))))

(effects/register {:play-all-gems play-all-gems})

(defn- get-choice-fn [data]
  (let [{:keys [choice] :as result} (if (vector? data)
                                      {:choice (first data)
                                       :args   (second data)}
                                      {:choice data})]
    (merge result {:choice-fn (effects/get-effect choice)})))

(defn- choose-single [game valid-choices selection]
  (when (sequential? selection)
    (assert (<= (count selection) 1) "Choose error: You can only pick 1 option."))
  (let [[{:keys [player-no card-id choice or-choice source min optional? bonus-damage]}] (get game :effect-stack)
        {:keys [choice-fn args]} (get-choice-fn choice)
        arg-name         (case source
                           :special :choice
                           :mixed :choice
                           :card-name)
        single-selection (if (sequential? selection)
                           (first selection)
                           selection)]
    (if (= min 1)
      (assert (or single-selection optional?) "Choose error: You must pick an option"))
    (when single-selection
      (assert (valid-choices single-selection) (str "Choose error: " single-selection " is not a valid option.")))
    (-> game
        pop-effect-stack
        (as-> game
              (if (and (nil? single-selection)
                       or-choice)
                (push-effect-stack game (merge {:player-no player-no
                                                :effects   (:effects or-choice)}
                                               (when bonus-damage
                                                 {:args {:bonus-damage bonus-damage}})))
                (choice-fn game (merge args
                                       {:player-no player-no}
                                       (when bonus-damage
                                         {:bonus-damage bonus-damage})
                                       (cond
                                         (map? single-selection) single-selection
                                         single-selection {:card-id card-id
                                                           arg-name single-selection}
                                         :else {:no-choice? true}))))))))

(defn- choose-multi [game valid-choices selection]
  (let [[{:keys [player-no card-id choice or-choice source area min max optional? choice-opts bonus-damage]}] (get game :effect-stack)
        {:keys [choice-fn args]} (get-choice-fn choice)
        arg-name        (cond
                          (#{:special :mixed} source) :choices
                          (= :prepped-spells area) :spells
                          (= :players source) :player-card-names
                          (= :discard area) :card-ids
                          :else :card-names)
        multi-selection (if (sequential? selection)
                          selection
                          (if selection
                            [selection]
                            []))]
    (when min
      (assert (or (<= min (count multi-selection))
                  (and optional? (empty? multi-selection))) (str "Choose error: You must pick at least " min " options.")))
    (when max
      (assert (<= (count multi-selection) max) (str "Choose error: You can only pick " max " options.")))
    (doseq [sel multi-selection]
      (assert (valid-choices sel) (str "Choose error: " (ut/format-name sel) " is not a valid choice.")))
    (when (:unique choice-opts)
      (assert (or (< (count multi-selection) 2)
                  (apply distinct? multi-selection)) (str "Choose error: All choices must be different: " (->> multi-selection
                                                                                                               (map ut/format-name)
                                                                                                               (string/join ", ")))))
    (when (:similar choice-opts)
      (assert (or (< (count multi-selection) 2)
                  (apply = multi-selection)) (str "Choose error: All choices must be similar: " (->> multi-selection
                                                                                                     (map ut/format-name)
                                                                                                     (string/join ", ")))))

    (-> game
        pop-effect-stack
        (as-> game
              (if (and (empty? multi-selection)
                       or-choice)
                (push-effect-stack game (merge {:player-no player-no
                                                :effects   (:effects or-choice)}
                                               (when bonus-damage
                                                 {:args {:bonus-damage bonus-damage}})))
                (choice-fn game (merge args
                                       (when bonus-damage
                                         {:bonus-damage bonus-damage})
                                       {:player-no player-no
                                        :card-id   card-id
                                        arg-name   multi-selection})))))))

(defn choose [game selection]
  (let [[{:keys [choice options min max]}] (get game :effect-stack)
        choose-fn     (if (= max 1) choose-single choose-multi)
        valid-choices (->> options
                           (map (fn [{:keys [option] :as option-data}]
                                  (or option
                                      option-data)))
                           set)]
    (assert choice "Choose error: You don't have a choice to make.")
    (assert (or (not-empty options)
                (nil? selection)) "Choose error: Choice has no options")
    (assert (or (nil? min) (nil? max) (<= min max)))
    (-> game
        (choose-fn valid-choices selection)
        check-stack)))

(defn give-choice [{:keys [mode] :as game} {:keys                 [player-no card-id min max optional? repeatable? unswift? choice-opts bonus-damage]
                                            [opt-name :as option] :options
                                            {:keys [effects]}     :or-choice
                                            :as                   choice}]
  (let [opt-fn    (effects/get-option opt-name)
        [area & opt-args] (ut/get-opt-args option)
        options   (cond->> (apply opt-fn game {:player-no player-no
                                               :card-id   card-id
                                               :area      area} opt-args)
                           (:unique choice-opts) distinct)
        {:keys [min max] :as choice} (-> choice
                                         (assoc :options options
                                                :source opt-name
                                                :area area)
                                         (cond-> (not repeatable?)
                                                 (cond-> min (update :min clojure.core/min (count options))
                                                         max (update :max clojure.core/min (count options)))))
        swiftable (and (= :swift mode)
                       (not unswift?)
                       (not-empty options)
                       (or (apply = options)
                           (<= (count options) min))
                       (= min (or max (count options)))
                       (not optional?)
                       (or (not repeatable?)
                           (apply = options)))]
    (cond
      (not-empty options) (-> game
                              (push-effect-stack {:player-no player-no
                                                  :card-id   card-id
                                                  :choice    choice})
                              (cond-> swiftable (choose (->> options
                                                             (take min)
                                                             (map (fn [o] (or (:option o) o)))))))
      effects (push-effect-stack game (merge {:player-no player-no
                                              :effects   effects}
                                             (when bonus-damage
                                               {:args {:bonus-damage bonus-damage}})))
      (or (nil? min)
          optional?) (-> game
                         (push-effect-stack {:player-no player-no
                                             :card-id   card-id
                                             :choice    choice})
                         (choose nil))
      :else game)))

(effects/register {:give-choice give-choice})

(defn card-effect [{:keys [real-game?] :as game} {:keys [player-no card]}]
  (let [{:keys [id effects]} card]
    (-> game
        (cond-> real-game? (update-in [:players player-no :this-turn] concat [{:play (:name card)}]))
        (push-effect-stack {:player-no player-no
                            :card-id   id
                            :effects   effects}))))

(effects/register {:card-effect card-effect})

(defn clear-player [game {:keys [player-no]}]
  (-> game
      (update-in [:players player-no] dissoc :aether :earmarked-aether :restricted-aether :this-turn :breach-capacity :breach-cost-reduction)
      (medley/update-existing-in [:players player-no :breaches]
                                 (partial mapv (fn [{:keys [status] :as breach}]
                                                 (cond-> breach
                                                         (#{:focused} status) (assoc :status :closed)))))))

(defn set-current-player [game {:keys [player-no turn-order-token]}]
  (-> game
      (assoc :current-player player-no)
      (cond-> turn-order-token (assoc-in [:players player-no :turn-order-token] turn-order-token))
      (set-phase {:player-no player-no
                  :phase     :casting})))

(effects/register {:clear-player       clear-player
                   :set-current-player set-current-player})
