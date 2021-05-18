(ns aeons-end.commands
  (:require [aeons-end.utils :as ut]
            [aeons-end.operations :as op]))

(defn- check-command [command {:keys [current-player effect-stack]} player-no]
  (when current-player
    (assert (= player-no current-player) (str command " error: " player-no " is not the current player " current-player ".")))
  (assert (empty? effect-stack) "End turn error: You have a choice to make."))

(defn start-game [game]
  (-> game
      (op/push-effect-stack {:effects [[:next-turn]]})
      op/check-stack))

(defn play [game player-no card-name]
  (check-command "Play" game player-no)
  (let [{{:keys [effects type] :as card} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (assert card (str "Play error: There is no " (ut/format-name card-name) " in your Hand."))
    (assert type (str "Play error: " (ut/format-name card-name) " has no type."))
    (assert (#{:gem :relic} type) (str "Play error: You can't play " (ut/format-name type) " cards."))
    (assert effects (str "Play error: " (ut/format-name card-name) " has no effects."))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:move-card {:card-name card-name
                                                        :from      :hand
                                                        :to        :play-area}]
                                           [:card-effect {:card card}]]})
        op/check-stack)))

(defn play-all-gems [game player-no]
  (check-command "Play all gems" game player-no)
  (let [{:keys [hand]} (get-in game [:players player-no])
        effects (->> hand
                     (filter (comp #{:gem} :type))
                     (sort-by (fn [{:keys [auto-play-index]}] (or auto-play-index 0)))
                     (mapcat (fn [{:keys [name] :as card}]
                               [[:move-card {:card-name name
                                             :from      :hand
                                             :to        :play-area}]
                                [:card-effect {:card card}]])))]
    (if (not-empty effects)
      (-> game
          (op/push-effect-stack {:player-no player-no
                                 :effects   (concat [[:set-phase {:phase :main}]]
                                                    effects)})
          op/check-stack)
      game)))

(defn prep-spell [game player-no card-name breach-no]
  (check-command "Prep" game player-no)
  (let [{{:keys [type] :as card} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})
        {:keys [status prepped-spells]} (get-in game [:players player-no :breaches breach-no])]
    (assert card (str "Prep error: There is no " (ut/format-name card-name) " in your Hand."))
    (assert (= :spell type) (str "Prep error: You can't prep " (ut/format-name card-name) ", which has type " (ut/format-name type) "."))
    (assert (#{:opened :focused} status) (str "Prep error: You can't prep " (ut/format-name card-name) " to breach " breach-no " with status " (ut/format-name status) "."))
    (assert (empty? prepped-spells) (str "Prep error: You can't prep " (ut/format-name card-name) " to breach " breach-no " which already has prepped spells [" (ut/format-types (map :name prepped-spells)) "]."))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:move-card {:player-no player-no
                                                        :card-name card-name
                                                        :from      :hand
                                                        :to        :breach
                                                        :breach-no breach-no}]]})
        op/check-stack)))

(defn cast-spell [game player-no card-name breach-no]
  (check-command "Cast" game player-no)
  (-> game
      (op/push-effect-stack {:player-no player-no
                             :effects   (concat [[:set-phase {:phase :casting}]
                                                 [:cast-spell {:card-name card-name
                                                               :breach-no breach-no}]])})
      op/check-stack))

(defn buy-card [game player-no card-name]
  (check-command "Buy" game player-no)
  (let [{:keys [card pile-size] :as supply-pile} (ut/get-pile-idx game card-name)
        {:keys [cost]} card]
    (assert supply-pile (str "Buy error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (assert (and pile-size (pos? pile-size)) (str "Buy error: " (ut/format-name card-name) " supply is empty."))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay cost]
                                           [:gain {:card-name card-name}]]})
        op/check-stack)))

(defn charge-ability [game player-no]
  (check-command "Charge" game player-no)
  (-> game
      (op/push-effect-stack {:player-no player-no
                             :effects   [[:set-phase {:phase :main}]
                                         [:pay 2]
                                         [:gain-charge]]})
      op/check-stack))

(defn activate-ability [game player-no]
  (check-command "Activate" game player-no)
  (let [{:keys [effects]} (get-in game [:players player-no :ability])]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   (concat [[:set-phase {:phase :main}]
                                                   [:spend-charges]]
                                                  effects)})
        op/check-stack)))

(defn focus-breach [game player-no breach-no]
  (check-command "Focus" game player-no)
  (let [{:keys [focus-cost]} (get-in game [:players player-no :breaches breach-no])]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay focus-cost]
                                           [:focus-breach {:breach-no breach-no}]]})
        op/check-stack)))

(defn open-breach [game player-no breach-no]
  (check-command "Open" game player-no)
  (let [{:keys [open-costs stage]} (get-in game [:players player-no :breaches breach-no])
        open-cost (get open-costs stage)]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay open-cost]
                                           [:open-breach {:breach-no breach-no}]]})
        op/check-stack)))

(defn discard [game player-no card-name]
  (check-command "Discard" game player-no)
  (-> game
      (op/push-effect-stack {:player-no player-no
                             :effects   [[:set-phase {:phase :draw}]
                                         [:move-card {:card-name card-name
                                                      :from      :play-area
                                                      :to        :discard}]]})
      op/check-stack))

(defn discard-all [game player-no]
  (check-command "Discard all" game player-no)
  (let [card-names (->> (get-in game [:players player-no :play-area])
                        (sort-by (juxt :cost (comp count :effects)))
                        reverse
                        (map :name))]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :draw}]
                                           [:move-cards {:card-names card-names
                                                         :from       :play-area
                                                         :to         :discard}]]})
        op/check-stack)))

(defn end-turn [game player-no]
  (check-command "End turn" game player-no)
  (let [{:keys [hand play-area]} (get-in game [:players player-no])]
    (assert (empty? play-area) (str "End turn error: You must discard all played cards first: "
                                    (->> play-area
                                         (map :name)
                                         (ut/format-types))))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:draw (max (- 5 (count hand)) 0)]
                                           [:clear-player]
                                           [:set-phase {:phase :out-of-turn}]
                                           [:next-turn]]})
        op/check-stack)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
