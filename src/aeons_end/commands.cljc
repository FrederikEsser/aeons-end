(ns aeons-end.commands
  (:require [aeons-end.utils :as ut]
            [aeons-end.operations :as op]
            [aeons-end.nemeses.knight-of-shackles :as knight-of-shackles]))

(defn- check-command [command {:keys [current-player effect-stack]} & [player-no]]
  (when (and current-player
             player-no)
    (assert (= player-no current-player) (str command " error: It's not your turn.")))
  (assert (empty? effect-stack) (str command " error: You have a choice to make.")))

(defn start-game [game]
  (-> game
      (op/push-effect-stack {:effects [[:setup]
                                       [:next-turn]]})
      op/check-stack))

(defn play [game player-no card-name]
  (check-command "Play" game player-no)
  (let [{{:keys [effects type] :as card} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (assert card (str "Play error: There is no " (ut/format-name card-name) " in your Hand."))
    (assert type (str "Play error: " (ut/format-name card-name) " has no type."))
    (assert (#{:gem :relic :corruption} type) (str "Play error: You can't play " (ut/format-name type) " cards."))
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
  (-> game
      (op/push-effect-stack {:player-no player-no
                             :effects   (concat [[:set-phase {:phase :main}]
                                                 [:play-all-gems]])})
      op/check-stack))

(defn prep-spell [game player-no breach-no card-name]
  (check-command "Prep" game player-no)
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
    (assert (ut/can-prep? game {:player-no player-no
                                :breach-no breach-no
                                :card      card})
            (str "Prep error: You can't prep " (ut/format-name card-name) " to breach " breach-no "."))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:prep-spell {:player-no player-no
                                                         :breach-no breach-no
                                                         :card-name card-name}]]})
        op/check-stack)))

(defn use-while-prepped [game player-no breach-no card-name]
  (check-command "While prepped" game player-no)
  (let [{:keys [card]} (ut/get-card-idx game [:players player-no :breaches breach-no :prepped-spells] {:name card-name})
        {:keys [phase]} (:while-prepped card)]
    (assert phase (str "While prepped error: " (ut/format-name card-name) " has no 'while prepped' effects."))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase phase}]
                                           [:use-while-prepped {:player-no player-no
                                                                :breach-no breach-no
                                                                :card-name card-name}]]})
        op/check-stack)))

(defn cast-spell [game player-no breach-no card-name]
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
        {:keys [cost type]} card]
    (assert supply-pile (str "Buy error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (assert (and pile-size (pos? pile-size)) (str "Buy error: " (ut/format-name card-name) " supply is empty."))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay {:amount cost
                                                  :type   type}]
                                           [:gain {:card-name card-name}]]})
        op/check-stack)))

(defn charge-ability [game player-no]
  (check-command "Charge" game player-no)
  (let [{:keys [charges charge-cost]
         :or   {charges 0}} (get-in game [:players player-no :ability])]
    (assert (and charge-cost (< charges charge-cost)) (str "Charge error: You already have " charges " charges.")))
  (-> game
      (op/push-effect-stack {:player-no player-no
                             :effects   [[:set-phase {:phase :main}]
                                         [:pay {:amount 2
                                                :type   :charge-ability}]
                                         [:gain-charge]]})
      op/check-stack))

(defn activate-ability [{:keys [current-player] :as game} player-no]
  (let [{:keys [activation]} (get-in game [:players player-no :ability])]
    (check-command "Activate" game (if (= :any-main-phase activation)
                                     current-player
                                     player-no))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:activate-ability]]})
        (op/push-effect-stack {:player-no current-player
                               :effects   [[:set-phase {:phase :main}]]})
        op/check-stack)))

(defn focus-breach [game player-no breach-no]
  (check-command "Focus" game player-no)
  (let [{:keys [focus-cost]} (get-in game [:players player-no :breaches breach-no])
        {:keys [breach-cost-reduction]} (get-in game [:players player-no])]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay {:amount (ut/minus-cost focus-cost breach-cost-reduction)
                                                  :type   :breach}]
                                           [:focus-breach {:breach-no breach-no}]]})
        op/check-stack)))

(defn open-breach [game player-no breach-no]
  (check-command "Open" game player-no)
  (let [{:keys [open-costs stage]} (get-in game [:players player-no :breaches breach-no])
        open-cost (get open-costs stage)
        {:keys [breach-cost-reduction]} (get-in game [:players player-no])]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay {:amount (ut/minus-cost open-cost breach-cost-reduction)
                                                  :type   :breach}]
                                           [:open-breach {:breach-no breach-no}]]})
        op/check-stack)))

(defn unfocus-nemesis-breach [game player-no breach-no]
  (check-command "Unfocus" game player-no)
  (let [{:keys [cost]} (get-in game [:nemesis :breaches breach-no])]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay {:amount cost
                                                  :type   :breach}]
                                           [::knight-of-shackles/unfocus-breach {:breach-no breach-no}]]})
        op/check-stack)))

(defn discard-power-card [game player-no card-name]
  (check-command "Resolve TO DISCARD" game player-no)
  (-> game
      (op/push-effect-stack {:player-no player-no
                             :effects   [[:discard-power-card {:card-name card-name}]]})
      op/check-stack))

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
                        (sort-by (juxt :cost ut/get-card-strength))
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
                               :effects   [[:set-phase {:phase :draw}]
                                           [:draw (max (- 5 (count hand)) 0)]
                                           [:set-phase {:phase :out-of-turn}]
                                           [:clear-player]
                                           [:next-turn]]})
        op/check-stack)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
