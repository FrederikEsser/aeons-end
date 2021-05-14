(ns aeons-end.commands
  (:require [aeons-end.utils :as ut]
            [aeons-end.operations :as op]))

(defn play
  ([game {:keys [player-no card-name]}]
   (play game player-no card-name))
  ([{:keys [effect-stack] :as game} player-no card-name]
   (let [{{:keys [effects type] :as card} :card} (ut/get-card-idx game [:players player-no :hand] {:name card-name})]
     (assert (-> effect-stack first :choice not) "Play error: You have a choice to make.")
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
         op/check-stack))))

(defn prep-spell [game player-no card-name breach-no]
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
  (let [{{:keys [effects]} :card} (ut/get-card-idx game [:players player-no :breaches breach-no :prepped-spells] {:name card-name})]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   (concat [[:set-phase {:phase :casting}]
                                                   [:move-card {:card-name card-name
                                                                :from      :breach
                                                                :breach-no breach-no
                                                                :to        :discard}]]
                                                  effects)})
        op/check-stack)))

(defn buy-card
  [{:keys [effect-stack] :as game} player-no card-name]
  (let [{:keys [card pile-size] :as supply-pile} (ut/get-pile-idx game card-name)
        {:keys [cost]} card]
    (assert (empty? effect-stack) "Buy error: You have a choice to make.")
    (assert supply-pile (str "Buy error: The supply doesn't have a " (ut/format-name card-name) " pile."))
    (assert (and pile-size (pos? pile-size)) (str "Buy error: " (ut/format-name card-name) " supply is empty."))
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay cost]
                                           [:gain {:card-name card-name
                                                   :bought    true}]]})
        op/check-stack)))

(defn buy-charge [{:keys [effect-stack] :as game} player-no]
  (assert (empty? effect-stack) "Charge error: You have a choice to make.")
  (-> game
      (op/push-effect-stack {:player-no player-no
                             :effects   [[:set-phase {:phase :main}]
                                         [:pay 2]
                                         [:gain-charge]]})
      op/check-stack))

(defn focus-breach [{:keys [effect-stack] :as game} player-no breach-no]
  (assert (empty? effect-stack) "Breach error: You have a choice to make.")
  (let [{:keys [focus-cost]} (get-in game [:players player-no :breaches breach-no])]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay focus-cost]
                                           [:focus-breach {:breach-no breach-no}]]})
        op/check-stack)))

(defn open-breach [{:keys [effect-stack] :as game} player-no breach-no]
  (assert (empty? effect-stack) "Breach error: You have a choice to make.")
  (let [{:keys [open-costs stage]} (get-in game [:players player-no :breaches breach-no])
        open-cost (get open-costs stage)]
    (-> game
        (op/push-effect-stack {:player-no player-no
                               :effects   [[:set-phase {:phase :main}]
                                           [:pay open-cost]
                                           [:open-breach {:breach-no breach-no}]]})
        op/check-stack)))

(defn discard [{:keys [effect-stack] :as game} player-no card-name]
  (assert (empty? effect-stack) "Discard error: You have a choice to make.")
  (-> game
      (op/push-effect-stack {:player-no player-no
                             :effects   [[:set-phase {:phase :draw}]
                                         [:move-card {:card-name card-name
                                                      :from      :play-area
                                                      :to        :discard}]]})
      op/check-stack))

(defn discard-all [{:keys [effect-stack] :as game} player-no]
  (assert (empty? effect-stack) "Discard error: You have a choice to make.")
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
