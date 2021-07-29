(ns aeons-end.nemesis
  (:require [aeons-end.operations :refer [push-effect-stack move-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.nemeses.rageborne :refer [rageborne]]
            [aeons-end.nemeses.umbra-titan :refer [umbra-titan]]
            [aeons-end.nemeses.carapace-queen :refer [carapace-queen deal-damage-to-husks]]
            [aeons-end.nemeses.blight-lord :refer [blight-lord]]
            [aeons-end.nemeses.crooked-mask :refer [crooked-mask]]
            [aeons-end.nemeses.magus-of-cloaks :refer [magus-of-cloaks]]
            [aeons-end.cards.attack :as attack]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.power :as power]))

(defn unleash [game args]
  (let [effects (get-in game [:nemesis :unleash])]
    (push-effect-stack game {:args    args
                             :effects effects})))

(effects/register {:unleash unleash})

(defn discard-nemesis-card [game {:keys [card-name]}]
  (move-card game {:card-name card-name
                   :from      :play-area
                   :to        :discard}))

(effects/register {:discard-nemesis-card discard-nemesis-card})

(defn set-resolving [game {:keys [card-name]}]
  (assoc game :resolving card-name))

(defn clear-resolving [game _]
  (dissoc game :resolving))

(effects/register {:set-resolving   set-resolving
                   :clear-resolving clear-resolving})

(defn resolve-power-card [game {:keys [card-name]}]
  (let [{:keys [idx card]} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
        {{:keys [power effects]} :power} card]
    (-> game
        (update-in [:nemesis :play-area idx :power :power] dec)
        (cond->
          (= 1 power) (push-effect-stack {:effects (concat [[:set-resolving {:card-name card-name}]]
                                                           effects
                                                           [[:clear-resolving]
                                                            [:discard-nemesis-card {:card-name card-name}]])})))))

(defn resolve-minion-card [game {:keys [card-name]}]
  (let [{:keys [effects]} (-> (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
                              :card
                              :persistent)]
    (push-effect-stack game {:effects (concat [[:set-resolving {:card-name card-name}]]
                                              effects
                                              [[:clear-resolving]])})))

(defn resolve-nemesis-cards-in-play [{:keys [current-player nemesis] :as game} _]
  (-> game
      (cond->
        current-player (assoc :current-player :nemesis))
      (push-effect-stack {:effects (->> (:play-area nemesis)
                                        (map (fn [{:keys [type name power]}]
                                               [:give-choice {:title   (ut/format-name (:name nemesis))
                                                              :text    (case type
                                                                         :power (if (< 1 (:power power))
                                                                                  (str (ut/format-name name) " powers up.") #_(str (ut/format-name (:name nemesis)) " powers up " (ut/format-name name) ".")
                                                                                  (str (ut/format-name name) " is fully powered!") #_(str (ut/format-name (:name nemesis)) " activates the fully powered " (ut/format-name name) "."))
                                                                         :minion (str (ut/format-name name) " attacks!") #_"Minion attacks.")
                                                              :choice  (case type
                                                                         :power :resolve-power-card
                                                                         :minion :resolve-minion-card)
                                                              :options [:nemesis :play-area {:name name}]
                                                              :min     1
                                                              :max     1}])))})))

(effects/register {:resolve-power-card            resolve-power-card
                   :resolve-minion-card           resolve-minion-card
                   :resolve-nemesis-cards-in-play resolve-nemesis-cards-in-play})

(defn set-minion-max-life [game {:keys [card-name life]}]
  (ut/update-in-vec game [:nemesis :play-area] {:name card-name} assoc :max-life life))

(defn resolve-nemesis-card [game {:keys [player-no card-name]}]
  (cond
    card-name (let [{{:keys [name type immediately] :as card} :card} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
                    effects (or (:effects card)
                                (:effects immediately))]
                (push-effect-stack game {:effects (concat
                                                    [[:set-resolving {:card-name name}]]
                                                    (when effects
                                                      effects)
                                                    [[:clear-resolving]]
                                                    (when (= :attack type)
                                                      [[:discard-nemesis-card {:card-name name}]]))}))
    player-no (push-effect-stack game {:player-no player-no
                                       :effects   [[:activate-ability]]})))

(defn draw-nemesis-card [{:keys [nemesis] :as game} _]
  (let [{:keys [name type life]} (get-in game [:nemesis :deck 0])]
    (-> game
        (cond-> (:phase nemesis) (assoc-in [:nemesis :phase] :draw))
        (push-effect-stack {:effects (concat [[:move-card {:card-name name
                                                           :from      :deck
                                                           :to        :play-area}]]
                                             (when (= :minion type)
                                               [[:set-minion-max-life {:card-name name
                                                                       :life      life}]])
                                             [[:give-choice {:title   (ut/format-name (:name nemesis))
                                                             :text    (str "Nemesis "
                                                                           (case type
                                                                             :attack (str "attacks with " (ut/format-name name) ".")
                                                                             :power (str "initiates " (ut/format-name name) ".")
                                                                             :minion (str "deploys its " (ut/format-name name) ".")))
                                                             :choice  :resolve-nemesis-card
                                                             :options (concat
                                                                        [:mixed
                                                                         [:nemesis :play-area {:name name}]]
                                                                        (when (#{:attack :power} type)
                                                                          [[:players :ability {:activation    :nemesis-draw
                                                                                               :fully-charged true}]]))
                                                             :min     1
                                                             :max     1}]])}))))

(effects/register {:set-minion-max-life  set-minion-max-life
                   :resolve-nemesis-card resolve-nemesis-card
                   :draw-nemesis-card    draw-nemesis-card})

(defn at-start-turn [{:keys [nemesis] :as game} _]
  (let [{:keys [at-start-turn]} nemesis]
    (cond-> game
            (:phase nemesis) (assoc-in [:nemesis :phase] :main)
            at-start-turn (push-effect-stack {:effects at-start-turn}))))

(defn at-end-turn [{:keys [nemesis] :as game} _]
  (let [{:keys [at-end-turn]} nemesis]
    (cond-> game
            (:phase nemesis) (assoc-in [:nemesis :phase] :out-of-turn)
            at-end-turn (push-effect-stack {:effects at-end-turn}))))

(effects/register {::at-start-turn at-start-turn
                   ::at-end-turn   at-end-turn})

(defn deal-damage-to-nemesis [{:keys [nemesis] :as game} {:keys [damage]}]
  (if (pos? damage)
    (let [{:keys [life modify-damage when-hit]} nemesis
          modify-damage-fn (when modify-damage
                             (effects/get-predicate modify-damage))
          damage           (if modify-damage-fn
                             (modify-damage-fn game damage)
                             damage)]
      (-> game
          (assoc-in [:nemesis :life] (max (- life damage) 0))
          (cond-> when-hit (push-effect-stack {:args    {:damage damage}
                                               :effects when-hit}))))
    game))

(defn deal-damage-to-minion [game {:keys [player-no card-name damage kill-effects] :as args}]
  (if (= :husks card-name)
    (deal-damage-to-husks game args)
    (let [{:keys [card idx]} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
          {:keys [life max-life modify-damage when-hit]} card
          modify-damage-fn (when modify-damage
                             (effects/get-predicate modify-damage))
          damage           (if modify-damage-fn
                             (modify-damage-fn game damage)
                             damage)
          killed?          (<= life damage)]
      (-> game
          (assoc-in [:nemesis :play-area idx :life] (if killed? (or max-life 0)
                                                                (- life damage)))
          (cond-> killed? (discard-nemesis-card {:card-name card-name}))
          (push-effect-stack {:player-no player-no
                              :effects   (concat
                                           (when (pos? damage)
                                             when-hit)
                                           (when killed?
                                             kill-effects))})))))

(defn deal-damage-to-target [game {:keys [player-no damage area card-name kill-effects]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :nemesis [[:deal-damage-to-nemesis {:damage damage}]]
                                        :minions [[:deal-damage-to-minion {:card-name    card-name
                                                                           :damage       damage
                                                                           :kill-effects kill-effects}]])}))

(defn deal-damage [{:keys [nemesis] :as game} {:keys [player-no arg bonus-damage kill-effects]
                                               :or   {bonus-damage 0}}]
  (let [{:keys [name]} nemesis
        minions (ut/options-from-nemesis game {:area :minions})
        damage  (+ arg bonus-damage)]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (not-empty minions)
                                          [[:give-choice {:text    (str "Deal " damage " damage to " (ut/format-name (or name :nemesis)) " or a Minion.")
                                                          :choice  [:deal-damage-to-target {:damage       damage
                                                                                            :kill-effects kill-effects}]
                                                          :options [:mixed
                                                                    [:nemesis]
                                                                    [:nemesis :minions]]
                                                          :min     1
                                                          :max     1}]]
                                          [[:deal-damage-to-nemesis {:damage damage}]])})))

(effects/register {:deal-damage-to-nemesis deal-damage-to-nemesis
                   :deal-damage-to-minion  deal-damage-to-minion
                   :deal-damage-to-target  deal-damage-to-target
                   :deal-damage            deal-damage})


(defn heal-nemesis [{{:keys [life max-life]} :nemesis :as game} {:keys [arg]}]
  (assoc-in game [:nemesis :life] (min (+ life arg)
                                       max-life)))

(effects/register {:heal-nemesis heal-nemesis})

(defn damage-gravehold [{:keys [gravehold] :as game} {:keys [arg]}]
  (assoc-in game [:gravehold :life] (max (- (:life gravehold) arg) 0)))

(effects/register {:damage-gravehold damage-gravehold})

(defn exhaust-player [game {:keys [player-no]}]
  (let [{:keys [name ability]} (get-in game [:players player-no])
        resolve-text (str (ut/format-name name) " exhausted")]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat [[:unleash {:resolving resolve-text}]
                                                 [:unleash {:resolving resolve-text}]
                                                 [:give-choice {:title   resolve-text
                                                                :text    "Destroy any of your breaches, discarding any spell prepped in that breach."
                                                                :choice  [:destroy-breach {:put-prepped-spells-in :discard}]
                                                                :options [:player :breaches]
                                                                :min     1
                                                                :max     1}]]
                                                (when (:charges ability)
                                                  [[:spend-charges]]))})))

(defn damage-player [game {:keys [player-no arg]}]
  (let [{:keys [life]} (get-in game [:players player-no])]
    (if (< arg life)
      (update-in game [:players player-no :life] - arg)
      (-> game
          (assoc-in [:players player-no :life] 0)
          (push-effect-stack {:player-no player-no
                              :effects   (concat
                                           (when (> arg life)
                                             [[:damage-gravehold (* 2 (- arg life))]])
                                           (when (pos? life)
                                             [[:exhaust-player]]))})))))

(effects/register {:exhaust-player exhaust-player
                   :damage-player  damage-player})

(def nemeses [rageborne
              umbra-titan
              carapace-queen
              blight-lord
              crooked-mask
              magus-of-cloaks])

(def basic-cards (concat
                   ; AE Tier 1
                   [attack/skewer
                    attack/slaughter
                    power/agony-field
                    power/eye-of-nothing
                    minion/bane-sire
                    minion/haze-spewer]
                   ; WE Tier 1
                   [attack/afflict
                    attack/encroach
                    attack/nix
                    attack/thrash
                    minion/catacomb-drone
                    minion/howling-spinners
                    power/heart-of-nothing
                    power/night-unending
                    power/planar-collision]
                   ; AE Tier 2
                   [minion/cauterizer
                    minion/needlemaw]
                   ; WE Tier 2
                   [attack/mutilate
                    attack/smite
                    minion/mage-ender
                    minion/mangleroot
                    minion/null-scion
                    power/aphotic-sun
                    power/chaos-flail
                    power/morbid-gyre]
                   ; AE Tier 3
                   [attack/topple
                    power/doom-aegis]
                   ; WE Tier 3
                   [attack/banish
                    attack/engulf
                    attack/quell
                    attack/throttle
                    minion/monstrosity-of-omens
                    power/apocalypse-ritual
                    power/cataclysmic-fate
                    power/withering-beam]
                   ; The Outer Dark
                   [minion/labyrinth-wisp
                    attack/assail
                    power/dire-abbatoir]))
