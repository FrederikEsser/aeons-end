(ns aeons-end.nemesis
  (:require [aeons-end.operations :refer [push-effect-stack move-card get-phase-change-effects]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.nemeses.rageborne :refer [rageborne]]
            [aeons-end.nemeses.umbra-titan :refer [umbra-titan]]
            [aeons-end.nemeses.carapace-queen :refer [carapace-queen deal-damage-to-husks]]
            [aeons-end.nemeses.knight-of-shackles :refer [knight-of-shackles]]
            [aeons-end.nemeses.blight-lord :refer [blight-lord]]
            [aeons-end.nemeses.crooked-mask :refer [crooked-mask]]
            [aeons-end.nemeses.prince-of-gluttons :refer [prince-of-gluttons]]
            [aeons-end.nemeses.hollow-crown :refer [hollow-crown]]
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
  (let [{{:keys [max-life power]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})]
    (cond-> game
            (:start-power power) (ut/update-in-vec [:nemesis :play-area] {:name card-name} assoc-in [:power :power] (:start-power power))
            max-life (ut/update-in-vec [:nemesis :play-area] {:name card-name} assoc :life max-life)
            card-name (move-card {:card-name card-name
                                  :from      :play-area
                                  :to        :discard}))))

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

(defn resolve-nemesis-cards-in-play [{:keys [nemesis] :as game} _]
  (push-effect-stack game {:effects (->> (:play-area nemesis)
                                         (filter (comp #{:attack :minion :power} :type))
                                         (map (fn [{:keys [type name power]}]
                                                [:give-choice {:title   (ut/format-name (:name nemesis))
                                                               :text    (case type
                                                                          :power (if (< 1 (:power power))
                                                                                   (str (ut/format-name name) " powers up.") #_(str (ut/format-name (:name nemesis)) " powers up " (ut/format-name name) ".")
                                                                                   (str (ut/format-name name) " is fully powered!") #_(str (ut/format-name (:name nemesis)) " activates the fully powered " (ut/format-name name) "."))
                                                                          :minion (str (ut/format-name name) " attacks!") #_"Minion attacks.")
                                                               :effect  (case type
                                                                          :power :resolve-power-card
                                                                          :minion :resolve-minion-card)
                                                               :options [:nemesis :play-area {:name name}]
                                                               :min     1
                                                               :max     1}])))}))

(effects/register {:resolve-power-card            resolve-power-card
                   :resolve-minion-card           resolve-minion-card
                   :resolve-nemesis-cards-in-play resolve-nemesis-cards-in-play})

(defn initialize-nemesis-card [game {:keys [card-name]}]
  (let [{{:keys [life power]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})]
    (cond-> game
            power (ut/update-in-vec [:nemesis :play-area] {:name card-name} assoc-in [:power :start-power] (:power power))
            life (ut/update-in-vec [:nemesis :play-area] {:name card-name} assoc :max-life life))))

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
  (let [{:keys [name type] :as card} (get-in game [:nemesis :deck 0])]
    (cond-> game
            (:phase nemesis) (assoc-in [:nemesis :phase] :draw)
            card (push-effect-stack {:effects [[:move-card {:card-name name
                                                            :from      :deck
                                                            :to        :play-area}]
                                               [:initialize-nemesis-card {:card-name name}]
                                               [:give-choice {:title   (ut/format-name (:name nemesis))
                                                              :text    (str "Nemesis "
                                                                            (case type
                                                                              :attack (str "attacks with " (ut/format-name name) ".")
                                                                              :power (str "initiates " (ut/format-name name) ".")
                                                                              :minion (str "deploys its " (ut/format-name name) ".")))
                                                              :effect  :resolve-nemesis-card
                                                              :options (concat
                                                                         [:mixed
                                                                          [:nemesis :play-area {:name name}]]
                                                                         (when (#{:attack :power} type)
                                                                           [[:players :ability {:activation    :nemesis-draw
                                                                                                :fully-charged true}]]))
                                                              :min     1
                                                              :max     1}]]}))))

(effects/register {:initialize-nemesis-card initialize-nemesis-card
                   :resolve-nemesis-card    resolve-nemesis-card
                   :draw-nemesis-card       draw-nemesis-card})

(defn reactivate-nemesis-card [game {:keys [card-name]}]
  (push-effect-stack game {:effects [[:move-card {:card-name card-name
                                                  :from      :discard
                                                  :to        :play-area}]
                                     [:resolve-nemesis-card {:card-name card-name}]]}))

(effects/register {:reactivate-nemesis-card reactivate-nemesis-card})

(defn at-start-turn [{:keys [nemesis] :as game} _]
  (let [{:keys [at-start-turn]} nemesis]
    (-> game
        (assoc :current-player :nemesis)
        (cond-> (:phase nemesis) (assoc-in [:nemesis :phase] :main)
                at-start-turn (push-effect-stack {:effects at-start-turn})))))

(defn at-end-turn [{:keys [nemesis] :as game} _]
  (let [{:keys [at-end-turn]} nemesis]
    (cond-> game
            (:phase nemesis) (assoc-in [:nemesis :phase] :out-of-turn)
            at-end-turn (push-effect-stack {:effects at-end-turn}))))

(effects/register {::at-start-turn at-start-turn
                   ::at-end-turn   at-end-turn})

(defn- get-damage [damage & {:keys [shield max-damage]}]
  (cond-> damage
          shield (- shield)
          max-damage (min max-damage)
          :always (max 0)))

(defn deal-damage-to-nemesis [{:keys [nemesis] :as game} {:keys [damage]}]
  (if (pos? damage)
    (let [{:keys [life max-damage shield when-hit]} nemesis
          damage (get-damage damage
                             :shield (ut/get-value shield game)
                             :max-damage (ut/get-value max-damage game))]
      (-> game
          (assoc-in [:nemesis :life] (max (- life damage) 0))
          (cond-> when-hit (push-effect-stack {:args    {:damage damage}
                                               :effects when-hit}))))
    game))

(defn deal-damage-to-minion [game {:keys [player-no card-name damage kill-effects] :as args}]
  (if (= :husks card-name)
    (deal-damage-to-husks game args)
    (let [{:keys [card idx]} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
          {:keys [life max-damage shield when-hit when-killed]} card
          damage  (get-damage damage
                              :shield (ut/get-value shield game)
                              :max-damage (ut/get-value max-damage game))
          killed? (<= life damage)]
      (-> game
          (assoc-in [:nemesis :play-area idx :life] (if killed? 0 (- life damage)))
          (cond-> killed? (discard-nemesis-card {:card-name card-name}))
          (push-effect-stack {:player-no player-no
                              :effects   (concat
                                           (when (pos? damage)
                                             when-hit)
                                           (when killed?
                                             (concat kill-effects
                                                     when-killed)))})))))

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
                                                          :effect  [:deal-damage-to-target {:damage       damage
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

(defn damage-gravehold [{:keys [gravehold prevent-damage] :as game} {:keys [arg]}]
  (cond-> game
          (not prevent-damage) (assoc-in [:gravehold :life] (max (- (:life gravehold) arg) 0))))

(effects/register {:damage-gravehold damage-gravehold})

(defn exhaust-player [{:keys [nemesis] :as game} {:keys [player-no]}]
  (let [{:keys [name ability]} (get-in game [:players player-no])
        resolve-text (str (ut/format-name name) " exhausted")
        {:keys [on-player-exhausted]} nemesis]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat (or on-player-exhausted
                                                    [[:unleash {:resolving resolve-text}]
                                                     [:unleash {:resolving resolve-text}]])
                                                [[:give-choice {:title   resolve-text
                                                                :text    "Destroy any of your breaches, discarding any spell prepped in that breach."
                                                                :effect  [:destroy-breach {:put-prepped-spells-in :discard}]
                                                                :options [:player :breaches]
                                                                :min     1
                                                                :max     1}]]
                                                (when (:charges ability)
                                                  [[:spend-charges]]))})))

(defn damage-player [{:keys [prevent-damage] :as game} {:keys [player-no arg]}]
  (if (or (zero? arg)
          prevent-damage)
    game
    (let [{:keys [life breaches]} (get-in game [:players player-no])
          damage  (->> breaches
                       (mapcat :prepped-spells)
                       (keep (comp :modify-damage :while-prepped))
                       (map effects/get-predicate)
                       (reduce (fn [damage modify-damage-fn]
                                 (modify-damage-fn damage))
                               arg))
          effects (get-phase-change-effects game {:player-no    player-no
                                                  :phase-change :at-suffer-damage})]
      (if (< damage life)
        (-> game
            (update-in [:players player-no :life] - damage)
            (cond-> (not-empty effects) (push-effect-stack {:player-no player-no
                                                            :effects   effects})))
        (-> game
            (assoc-in [:players player-no :life] 0)
            (push-effect-stack {:player-no player-no
                                :effects   (concat
                                             (when (> damage life)
                                               [[:damage-gravehold (* 2 (- damage life))]])
                                             effects
                                             (when (pos? life)
                                               [[:exhaust-player]]))}))))))

(effects/register {:exhaust-player exhaust-player
                   :damage-player  damage-player})

(defn setup [{:keys [nemesis] :as game} _]
  (let [{:keys [setup]} nemesis]
    (cond-> game
            setup (push-effect-stack {:effects setup}))))

(effects/register {:setup setup})

(def nemeses [rageborne
              umbra-titan
              carapace-queen
              knight-of-shackles
              blight-lord
              crooked-mask
              prince-of-gluttons
              hollow-crown
              magus-of-cloaks])

(def basic-cards (concat
                   ; AE Tier 1
                   [attack/skewer
                    attack/slaughter
                    minion/bane-sire
                    minion/haze-spewer
                    power/agony-field
                    power/bleed-static
                    power/eye-of-nothing
                    power/woven-sky]
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
                   [attack/awaken
                    attack/dispel
                    attack/lay-waste
                    minion/cauterizer
                    minion/needlemaw
                    minion/venomite
                    power/pulverizing-ray]
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
                   [attack/gathering-darkness
                    attack/obliterate
                    attack/sunder
                    attack/topple
                    minion/jagged-one
                    power/doom-aegis
                    power/reality-rupture]
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
