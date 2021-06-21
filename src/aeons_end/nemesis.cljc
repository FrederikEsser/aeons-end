(ns aeons-end.nemesis
  (:require [aeons-end.operations :refer [push-effect-stack move-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.nemeses.rageborne :refer [rageborne]]
            [aeons-end.nemeses.umbra-titan :refer [umbra-titan]]
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
        current-player (assoc :current-player :nemesis)
        (:phase nemesis) (assoc-in [:nemesis :phase] :main))
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
    card-name (let [{{:keys [name type effects]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})]
                (cond-> game
                        (= :attack type) (push-effect-stack {:effects (concat
                                                                        [[:set-resolving {:card-name name}]]
                                                                        effects
                                                                        [[:clear-resolving]]
                                                                        [[:discard-nemesis-card {:card-name name}]])})))
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

(defn after-effects [{:keys [nemesis] :as game} _]
  (let [{:keys [after-effects]} nemesis]
    (cond-> game
            (:phase nemesis) (assoc-in [:nemesis :phase] :out-of-turn)
            after-effects (push-effect-stack {:effects after-effects}))))

(effects/register {:after-effects after-effects})

(defn deal-damage-to-nemesis [game {:keys [damage]}]
  (let [life (get-in game [:nemesis :life])]
    (assoc-in game [:nemesis :life] (max (- life damage) 0))))

(defn deal-damage-to-minion [game {:keys [card-name damage kill-effects]}]
  (let [{:keys [card idx]} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
        {:keys [life max-life modify-damage]} card
        modify-damage-fn (when modify-damage
                           (effects/get-predicate modify-damage))
        damage           (if modify-damage-fn
                           (modify-damage-fn damage)
                           damage)
        killed?          (<= life damage)]
    (-> game
        (assoc-in [:nemesis :play-area idx :life] (if killed? (or max-life 0)
                                                              (- life damage)))
        (cond-> killed? (discard-nemesis-card {:card-name card-name}))
        (cond-> (and killed?
                     kill-effects) (push-effect-stack {:effects kill-effects})))))

(defn deal-damage-to-target [game {:keys [damage area card-name kill-effects]}]
  (push-effect-stack game {:effects (case area
                                      :nemesis [[:deal-damage-to-nemesis {:damage damage}]]
                                      :minions [[:deal-damage-to-minion {:card-name    card-name
                                                                         :damage       damage
                                                                         :kill-effects kill-effects}]])}))

(defn deal-damage [{:keys [nemesis] :as game} {:keys [arg bonus-damage kill-effects]
                                               :or   {bonus-damage 0}}]
  (let [{:keys [name play-area]} nemesis
        minions (->> play-area
                     (filter (comp #{:minion} :type)))
        damage  (+ arg bonus-damage)]
    (push-effect-stack game {:effects (if (not-empty minions)
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

(defn damage-gravehold [{:keys [gravehold] :as game} {:keys [arg]}]
  (assoc-in game [:gravehold :life] (max (- (:life gravehold) arg) 0)))

(effects/register {:damage-gravehold damage-gravehold})

(defn exhaust-player [game {:keys [player-no]}]
  (let [{:keys [name]} (get-in game [:players player-no])
        resolve-text (str (ut/format-name name) " exhausted")]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:unleash {:resolving resolve-text}]
                                         [:unleash {:resolving resolve-text}]
                                         [:give-choice {:title   resolve-text
                                                        :text    "Destroy any of your breaches, discarding any spell prepped in that breach."
                                                        :choice  :destroy-breach
                                                        :options [:player :breaches]
                                                        :min     1
                                                        :max     1}]
                                         [:spend-charges]]})))

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

(def generic-nemesis {:name       :generic
                      :difficulty 1
                      :life       70
                      :unleash    [[:damage-gravehold 2]]
                      :cards      [attack/nix minion/howling-spinners power/planar-collision
                                   attack/smite minion/null-scion power/aphotic-sun
                                   attack/throttle minion/monstrosity-of-omens power/apocalypse-ritual]})

(def nemeses [rageborne
              umbra-titan])

(def basic-cards (concat
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
                   ; WE Tier 2
                   [attack/mutilate
                    attack/smite
                    minion/mage-ender
                    minion/mangleroot
                    minion/null-scion
                    power/aphotic-sun
                    power/chaos-flail
                    power/morbid-gyre]
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
                   [minion/labyrinth-wisp]))
