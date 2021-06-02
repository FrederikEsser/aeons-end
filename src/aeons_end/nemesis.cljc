(ns aeons-end.nemesis
  (:require [aeons-end.operations :refer [push-effect-stack move-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
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

(defn deal-damage-to-nemesis [game {:keys [damage]}]
  (let [life (get-in game [:nemesis :life])]
    (assoc-in game [:nemesis :life] (max (- life damage) 0))))

(defn deal-damage-to-minion [game {:keys [card-name damage]}]
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
        (cond-> killed? (discard-nemesis-card {:card-name card-name})))))

(defn deal-damage-to-target [game {:keys [damage area card-name]}]
  (push-effect-stack game {:effects (case area
                                      :nemesis [[:deal-damage-to-nemesis {:damage damage}]]
                                      :minions [[:deal-damage-to-minion {:card-name card-name :damage damage}]])}))

(defn deal-damage [{:keys [nemesis] :as game} {:keys [arg bonus-damage]}]
  (let [{:keys [name play-area]} nemesis
        minions (->> play-area
                     (filter (comp #{:minion} :type)))
        damage  (cond-> arg
                        bonus-damage (+ bonus-damage))]
    (push-effect-stack game {:effects (if (not-empty minions)
                                        [[:give-choice {:text    (str "Deal " damage " damage to " (ut/format-name (or name :nemesis)) " or a Minion.")
                                                        :choice  [:deal-damage-to-target {:damage damage}]
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

(def nemeses [umbra-titan])

(def basic-cards (concat
                   ; WE Tier 1
                   [attack/afflict
                    minion/catacomb-drone
                    power/heart-of-nothing
                    minion/howling-spinners
                    power/night-unending
                    attack/nix
                    power/planar-collision
                    attack/thrash]
                   ; WE Tier 2
                   [power/aphotic-sun
                    minion/mangleroot
                    power/morbid-gyre
                    attack/mutilate
                    minion/null-scion
                    attack/smite]
                   ; WE Tier 3
                   [power/apocalypse-ritual
                    attack/banish
                    power/cataclysmic-fate
                    minion/monstrosity-of-omens
                    attack/quell
                    attack/throttle
                    power/withering-beam]))
