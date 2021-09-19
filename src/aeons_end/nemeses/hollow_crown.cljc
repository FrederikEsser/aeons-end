(ns aeons-end.nemeses.hollow-crown
  (:require [aeons-end.operations :refer [push-effect-stack move-card add-card]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]
            [medley.core :as medley]
            [aeons-end.cards.minion :as minion]
            [aeons-end.cards.power :as power]
            [aeons-end.cards.attack :as attack]))

(defn resolve-blood-magic [game {:keys [card-name avoid-self-damage]}]
  (let [{:keys [effects]} (-> (ut/get-card-idx game [:nemesis :play-area] {:name card-name})
                              :card
                              :blood-magic)
        effects (cond->> effects
                         avoid-self-damage (remove (comp #{:deal-damage-to-minion} first)))]
    (push-effect-stack game (medley/assoc-some {:effects effects}
                                               :args (when avoid-self-damage
                                                       {:avoid-self-damage avoid-self-damage})))))

(defn resolve-all-acolytes [game {:keys [title]}]
  (let [acolytes (->> (get-in game [:nemesis :play-area])
                      (filter (comp #{:acolyte} :type)))]
    (push-effect-stack game {:effects (->> acolytes
                                           (map (fn [{:keys [name]}]
                                                  [:give-choice {:title   title
                                                                 :text    "Resolve the Blood Magic effect of each acolyte in play without those acolytes suffering damage from their Blood Magic effects."
                                                                 :choice  [::resolve-blood-magic {:avoid-self-damage true}]
                                                                 :options [:nemesis :play-area {:name name}]
                                                                 :min     1
                                                                 :max     1}])))})))

(defn do-unleash [{:keys [difficulty] :as game} args]
  (let [title (keyword (or (:resolving args)
                           (:resolving game))
                       "unleash")]
    (push-effect-stack game {:effects (if (#{:expert :extinction} difficulty)
                                        [[::resolve-all-acolytes]]
                                        [[:give-choice {:title   title
                                                        :text    "Resolve the Blood Magic effect of the acolyte with the highest life."
                                                        :choice  ::resolve-blood-magic
                                                        :options [:nemesis :minions {:type      :acolyte
                                                                                     :most-life true}]
                                                        :min     1
                                                        :max     1}]])})))

(effects/register {::resolve-blood-magic  resolve-blood-magic
                   ::resolve-all-acolytes resolve-all-acolytes
                   ::unleash              do-unleash})

(defn draw-acolyte [game _]
  (let [{:keys [name] :as card} (get-in game [:nemesis :acolytes 0])]
    (cond-> game
            card (push-effect-stack {:effects [[:move-card {:from          :acolytes
                                                            :from-position :top
                                                            :to            :play-area}]
                                               [:initialize-nemesis-card {:card-name name}]]}))))

(effects/register {::draw-acolyte draw-acolyte})

(defn edryss-tragg-self-damage [game {:keys [player-no]}]
  (let [{:keys [life]} (get-in game [:players player-no])]
    (cond-> game
            (<= life 5) (push-effect-stack {:effects [[:deal-damage-to-minion {:card-name :edryss-tragg
                                                                               :damage    2}]]}))))

(defn edryss-tragg-damage [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 2]
                                       [::edryss-tragg-self-damage]]}))

(defn edryss-tragg-blood-magic [game {:keys [avoid-self-damage]}]
  (push-effect-stack game {:effects [[:give-choice {:title   :acolyte
                                                    :text    "Any player suffers 2 damage."
                                                    :choice  (if avoid-self-damage
                                                               [:damage-player {:arg 2}]
                                                               ::edryss-tragg-damage)
                                                    :options [:players]
                                                    :min     1
                                                    :max     1}]]}))

(effects/register {::edryss-tragg-self-damage edryss-tragg-self-damage
                   ::edryss-tragg-damage      edryss-tragg-damage
                   ::edryss-tragg-blood-magic edryss-tragg-blood-magic})

(def edryss-tragg {:name        :edryss-tragg
                   :type        :acolyte
                   :life        11
                   :blood-magic {:text    "Any player suffers 2 damage. Then, if that player has 5 life or less, this minion suffers 2 damage."
                                 :effects [[::edryss-tragg-blood-magic]]}
                   :when-killed [[::draw-acolyte]]
                   :quote       "'We become that which is most feared.'"})

(defn setup [{:keys [difficulty] :as game} _]
  (-> game
      (assoc-in [:nemesis :life] 1)
      (update-in [:nemesis :acolytes] (comp vec shuffle))
      (push-effect-stack {:effects (concat [[::draw-acolyte]
                                            [::draw-acolyte]]
                                           (when (#{:beginner} difficulty)
                                             [[:move-card {:from          :acolytes
                                                           :from-position :top
                                                           :to            :discard}]]))})))

(effects/register {::setup setup})

(defn unleash-text [{:keys [difficulty]}]
  (if (#{:expert :extinction} difficulty)
    "Resolve the Blood Magic effect of each acolyte in play without those acolytes suffering damage from their Blood Magic effects."
    "Resolve the Blood Magic effect of the acolyte with the highest life."))

(effects/register-predicates {::unleash-text unleash-text})

(defn victory-condition [{:keys [current-player] :as game}]
  (let [acolytes-in-play (->> (get-in game [:nemesis :play-area])
                              (filter (comp #{:acolyte} :type)))]
    (when (and (empty? acolytes-in-play)
               (= :no-one current-player))                  ; at the end of a turn
      {:conclusion :victory
       :text       "All Hollow Crown's acolytes have been defeated."})))

(effects/register-predicates {::victory-condition victory-condition})

(def hollow-crown {:name                :hollow-crown
                   :level               6
                   :setup               [[::setup]]
                   :unleash             [[::unleash]]
                   :unleash-text        ::unleash-text
                   :additional-rules    ["- Hollow Crow cannot be dealt damage."
                                         "- When there are no acolytes in play at the end of any turn, the players win the game."
                                         "- When an acolyte is discarded, place it in the acolyte discard pile and draw a new acolyte."
                                         "- When multiple Blood Magic effects would be resolved at the same time, resolve them in order starting with the acolyte that as been in play the longest."
                                         "- When a player is exhausted, Gravehold suffers 4 damage instead of the nemesis Unleashing twice. Resolve the rest of the On Exhaust effects."]
                   :max-damage          0
                   :victory-condition   ::victory-condition
                   :on-player-exhausted [[:damage-gravehold 4]]
                   :acolytes            (->> (range 1 9)
                                             (map (fn [n]
                                                    {:name        (str "Acolyte " n)
                                                     :type        :acolyte
                                                     :life        11
                                                     :blood-magic {:text    "Any player suffers 1 damage."
                                                                   :effects [[:give-choice {:title   :acolyte
                                                                                            :text    "Any player suffers 1 damage."
                                                                                            :choice  [:damage-player {:arg 1}]
                                                                                            :options [:players]
                                                                                            :min     1
                                                                                            :max     1}]]}
                                                     :when-killed [[::draw-acolyte]]}))
                                             (concat [edryss-tragg]))
                   :cards               [(minion/generic 1) (power/generic 1) (attack/generic 1)
                                         (minion/generic 2) (power/generic 2) (attack/generic 2)
                                         (minion/generic 3) (power/generic 3) (attack/generic 3)]})
