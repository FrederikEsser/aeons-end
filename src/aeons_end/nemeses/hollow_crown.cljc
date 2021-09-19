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

(defn holadran-damage [game {:keys [player-no player-card-names]}]
  (let [player-numbers (or (some->> player-card-names
                                    (map :player-no))
                           (when player-no
                             [player-no player-no]))]
    (push-effect-stack game {:effects (->> player-numbers
                                           (map (fn [player-no]
                                                  [:damage-player {:player-no player-no :arg 1}])))})))

(effects/register {::holadran-damage holadran-damage})

(def holadran {:name        :holadran
               :type        :acolyte
               :life        11
               :blood-magic {:text    "Two different players each suffer 1 damage."
                             :effects [[:give-choice {:title   :holadran
                                                      :text    "Two different players each suffer 1 damage."
                                                      :choice  ::holadran-damage
                                                      :options [:players]
                                                      :min     2
                                                      :max     2}]]}
               :when-killed [[::draw-acolyte]]
               :quote       "'We renounce our existence behind each mask.'"})

(def kurgax {:name        :kurgax
             :type        :acolyte
             :life        11
             :blood-magic {:text    ["Gravehold suffers 1 damage."
                                     "Any player suffers 1 damage."]
                           :effects [[:damage-gravehold 1]
                                     [:give-choice {:title   :kurgax
                                                    :text    "Any player suffers 1 damage."
                                                    :choice  [:damage-player {:arg 1}]
                                                    :options [:players]
                                                    :min     1
                                                    :max     1}]]}
             :when-killed [[::draw-acolyte]]
             :quote       "'We hunger to be nothing.'"})

(def lurzan {:name        :lurzan
             :type        :acolyte
             :life        11
             :blood-magic {:text    "The player with the lowest life suffers 1 damage."
                           :effects [[:damage-gravehold 1]
                                     [:give-choice {:title   :kurgax
                                                    :text    "The player with the lowest life suffers 1 damage."
                                                    :choice  [:damage-player {:arg 1}]
                                                    :options [:players {:lowest-life true}]
                                                    :min     1
                                                    :max     1}]]}
             :when-killed [[::draw-acolyte]]
             :quote       "'We call the weak to feed the worthy."})

(def nhavkalas {:name        :nhavkalas
                :type        :acolyte
                :life        11
                :blood-magic {:text    ["Gravehold suffers 3 damage."
                                        "This minion suffers 1 damage."]
                              :effects [[:damage-gravehold 3]
                                        [:deal-damage-to-minion {:card-name :nhavkalas
                                                                 :damage    1}]]}
                :when-killed [[::draw-acolyte]]
                :quote       "'We cast aside the haven that has become our shackle.'"})

(defn solara-discard [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :solara
                                                      :text    "Discard 2 cards in hand."
                                                      :choice  :discard-from-hand
                                                      :options [:player :hand]
                                                      :min     2
                                                      :max     2}]]}))

(defn solara-choice [{:keys [players] :as game} _]
  (let [max-cards (->> players
                       (map ut/count-cards-in-hand)
                       (apply max 0))]
    (push-effect-stack game {:effects [[:give-choice {:title   :solara
                                                      :text    "Any player discards 2 cards in hand."
                                                      :choice  ::solara-discard
                                                      :options [:players {:min-hand (min 2 max-cards)}]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::solara-discard solara-discard
                   ::solara-choice  solara-choice})

(def solara {:name        :solara
             :type        :acolyte
             :life        11
             :blood-magic {:text    ["Any player discards 2 cards in hand."
                                     "This minion suffers 3 damage."]
                           :effects [[::solara-choice]
                                     [:deal-damage-to-minion {:card-name :solara
                                                              :damage    3}]]}
             :when-killed [[::draw-acolyte]]
             :quote       "'We wield the breach in reverence to those from beyond it.'"})

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
                   :acolytes            (->> (range 1 4)
                                             (map (fn [n]
                                                    {:name        (str "Acolyte " n)
                                                     :type        :acolyte
                                                     :life        11
                                                     :blood-magic {:text    "Any player suffers 2 damage."
                                                                   :effects [[:give-choice {:title   :acolyte
                                                                                            :text    "Any player suffers 2 damage."
                                                                                            :choice  [:damage-player {:arg 2}]
                                                                                            :options [:players]
                                                                                            :min     1
                                                                                            :max     1}]]}
                                                     :when-killed [[::draw-acolyte]]}))
                                             (concat [edryss-tragg holadran kurgax
                                                      lurzan nhavkalas solara]))
                   :cards               [(minion/generic 1) (power/generic 1) (attack/generic 1)
                                         (minion/generic 2) (power/generic 2) (attack/generic 2)
                                         (minion/generic 3) (power/generic 3) (attack/generic 3)]})
