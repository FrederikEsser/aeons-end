(ns aeons-end.nemeses.hollow-crown
  (:require [aeons-end.operations :refer [push-effect-stack move-card add-card]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]
            [medley.core :as medley]
            [aeons-end.cards.power :as power]))

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
                                                                 :effect  [::resolve-blood-magic {:avoid-self-damage true}]
                                                                 :options [:nemesis :play-area {:name name}]
                                                                 :min     1
                                                                 :max     1}])))})))

(defn do-unleash [{:keys [difficulty] :as game} args]
  (let [title (keyword (or (:resolving args)
                           (:resolving game))
                       "unleash")]
    (push-effect-stack game {:effects (if (#{:expert :extinction} difficulty)
                                        [[::resolve-all-acolytes {:title title}]]
                                        [[:give-choice {:title   title
                                                        :text    "Resolve the Blood Magic effect of the acolyte with the highest life."
                                                        :effect  ::resolve-blood-magic
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

(defn heal-acolyte [game {:keys [card-name arg]}]
  (let [{{:keys [life max-life]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})]
    (ut/update-in-vec game [:nemesis :play-area] {:name card-name} assoc :life (min (+ life arg)
                                                                                    max-life))))

(effects/register {::heal-acolyte heal-acolyte})

(defn shuffle-acolytes [game _]
  (update-in game [:nemesis :acolytes] (comp vec shuffle)))

(effects/register {::shuffle-acolytes shuffle-acolytes})

(defn bezu-damage [{:keys [players] :as game} {:keys [avoid-self-damage]}]
  (let [damage (->> players
                    (map ut/count-prepped-spells)
                    (apply max))]
    (push-effect-stack game {:effects (concat [[:damage-gravehold damage]]
                                              (when-not avoid-self-damage
                                                [[:deal-damage-to-minion {:card-name :bezu
                                                                          :damage    damage}]]))})))

(effects/register {::bezu-damage bezu-damage})

(def bezu {:name        :bezu
           :type        :acolyte
           :life        11
           :blood-magic {:text    "Gravehold and this minion suffer 1 damage for each spell prepped by the player with the most prepped spells."
                         :effects [[::bezu-damage]]}
           :when-killed [[::draw-acolyte]]
           :quote       "'We find wisdom in their great cruelty.'"})

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
  (push-effect-stack game {:effects [[:give-choice {:title   :edryss-tragg
                                                    :text    "Any player suffers 2 damage."
                                                    :effect  (if avoid-self-damage
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
                                                      :effect  ::holadran-damage
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
                                                    :effect  [:damage-player {:arg 1}]
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
                                     [:give-choice {:title   :lurzan
                                                    :text    "The player with the lowest life suffers 1 damage."
                                                    :effect  [:damage-player {:arg 1}]
                                                    :options [:players {:lowest-life true}]
                                                    :min     1
                                                    :max     1}]]}
             :when-killed [[::draw-acolyte]]
             :quote       "'We call the weak to feed the worthy."})

(defn mord-damage [game {:keys [player-no avoid-self-damage] :as args}]
  (push-effect-stack game {:player-no player-no
                           :effects   (concat [[:spend-charges 1]
                                               [:damage-gravehold 1]]
                                              (when-not avoid-self-damage
                                                [[:deal-damage-to-minion {:card-name :mord
                                                                          :damage    1}]]))}))

(defn mord-choice [game {:keys [avoid-self-damage] :as args}]
  (push-effect-stack game {:effects [[:give-choice {:title   :mord
                                                    :text    "Any player loses 1 charge, Gravehold suffers 1 damage, and this minion suffers 1 damage."
                                                    :effect  [::mord-damage {:avoid-self-damage avoid-self-damage}]
                                                    :options [:players :ability {:min-charges 1}]
                                                    :or      {:text    "Gravehold suffers 3 damage."
                                                              :effects [[:damage-gravehold 3]]}
                                                    :max     1}]]}))

(effects/register {::mord-damage mord-damage
                   ::mord-choice mord-choice})

(def mord {:name        :mord
           :type        :acolyte
           :life        11
           :blood-magic {:text    ["Gravehold suffers 3 damage."
                                   "OR"
                                   "Any player loses 1 charge, Gravehold suffers 1 damage, and this minion suffers 1 damage."]
                         :effects [[::mord-choice]]}
           :when-killed [[::draw-acolyte]]
           :quote       "'We wipe the sister-words from our tongues to taste oblivion.'"})

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

(defn ren-goda-discard [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 1]
                                       [:discard-from-hand {:card-name card-name}]]}))

(defn ren-goda-damage [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   "Ren-Goda"
                                                      :text    "Discard the most expensive card in your hand and suffer 1 damage."
                                                      :effect  ::ren-goda-discard
                                                      :options [:player :hand {:most-expensive true}]
                                                      :or      {:text    "Suffer 2 damage."
                                                                :effects [[:damage-player 2]]}
                                                      :max     1}]]}))

(effects/register {::ren-goda-discard ren-goda-discard
                   ::ren-goda-damage  ren-goda-damage})

(def ren-goda {:name        :ren-goda
               :name-ui     "Ren-Goda"
               :type        :acolyte
               :life        11
               :blood-magic {:text    ["Any player suffers 2 damage."
                                       "OR"
                                       "Any player suffers 1 damage and discards their most expensive card in hand."]
                             :effects [[:give-choice {:title   "Ren-Goda"
                                                      :text    ["Any player suffers 2 damage."
                                                                "OR"
                                                                "Any player suffers 1 damage and discards their most expensive card in hand."]
                                                      :effect  ::ren-goda-damage
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]}
               :when-killed [[::draw-acolyte]]
               :quote       "'We honor the void in blood.'"})

(defn solara-discard [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :solara
                                                      :text    "Discard 2 cards in hand."
                                                      :effect  :discard-from-hand
                                                      :options [:player :hand]
                                                      :min     2
                                                      :max     2}]]}))

(defn solara-choice [{:keys [players] :as game} _]
  (let [max-cards (->> players
                       (map ut/count-cards-in-hand)
                       (apply max 0))]
    (push-effect-stack game {:effects [[:give-choice {:title   :solara
                                                      :text    "Any player discards 2 cards in hand."
                                                      :effect  ::solara-discard
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

(defn ascend-heal-and-resolve [game {:keys [card-name]}]
  (push-effect-stack game {:effects [[::heal-acolyte {:card-name card-name
                                                      :arg       10}]
                                     [::resolve-blood-magic {:card-name card-name}]]}))

(effects/register {::ascend-heal-and-resolve ascend-heal-and-resolve})

(def ascend {:name    :ascend
             :type    :attack
             :tier    2
             :text    ["Unleash."
                       "The acolyte with the lowest life gains 10 life. Resolve that acolyte's Blood Effect."]
             :effects [[:unleash]
                       [:give-choice {:title   :ascend
                                      :text    "The acolyte with the lowest life gains 10 life. Resolve that acolyte's Blood Effect."
                                      :effect  ::ascend-heal-and-resolve
                                      :options [:nemesis :minions {:type        :acolyte
                                                                   :lowest-life true}]
                                      :min     1
                                      :max     1}]]
             :quote   "'Perhaps it is not our doom, Brama. Perhaps it is our very maker.' Xaxos, Voidbringer"})

(defn beseech-return-acolyte [game {:keys [card-name]}]
  (push-effect-stack game {:effects [[::heal-acolyte {:card-name card-name
                                                      :arg       10}]
                                     [:move-card {:card-name card-name
                                                  :from      :play-area
                                                  :to        :acolytes}]]}))

(effects/register {::beseech-return-acolyte beseech-return-acolyte})

(def beseech {:name    :beseech
              :type    :attack
              :tier    1
              :text    ["Place the acolyte in play with the lowest life on the bottom of the acolyte deck. Draw an acolyte."
                        "Unleash."]
              :effects [[:give-choice {:title   :beseech
                                       :text    "Place the acolyte in play with the lowest life on the bottom of the acolyte deck."
                                       :effect  ::beseech-return-acolyte
                                       :options [:nemesis :minions {:type        :acolyte
                                                                    :lowest-life true}]
                                       :min     1
                                       :max     1}]
                        [::draw-acolyte]
                        [:unleash]]
              :quote   "'The Hollow Crown gestured to the acolyte, who removed its mask and walked into the blinding light within the breach.'"})

(def dominate {:name    :dominate
               :type    :attack
               :tier    3
               :text    ["Resolve the Blood Magic effect of each acolyte in play without those acolytes suffering damage from their Blood Magic effects."
                         "Repeat this one additional time."]
               :effects [[::resolve-all-acolytes {:title :dominate}]
                         [::resolve-all-acolytes {:title :dominate}]]
               :quote   "'Behind their masks is nothing, youngling. They have made their choices.' Gex, Breach Mage Advisor"})

(def enslave {:name    :enslave
              :type    :attack
              :tier    1
              :text    "Resolve the Blood Magic effect of each acolyte in play without those acolytes suffering damage from their Blood Magic effects."
              :effects [[::resolve-all-acolytes {:title :enslave}]]
              :quote   "'It has but one weapon: blood obedience.'"})

(def infernal-domain {:name       :infernal-domain
                      :type       :power
                      :tier       2
                      :to-discard {:text      "Spend 8 Aether."
                                   :predicate [::power/can-afford? {:amount 8}]
                                   :effects   [[:pay {:amount 8
                                                      :type   :discard-power-card}]]}
                      :power      {:power   2
                                   :text    ["Draw an acolyte."
                                             "Resolve the Blood Magic effect of each acolyte in play without those acolytes suffering damage from their Blood Magic effects."]
                                   :effects [[::draw-acolyte]
                                             [::resolve-all-acolytes {:title :infernal-domain}]]}})

(def nameless-faith {:name       :nameless-faith
                     :type       :power
                     :tier       3
                     :to-discard {:text      "Spend 8 Aether."
                                  :predicate [::power/can-afford? {:amount 8}]
                                  :effects   [[:pay {:amount 8
                                                     :type   :discard-power-card}]]}
                     :power      {:power   1
                                  :text    ["Shuffle the two most recently discarded acolytes in the acolyte discard pile into the acolyte deck."
                                            "Resolve the Blood Magic effect of each acolyte in play without those acolytes suffering damage from their Blood Magic effects."]
                                  :effects [[:give-choice {:title   :nameless-faith
                                                           :text    "Shuffle the two most recently discarded acolytes in the acolyte discard pile into the acolyte deck."
                                                           :effect  [:move-card {:from :discard
                                                                                 :to   :acolytes}]
                                                           :options [:nemesis :discard {:type :acolyte :most-recent true}]
                                                           :min     1
                                                           :max     1}]
                                            [:give-choice {:title   :nameless-faith
                                                           :text    "Shuffle the most recently discarded acolyte in the acolyte discard pile into the acolyte deck."
                                                           :effect  [:move-card {:from :discard
                                                                                 :to   :acolytes}]
                                                           :options [:nemesis :discard {:type :acolyte :most-recent true}]
                                                           :min     1
                                                           :max     1}]
                                            [::shuffle-acolytes]
                                            [::resolve-all-acolytes {:title :nameless-faith}]]}})

(defn reign-resolve [game {:keys [card-name]}]
  (push-effect-stack game {:effects [[::resolve-blood-magic {:card-name card-name}]
                                     [:deal-damage-to-minion {:card-name card-name
                                                              :damage    6}]]}))

(effects/register {::reign-resolve reign-resolve})

(def reign {:name    :reign
            :type    :attack
            :tier    2
            :text    ["Resolve the Blood Magic effect of the acolyte with the most life. That acolyte suffers 6 damage."
                      "Unleash twice."]
            :effects [[:give-choice {:title   :reign
                                     :text    "Resolve the Blood Magic effect of the acolyte with the most life. That acolyte suffers 6 damage."
                                     :effect  ::reign-resolve
                                     :options [:nemesis :minions {:type      :acolyte
                                                                  :most-life true}]
                                     :min     1
                                     :max     1}]
                      [:unleash]
                      [:unleash]]
            :quote   "'As it passed through the city, men and women dropped to their knees faithfully, their eyes heavy with tears of awe.'"})

(defn thronewatch-resolve [game {:keys [card-name]}]
  (push-effect-stack game {:effects [[::resolve-blood-magic {:card-name card-name}]
                                     [:deal-damage-to-minion {:card-name card-name
                                                              :damage    1}]]}))

(effects/register {::thronewatch-resolve thronewatch-resolve})

(def thronewatch {:name       :thronewatch
                  :type       :minion
                  :tier       1
                  :life       6
                  :persistent {:text    "Resolve the Blood Magic effect of the acolyte with the lowest life. That acolyte suffers 1 damage."
                               :effects [[:give-choice {:title   :thronewatch
                                                        :text    "Resolve the Blood Magic effect of the acolyte with the lowest life. That acolyte suffers 1 damage."
                                                        :effect  ::thronewatch-resolve
                                                        :options [:nemesis :minions {:type        :acolyte
                                                                                     :lowest-life true}]
                                                        :min     1
                                                        :max     1}]]}})

(defn viscera-bride-heal-and-resolve [game {:keys [card-name]}]
  (push-effect-stack game {:effects [[::heal-acolyte {:card-name card-name
                                                      :arg       10}]
                                     [::resolve-blood-magic {:card-name card-name}]
                                     [::resolve-blood-magic {:card-name card-name}]]}))

(effects/register {::viscera-bride-heal-and-resolve viscera-bride-heal-and-resolve})

(def viscera-bride {:name       :viscera-bride
                    :type       :minion
                    :tier       3
                    :life       18
                    :persistent {:text    "The acolyte with the lowest life gains 10 life. Resolve that acolyte's Blood Magic effect twice."
                                 :effects [[:give-choice {:title   :viscera-bride
                                                          :text    "The acolyte with the lowest life gains 10 life. Resolve that acolyte's Blood Magic effect twice."
                                                          :effect  ::viscera-bride-heal-and-resolve
                                                          :options [:nemesis :minions {:type        :acolyte
                                                                                       :lowest-life true}]
                                                          :min     1
                                                          :max     1}]]}
                    :quote      "'All kings must have a queen.' Dezmodia, Voidborn Prodigy"})

(defn setup [{:keys [difficulty] :as game} _]
  (-> game
      (assoc-in [:nemesis :life] 1)
      (push-effect-stack {:effects (concat [[::shuffle-acolytes]
                                            [::draw-acolyte]
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
                   :acolytes            [bezu edryss-tragg holadran
                                         kurgax lurzan mord
                                         nhavkalas ren-goda solara]
                   :cards               [beseech enslave thronewatch
                                         ascend infernal-domain reign
                                         dominate nameless-faith viscera-bride]})
