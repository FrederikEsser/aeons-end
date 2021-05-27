(ns aeons-end.nemeses.umbra-titan
  (:require [aeons-end.operations :refer [push-effect-stack move-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack]))

(defn lose-nemesis-tokens [game {:keys [arg]}]
  (update-in game [:nemesis :tokens] - arg))

(effects/register {:lose-nemesis-tokens lose-nemesis-tokens})

(defn crumble-revive-minion [game {:keys [card-name]}]
  (-> game
      (push-effect-stack {:effects [[:move-card {:card-name card-name
                                                 :from      :discard
                                                 :to        :play-area}]
                                    [:unleash]]})))

(effects/register {::crumble-revive-minion crumble-revive-minion})

(def crumble {:name    :crumble
              :type    :attack
              :tier    3
              :text    ["Place the most recently discarded minion in the nemesis discard pile back into play. Unleash."
                        "OR"
                        "Umbra Titan loses three nemesis tokens."]
              :effects [[:give-choice {:title     :crumble
                                       :text      "Place the most recently discarded minion in the nemesis discard pile back into play. Unleash."
                                       :choice    ::crumble-revive-minion
                                       :or-choice {:text    "Umbra Titan loses three nemesis tokens."
                                                   :effects [[:lose-nemesis-tokens 3]]}
                                       :options   [:nemesis-discard {:type :minion :most-recent true}]
                                       :max       1}]]})

(def cryptid {:name       :cryptid
              :type       :minion
              :tier       1
              :life       6
              :persistent {:text    ["The player with the most expensive prepped spell discards that spell."
                                     "OR"
                                     "Umbra Titan loses one nemesis token."]
                           :effects [[:give-choice {:title     :cryptid
                                                    :text      "The player with the most expensive prepped spell discards that spell."
                                                    :choice    :discard-prepped-spells
                                                    :or-choice {:text    "Umbra titan loses one nemesis token"
                                                                :effects [[:lose-nemesis-tokens 1]]}
                                                    :options   [:prepped-spells {:most-expensive true}]
                                                    :max       1}]]}
              :quote      "'The beasts of this cave seem to revere the Titan as though it were some ancient god.' Mazhaedron, Henge Mystic"})

(defn grubber-persistent [game _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (push-effect-stack game {:effects (if (= 2 discarded-nemesis-cards)
                                        [[:lose-nemesis-tokens 1]]
                                        [[:damage-gravehold 2]])})))

(effects/register {::grubber-persistent grubber-persistent})

(def grubber {:name       :grubber
              :type       :minion
              :tier       1
              :life       5
              :persistent {:text    ["If the nemesis has two turn order cards in the turn order discard pile, Umbra Titan loses one nemesis token."
                                     "Otherwise, Gravehold suffers 2 damage."]
                           :effects [[::grubber-persistent]]}
              :quote      "'Kick it, stab it, burn it... the thing just keeps grinning.' Sparrow, Breach Mage Soldier"})

(defn maul-choice [{:keys [players] :as game} {:keys [choice]}]
  (case choice
    :lose-tokens (push-effect-stack game {:effects [[:lose-nemesis-tokens 2]]})
    :destroy (let [sorted-spells (->> players
                                      (map-indexed (fn [player-no {:keys [breaches]}]
                                                     (->> breaches
                                                          (map-indexed (fn [breach-no breach]
                                                                         (->> (:prepped-spells breach)
                                                                              (map (fn [{:keys [name cost]}]
                                                                                     {:player-no player-no
                                                                                      :breach-no breach-no
                                                                                      :card-name name
                                                                                      :cost      cost})))))
                                                          (apply concat))))
                                      (apply concat)
                                      (sort-by :cost >))
                   [_ cost-2] (map :cost sorted-spells)]
               (push-effect-stack game {:effects (concat
                                                   [[:destroy-prepped-spells (first sorted-spells)]
                                                    [:give-choice {:title   :maul
                                                                   :text    "The players collectively destroy the most expensive prepped spell."
                                                                   :choice  :destroy-prepped-spells
                                                                   :options [:prepped-spells {:min-cost cost-2}]
                                                                   :min     1
                                                                   :max     1}]])}))))

(defn maul-give-choice [{:keys [players] :as game} _]
  (let [[cost-1 cost-2 cost-3] (->> players
                                    (mapcat :breaches)
                                    (mapcat :prepped-spells)
                                    (map :cost)
                                    (sort >))]
    (push-effect-stack game {:effects (cond
                                        (nil? cost-2) [[:lose-nemesis-tokens 2]]
                                        (or (= cost-1 cost-2)
                                            (not= cost-2 cost-3)) [[:give-choice {:title     :maul
                                                                                  :text      "The players collectively destroy the two most expensive prepped spells."
                                                                                  :choice    :destroy-prepped-spells
                                                                                  :or-choice {:text    "Umbra Titan loses two nemesis tokens"
                                                                                              :effects [[:lose-nemesis-tokens 2]]}
                                                                                  :options   [:prepped-spells {:min-cost cost-2}]
                                                                                  :min       2
                                                                                  :max       2
                                                                                  :optional? true}]]
                                        :else [[:give-choice {:title   :maul
                                                              :choice  ::maul-choice
                                                              :options [:special
                                                                        {:option :destroy :text "The players collectively destroy the two most expensive prepped spells"}
                                                                        {:option :lose-tokens :text "Umbra Titan loses two nemesis tokens"}]
                                                              :min     1
                                                              :max     1}]])})))

(effects/register {::maul-choice      maul-choice
                   ::maul-give-choice maul-give-choice})

(def maul {:name    :maul
           :type    :attack
           :tier    2
           :text    ["The players collectively destroy the two most expensive prepped spells."
                     "OR"
                     "Umbra Titan loses two nemesis tokens."]
           :effects [[::maul-give-choice]]})

(defn seismic-roar-can-discard? [game {:keys [player-no]}]
  (let [aether (or (get-in game [:players player-no :aether]) 0)]
    (>= aether 6)))

(effects/register-predicates {::seismic-roar-can-discard? seismic-roar-can-discard?})

(def seismic-roar {:name       :seismic-roar
                   :type       :power
                   :tier       1
                   :to-discard {:text      "Spend 6 Aether."
                                :predicate ::seismic-roar-can-discard?
                                :effects   [[:pay 6]]}
                   :power      {:power   3
                                :text    "Umbra Titan loses two nemesis tokens."
                                :effects [[:lose-nemesis-tokens 2]]}
                   :quote      "'I roared as it bore through the rock. And Gravehold shuddered like those within its walls.' Nerva, Survivor"})

(defn tombfright-persistent [game _]
  (let [tokens (get-in game [:nemesis :tokens])]
    (push-effect-stack game {:effects (if (>= tokens 5)
                                        [[:lose-nemesis-tokens 1]]
                                        [[:damage-gravehold 3]])})))

(effects/register {::tombfright-persistent tombfright-persistent})

(def tombfright {:name       :tombfright
                 :type       :minion
                 :tier       2
                 :life       8
                 :persistent {:text    ["If Umbra Titan has 5 or more nemesis tokens, it loses a nemesis token."
                                        "Otherwise, Gravehold suffers 3 damage."]
                              :effects [[::tombfright-persistent]]}
                 :quote      "'With two heads, it's twice as eager to make a meal of a mage.' ― Sparrow, Breach Mage Soldier"})

(defn vault-behemoth-lose-token [game _]
  (let [{{:keys [life]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name :vault-behemoth})]
    (cond-> game
            (<= life 8) (push-effect-stack {:effects [[:lose-nemesis-tokens 1]]}))))

(effects/register {::vault-behemoth-lose-token vault-behemoth-lose-token})

(def vault-behemoth {:name       :vault-behemoth
                     :type       :minion
                     :tier       2
                     :life       9
                     :persistent {:text    "Any player suffers 2 damage. If this minion has 8 or less life, Umbra Titan loses one nemesis token."
                                  :effects [[:give-choice {:title   :vault-behemoth
                                                           :text    "Any player suffers 2 damage."
                                                           :choice  [:damage-player {:arg 2}]
                                                           :options [:players]
                                                           :min     1
                                                           :max     1}]
                                            [::vault-behemoth-lose-token]]}
                     :quote      "'The air rasping in its massive lungs in enough to burst your eardrums.'"})

(def demi-ancient {:name       :demi-ancient
                   :type       :minion
                   :tier       3
                   :life       18
                   :persistent {:text    "Umbra Titan loses one nemesis token."
                                :effects [[:lose-nemesis-tokens 1]]}
                   :quote      "'In the time before ours, their kind thrived in the tumult of the fledgling world. Now, they seek a new home among The Nameless.' ― Mazahaedron, Henge Mystic"})

(defn yawning-black-can-discard? [game {:keys [player-no]}]
  (let [aether (or (get-in game [:players player-no :aether]) 0)]
    (>= aether 8)))

(effects/register-predicates {::yawning-black-can-discard? yawning-black-can-discard?})

(def yawning-black {:name       :yawning-black
                    :type       :power
                    :tier       3
                    :to-discard {:text      "Spend 8 Aether."
                                 :predicate ::yawning-black-can-discard?
                                 :effects   [[:pay 8]]}
                    :power      {:power   2
                                 :text    ["Any player suffers 6 damage."
                                           "OR"
                                           "Umbra Titan loses three nemesis tokens."]
                                 :effects [[:give-choice {:title     :yawning-black
                                                          :text      "Any player suffers 6 damage."
                                                          :choice    [:damage-player {:arg 6}]
                                                          :or-choice {:text    "Umbra Titan loses three nemesis tokens."
                                                                      :effects [[:lose-nemesis-tokens 3]]}
                                                          :options   [:players]
                                                          :max       1}]]}
                    :quote      "The Titan hides beneath the skin of the cave, emerging only to strike."})

(defn umbra-titan-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :damage [[:damage-gravehold 2]]
                                      :token [[:lose-nemesis-tokens 1]])}))

(defn umbra-titan-unleash [game _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (assert (<= 1 discarded-nemesis-cards 2) (str "Turn order error: There are " discarded-nemesis-cards " nemesis turn order cards in the turn order discard pile."))
    (push-effect-stack game {:effects [[:give-choice (case discarded-nemesis-cards
                                                       1 {:title     :unleash
                                                          :text      "Any player suffers 2 damage."
                                                          :choice    [:damage-player {:arg 2}]
                                                          :or-choice {:text    "Umbra titan loses one nemesis token"
                                                                      :effects [[:lose-nemesis-tokens 1]]}
                                                          :options   [:players]
                                                          :max       1}
                                                       2 {:title   :unleash
                                                          :choice  ::umbra-titan-choice
                                                          :options [:special
                                                                    {:option :damage :text "Gravehold suffers 2 damage"}
                                                                    {:option :token :text "Umbra titan loses one nemesis token"}]
                                                          :min     1
                                                          :max     1})]]})))

(effects/register {::umbra-titan-choice  umbra-titan-choice
                   ::umbra-titan-unleash umbra-titan-unleash})

(def umbra-titan {:name       :umbra-titan
                  :difficulty 3
                  :life       70
                  :tokens     8
                  :unleash    [[::umbra-titan-unleash]]
                  :cards      [cryptid grubber seismic-roar
                               maul tombfright vault-behemoth
                               crumble demi-ancient yawning-black]})
