(ns aeons-end.nemeses.rageborne
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.power :as power]))

(defn gain-fury [game _]
  (update-in game [:nemesis :fury] ut/plus 1))

(defn lose-fury [game {:keys [arg]}]
  (let [fury (get-in game [:nemesis :fury])]
    (assoc-in game [:nemesis :fury] (max (- fury arg) 0))))

(defn resolve-strike [{:keys [difficulty] :as game} {:keys [card-name]}]
  (let [{{:keys [effects]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name card-name})]
    (-> game
        (push-effect-stack {:effects (concat
                                       [[:set-resolving {:card-name card-name}]]
                                       effects
                                       [[:clear-resolving]
                                        [::lose-fury (if (#{:expert :extinction} difficulty) 1 3)]
                                        [:move-card {:card-name card-name
                                                     :from      :play-area
                                                     :to        :discard}]])}))))

(defn strike [{:keys [nemesis] :as game} _]
  (let [{:keys [name] :as card} (->> nemesis :strike-deck first)]
    (-> game
        (update-in [:nemesis :play-area] (comp vec concat) [card])
        (update-in [:nemesis :strike-deck] shuffle)
        (push-effect-stack {:effects [[:give-choice {:title   "Rageborne strikes!"
                                                     :text    (str "Resolve " (ut/format-name name) ".")
                                                     :choice  ::resolve-strike
                                                     :options [:nemesis :play-area {:name name}]
                                                     :min     1
                                                     :max     1}]]}))))

(defn after-effects [game _]
  (let [fury (get-in game [:nemesis :fury])]
    (cond-> game
            (<= 4 fury) (strike {}))))

(effects/register {::gain-fury      gain-fury
                   ::lose-fury      lose-fury
                   ::resolve-strike resolve-strike
                   ::strike         strike
                   ::after-effects  after-effects})

(def avatar-of-wrath {:name       :avatar-of-wrath
                      :type       :minion
                      :tier       3
                      :life       16
                      :persistent {:text    "Rageborne Strikes."
                                   :effects [[::strike]]}
                      :quote      "'Many a mage found their end in the gullet of this beast.'"})

(defn blood-cry-can-discard? [game {:keys [player-no]}]
  (let [charges (get-in game [:players player-no :ability :charges])]
    (<= 4 charges)))

(effects/register-predicates {::blood-cry-can-discard? blood-cry-can-discard?})

(def blood-cry {:name       :blood-cry
                :type       :power
                :tier       2
                :to-discard {:text      "Lose 4 charges."
                             :predicate ::blood-cry-can-discard?
                             :effects   [[:spend-charges 4]]}
                :power      {:power   2
                             :text    "Unleash 4 times."
                             :effects [[:unleash]
                                       [:unleash]
                                       [:unleash]
                                       [:unleash]]}
                :quote      "'The cave walls crumble and quake as it bellows to its kin.'"})

(def cleave {:name    :cleave
             :type    :attack
             :tier    1
             :text    "Rageborne Strikes."
             :effects [[::strike]]
             :quote   "'Thraxir was breach commander for as long as most could remember. To honor him, to this very day no man or woman has been elected to the post in his stead.'"})

(defn invoke-carnage-can-discard? [game {:keys [player-no]}]
  (power/can-afford? game {:player-no player-no :amount 7}))

(effects/register-predicates {::invoke-carnage-can-discard? invoke-carnage-can-discard?})

(defn invoke-carnage-damage [game _]
  (let [damage (inc (get-in game [:nemesis :fury]))]
    (push-effect-stack game {:effects [[:give-choice {:title   :invoke-carnage
                                                      :text    (str "Any player suffers " damage " damage.")
                                                      :choice  [:damage-player {:arg damage}]
                                                      :options [:players]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::invoke-carnage-damage invoke-carnage-damage})

(def invoke-carnage {:name       :invoke-carnage
                     :type       :power
                     :tier       2
                     :to-discard {:text      "Spend 7 Aether."
                                  :predicate ::invoke-carnage-can-discard?
                                  :effects   [[:pay {:amount 7
                                                     :type   :discard-power-card}]]}
                     :power      {:power   2
                                  :text    ["Unleash."
                                            "Any player suffers 1 damage. That player suffers 1 additional damage for each Fury token Rageborne has."]
                                  :effects [[:unleash]
                                            [::invoke-carnage-damage]]}
                     :quote      "'There is but a single power among The Nameless that can shatter Gravehold.' Brama, Breach Mage Elder"})

(defn onslaught-discard [game _]
  (let [fury (get-in game [:nemesis :fury])]
    (push-effect-stack game {:effects [[:give-choice {:title   :invoke-carnage
                                                      :text    (str "The players collectively discard " (ut/number->text fury) " cards in hand.")
                                                      :choice  :collective-discard-from-hand
                                                      :options [:players :hand]
                                                      :min     fury
                                                      :max     fury}]]})))

(effects/register {::onslaught-discard onslaught-discard})

(def onslaught {:name    :onslaught
                :type    :attack
                :tier    3
                :text    ["Unleash three times."
                          "The player collectively discard cards in hand equal to the number of Fury tokens Rageborne has."]
                :effects [[:unleash]
                          [:unleash]
                          [:unleash]
                          [::onslaught-discard]]
                :quote   "'Even its shadow leaves a wake of carnage, smoke, and screams.'"})

(defn provoker-damage [game _]
  (let [fury (get-in game [:nemesis :fury])]
    (push-effect-stack game {:effects [[:damage-gravehold fury]]})))

(effects/register {::provoker-damage provoker-damage})

(def provoker {:name       :provoker
               :type       :minion
               :tier       1
               :life       5
               :persistent {:text    "Gravehold suffers damage equal to the number of Fury tokens Rageborne has."
                            :effects [[::provoker-damage]]}
               :quote      "'I nearly wasted a freshly forged chopper on one of these things.' Reeve, Breach Mage Elite"})

(defn rolling-death-can-discard? [game {:keys [player-no]}]
  (power/can-afford? game {:player-no player-no :amount 8}))

(effects/register-predicates {::rolling-death-can-discard? rolling-death-can-discard?})

(def rolling-death {:name       :rolling-death
                    :type       :power
                    :tier       3
                    :to-discard {:text      "Spend 8 Aether."
                                 :predicate ::rolling-death-can-discard?
                                 :effects   [[:pay {:amount 8
                                                    :type   :discard-power-card}]]}
                    :power      {:power   2
                                 :text    "Rageborne Strikes twice."
                                 :effects [[::strike]
                                           [::strike]]}
                    :quote      "Its breath rasped as it turned the black rocks red. And those who could, ran.' Nerva, Survivor"})

(defn scorn-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :damage [[:damage-gravehold 3]]
                                      :unleash [[:unleash]
                                                [:unleash]])}))

(effects/register {::scorn-choice scorn-choice})

(def scorn {:name       :scorn
            :type       :minion
            :tier       2
            :life       9
            :persistent {:text    ["Gravehold suffers 3 damage."
                                   "OR"
                                   "Unleash twice."]
                         :effects [[:give-choice {:title   :scorn
                                                  :choice  ::scorn-choice
                                                  :options [:special
                                                            {:option :damage :text "Gravehold suffers 3 damage"}
                                                            {:option :unleash :text "Unleash twice"}]
                                                  :min     1
                                                  :max     1}]]}
            :quote      "'Part of me almost admires the craftmanship of this horrible thing.' Adelheim, Breach Mage Weaponsmith"})

(defn unrelenting-ire-unleash [game _]
  (let [discarded-nemesis-cards (->> (get-in game [:turn-order :discard])
                                     (filter (comp #{:nemesis} :type))
                                     count)]
    (cond-> game
            (= 2 discarded-nemesis-cards) (push-effect-stack {:effects [[:unleash]
                                                                        [:unleash]]}))))

(effects/register {::unrelenting-ire-unleash unrelenting-ire-unleash})

(def unrelenting-ire {:name    :unrelenting-ire
                      :type    :attack
                      :tier    1
                      :text    ["Unleash."
                                "If there are two nemesis turn order cards in the turn order discard pile, Unleash two additional times."]
                      :effects [[:unleash]
                                [::unrelenting-ire-unleash]]
                      :quote   "'I reckon its head would look good on a post.' Reeve, Breach Mage Elite"})

(def convoke {:name    :convoke
              :type    :strike
              :text    "Any player suffers 4 damage."
              :effects [[:give-choice {:title   :convoke
                                       :text    "Any player suffers 4 damage."
                                       :choice  [:damage-player {:arg 4}]
                                       :options [:players]
                                       :min     1
                                       :max     1}]]
              :quote   "'No other thing, in our world or theirs, has been so violent as this beast.' Brama, Breach Mage Elder"})

(def devastate {:name    :devastate
                :type    :strike
                :text    "Gravehold suffers 5 damage."
                :effects [[:damage-gravehold 5]]
                :quote   "'Anger is born in fire.' Z'hana, Breach Mage Renegade"})

(def eviscerate {:name    :eviscerate
                 :type    :strike
                 :text    "The player with the lowest life suffers 2 damage."
                 :effects [[:give-choice {:title   :eviscerate
                                          :text    "The player with the lowest life suffers 2 damage."
                                          :choice  [:damage-player {:arg 2}]
                                          :options [:players {:least-life true}]
                                          :min     1
                                          :max     1}]]
                 :quote   "'The breach commander Thraxir was split in twain with a single blow, his armor twisted and glowing like a brand. I have never seen such a thing before or since.' Mist, Breach Mage Dagger Captain"})

(def frenzy {:name    :frenzy
             :type    :strike
             :text    "The player with the most opened breaches suffers 3 damage."
             :effects [[:give-choice {:title   :frenzy
                                      :text    "The player with the most opened breaches suffers 3 damage."
                                      :choice  [:damage-player {:arg 3}]
                                      :options [:players {:most-opened-breaches true}]
                                      :min     1
                                      :max     1}]]
             :quote   "'All great armies have a warrior that is feared above all others. We call theirs Rageborne.' Mist, Breach Mage Dagger Captain"})

(def raze {:name    :raze
           :type    :strike
           :text    ["Gravehold suffers 3 damage."
                     "Any player suffers 1 damage."]
           :effects [[:damage-gravehold 3]
                     [:give-choice {:title   :raze
                                    :text    "Any player suffers 1 damage."
                                    :choice  [:damage-player {:arg 1}]
                                    :options [:players]
                                    :min     1
                                    :max     1}]]
           :quote   "'The fire spread from the old barracks to the south wall in a moment, the ancient stone hissing and turning black as the beast strode through the city.' Nerva, Survivor"})

(defn seize-choice [{:keys [players] :as game} _]
  (let [sorted-spells        (->> players
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
        [_ cost-2 cost-3] (map :cost sorted-spells)
        auto-discard-cards   (cond
                               (<= (count sorted-spells) 2) sorted-spells
                               (not= cost-2 cost-3) (take 2 sorted-spells)
                               :else (->> sorted-spells
                                          (filter (comp #(> % cost-2) :cost))))
        manual-discard-count (- (min 2 (count sorted-spells))
                                (count auto-discard-cards))]
    (push-effect-stack game {:effects (concat
                                        (when (not-empty auto-discard-cards)
                                          [[:discard-prepped-spells {:spells auto-discard-cards}]])
                                        (when (pos? manual-discard-count)
                                          [[:give-choice {:title   :seize
                                                          :text    (str "The players collectively discard the "
                                                                        (when (> manual-discard-count 1)
                                                                          (ut/number->text manual-discard-count))
                                                                        " most expensive prepped spell"
                                                                        (when (> manual-discard-count 1)
                                                                          "s")
                                                                        ".")
                                                          :choice  [:discard-prepped-spells]
                                                          :options [:players :prepped-spells {:min-cost cost-2}]
                                                          :min     manual-discard-count
                                                          :max     manual-discard-count}]]))})))

(effects/register {::seize-choice seize-choice})

(def seize {:name    :seize
            :type    :strike
            :text    "The players collectively discard the two most expensive prepped spells."
            :effects [[::seize-choice]]
            :quote   "'How this cave has not long ago collapsed is a wonder and a mercy.' Nerva, Survivor"})

(defn additional-rules [{:keys [difficulty]}]
  ["When Rageborne Strikes, resolve the following in order:"
   "- Draw a card from the strike deck and resolve it."
   "- Shuffle that card back into the strike deck."
   (if (#{:beginner :normal} difficulty)
     "- Rageborne loses three Fury tokens"
     "- Rageborne loses one Fury token")
   "At the end of the nemesis turn, if Rageborne has four or more Fury tokens, it Strikes once."])

(effects/register-predicates {::additional-rules additional-rules})

(def rageborne {:name             :rageborne
                :level            2
                :life             70
                :unleash          [[::gain-fury]]
                :unleash-text     "Rageborne gains one Fury token."
                :additional-rules ::additional-rules
                :after-effects    [[::after-effects]]
                :cards            [cleave provoker unrelenting-ire
                                   blood-cry invoke-carnage scorn
                                   avatar-of-wrath onslaught rolling-death]
                :fury             1
                :strike-deck      (->> [convoke
                                        devastate
                                        eviscerate
                                        frenzy
                                        raze
                                        seize]
                                       shuffle)})
