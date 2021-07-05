(ns aeons-end.cards.relic
  (:require [aeons-end.cards.common]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn astral-cube-heal [game _]
  (let [{:keys [player-no]} (-> game :turn-order :deck first :type)]
    (cond
      (= -1 player-no) (push-effect-stack game {:effects [[:give-choice {:title   :astral-cube
                                                                         :text    "Any player gains 1 life."
                                                                         :choice  [:heal {:life 1}]
                                                                         :options [:players {:not-exhausted true}]
                                                                         :min     1
                                                                         :max     1}]]})
      player-no (push-effect-stack game {:player-no player-no
                                         :effects   [[:heal {:life 1}]]})
      :else game)))

(effects/register {::astral-cube-heal astral-cube-heal})

(def astral-cube {:name    :astral-cube
                  :type    :relic
                  :cost    5
                  :text    ["Return a gem you played this turn to your hand."
                            "Reveal the top card of the turn order deck. If you revealed a player's turn order card, that player gains 1 life."]
                  :effects [[:give-choice {:title   :astral-cube
                                           :text    "Return a gem you played this turn to your hand."
                                           :choice  [:move-card {:from :play-area
                                                                 :to   :hand}]
                                           :options [:player :play-area {:type :gem}]
                                           :min     1
                                           :max     1}]
                            [:reveal-top-turn-order]
                            [::astral-cube-heal]]
                  :quote   "'Riddles within riddles within shapes within black. There is a power here the other refuse to taste.' Xaxos, Voidbringer"})

(def blasting-staff {:name    :blasting-staff
                     :type    :relic
                     :cost    4
                     :text    "You may cast a prepped spell that you prepped this turn. If you do, that spell deals 2 additional damage."
                     :effects [[:give-choice {:title   :blasting-staff
                                              :text    "You may cast a prepped spell that you prepped this turn. If you do, that spell deals 2 additional damage."
                                              :choice  [:cast-spell {:additional-damage 2}]
                                              :options [:player :prepped-spells {:prepped-this-turn true}]
                                              :max     1}]]
                     :quote   "'While appearing to be crudely fashioned, these staffs contain within them the very essence of the void.'"})

(def cairn-compass {:name    :cairn-compass
                    :type    :relic
                    :cost    4
                    :text    "Any ally may prep a spell in their discard pile to their opened or closed breach(es)."
                    :effects [[:give-choice {:title   :cairn-compass
                                             :text    "Any ally may prep a spell in their discard pile to their opened or closed breach(es)."
                                             :choice  [:prep-from-discard {:closed-breaches? true}]
                                             :options [:players :discard {:ally         true
                                                                          :type         :spell
                                                                          :empty-breach true}]
                                             :max     1}]]
                    :quote   "'Being lost down there once was more than enough.' Indira, Breach Apprentice"})

(defn fiend-catcher-move-nemesis-card [game {:keys [card-name]}]
  (cond-> game
          (= :nemesis card-name) (push-effect-stack {:effects [[:put-turn-order-top-to-bottom]]})))

(defn fiend-catcher-choice [game {:keys [player-no]}]
  (let [{:keys [type]} (-> game :turn-order :deck first)]
    (cond-> game
            (= :nemesis type) (push-effect-stack {:player-no player-no
                                                  :effects   [[:give-choice {:title   :fiend-catcher
                                                                             :text    "You may place the nemesis turn order card on the bottom of the turn order deck."
                                                                             :choice  ::fiend-catcher-move-nemesis-card
                                                                             :options [:turn-order :revealed]
                                                                             :max     1}]]}))))

(effects/register {::fiend-catcher-move-nemesis-card fiend-catcher-move-nemesis-card
                   ::fiend-catcher-choice            fiend-catcher-choice})

(def fiend-catcher {:name    :fiend-catcher
                    :type    :relic
                    :cost    3
                    :text    ["You may destroy a card in your hand or discard pile."
                              "Reveal the top card of the turn order deck. If you revealed a nemesis turn order card, you may place that card on the bottom of the turn order deck."]
                    :effects [[:give-choice {:title   :fiend-catcher
                                             :text    "You may destroy a card in your hand or discard pile."
                                             :choice  :destroy-from-area
                                             :options [:mixed
                                                       [:player :hand]
                                                       [:player :discard]]
                                             :max     1}]
                              [:reveal-top-turn-order]
                              [::fiend-catcher-choice]]
                    :quote   "'It's as good a place as any for a world-swallowing beast.' Garu, Oathsworn Protector"})

(defn focusing-orb-destroy [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:move-card {:card-name :focusing-orb
                                                    :from      :play-area
                                                    :to        :trash}]
                                       [:heal-gravehold 3]]}))

(effects/register {::focusing-orb-destroy focusing-orb-destroy})

(def focusing-orb {:name    :focusing-orb
                   :type    :relic
                   :cost    4
                   :text    ["Focus any player's breach."
                             "OR"
                             "Destroy this. Gravehold gains 3 life."]
                   :effects [[:give-choice {:title     :focusing-orb
                                            :text      "Focus any player's breach."
                                            :choice    :focus-breach
                                            :options   [:players :breaches {:opened false}]
                                            :or-choice {:text    "Destroy this. Gravehold gains 3 life."
                                                        :effects [[::focusing-orb-destroy]]}
                                            :max       1}]]
                   :quote   "'Jian picked up the orb as if knowing its purpose. That was the last day she ever spoke.'"})

(def mages-talisman {:name    :mage's-talisman
                     :type    :relic
                     :cost    5
                     :text    ["Gain 1 charge"
                               "Any ally gains 1 charge."]
                     :effects [[:gain-charge]
                               [:give-choice {:title   :mage's-talisman
                                              :text    "Any ally gains 1 charge"
                                              :choice  :gain-charge
                                              :options [:players :ability {:ally true :fully-charged false}]
                                              :min     1
                                              :max     1}]]
                     :quote   "'The sister-mages of old would wear their talismans as a symbol of their station and a warning to those that would challenge them.'"})

(defn mages-totem-destroy [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (concat [[:move-card {:card-name card-name
                                                            :from      :play-area
                                                            :to        :trash}]]
                                              (when (= :mage's-totem card-name)
                                                [[:heal-gravehold 1]]))}))

(effects/register {::mages-totem-destroy mages-totem-destroy})

(def mages-totem {:name    :mage's-totem
                  :type    :relic
                  :cost    2
                  :text    ["Destroy a gem or relic you played this turn."
                            "OR"
                            "Destroy this. Gravehold gains 1 life."]
                  :effects [[:give-choice {:title   :mage's-totem
                                           :text    ["Destroy a gem or relic you played this turn."
                                                     "OR"
                                                     "Destroy this. Gravehold gains 1 life."]
                                           :choice  ::mages-totem-destroy
                                           :options [:player :play-area {:types #{:gem :relic}}]
                                           :min     1
                                           :max     1}]]
                  :quote   "'Once, the conclave worshipped at the foot of a great tower much like this very effigy.' Yan Magda, Enlightened Exile"})

(defn temporal-helix-choice [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :temporal-helix
                                                      :text    "Cast any player's prepped spell without discarding it."
                                                      :choice  [:spell-effect {:caster player-no}]
                                                      :options [:players :prepped-spells]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::temporal-helix-choice temporal-helix-choice})

(def temporal-helix {:name    :temporal-helix
                     :type    :relic
                     :cost    7
                     :text    "Cast any player's prepped spell without discarding it."
                     :effects [[::temporal-helix-choice]]
                     :quote   "'Breach mages are never truly in possession of their own minds.' Brama, Breach Mage Elder"})

(defn vortex-gauntlet-cast [game {:keys [player-no breach-no card-name] :as args}]
  (let [{{:keys [id]} :card} (ut/get-card-idx game [:players player-no :breaches breach-no :prepped-spells] {:name card-name})]
    (cond-> game
            card-name (push-effect-stack {:player-no player-no
                                          :effects   [[:cast-spell args]
                                                      [:move-card {:move-card-id id
                                                                   :from         :discard
                                                                   :to           :hand}]]}))))

(defn vortex-gauntlet-choice [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :vortex-gauntlet
                                                      :text    "Cast any player's prepped spell. Return that spell to that player's hand."
                                                      :choice  [::vortex-gauntlet-cast {:caster player-no}]
                                                      :options [:players :prepped-spells]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::vortex-gauntlet-cast   vortex-gauntlet-cast
                   ::vortex-gauntlet-choice vortex-gauntlet-choice})

(def vortex-gauntlet {:name    :vortex-gauntlet
                      :type    :relic
                      :cost    6
                      :text    "Cast any player's prepped spell. Return that spell to that player's hand."
                      :effects [[::vortex-gauntlet-choice]]
                      :quote   "'One does not wield this glove. The glove wields the bearer.' Gex, Breach Mage Advisor"})

(defn unstable-prism-play-gem [game {:keys [player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:play-twice {:card-name card-name}]
                                       [:move-card {:card-name card-name
                                                    :from      :play-area
                                                    :to        :trash}]]}))

(effects/register {::unstable-prism-play-gem unstable-prism-play-gem})

(def unstable-prism {:name    :unstable-prism
                     :type    :relic
                     :cost    3
                     :text    ["Play a gem in hand twice and destroy it."
                               "OR"
                               "Gain 2 Aether."]
                     :effects [[:give-choice {:title     :unstable-prism
                                              :text      "Play a gem in hand twice and destroy it."
                                              :choice    ::unstable-prism-play-gem
                                              :or-choice {:text    "Gain 2 Aether"
                                                          :effects [[:gain-aether 2]]}
                                              :options   [:player :hand {:type :gem}]
                                              :max       1}]]
                     :quote   "'Not every pretty rock is a means to wage war. Some must be cut and carved into more useful things.' Adelheim, Breach Mage Weaponsmith"})

(def cards [astral-cube
            blasting-staff
            cairn-compass
            fiend-catcher
            focusing-orb
            mages-talisman
            mages-totem
            temporal-helix
            vortex-gauntlet
            unstable-prism])
