(ns aeons-end.cards.relic
  (:require [aeons-end.cards.common]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn astral-cube-heal [game _]
  (let [{:keys [player-no player-nos] :as type} (-> game :turn-order :deck first :type)]
    (cond
      (= :wild type) (push-effect-stack game {:effects [[:give-choice {:title   :astral-cube
                                                                       :text    "Any player gains 1 life."
                                                                       :choice  [:heal {:life 1}]
                                                                       :options [:players {:can-heal true}]
                                                                       :min     1
                                                                       :max     1}]]})
      player-no (push-effect-stack game {:player-no player-no
                                         :effects   [[:heal {:life 1}]]})
      player-nos (push-effect-stack game {:effects [[:give-choice {:title   :astral-cube
                                                                   :text    (str "Player "
                                                                                 (case player-nos
                                                                                   #{0 1} "1 or 2"
                                                                                   #{2 3} "3 or 4")
                                                                                 " gains 1 life.")
                                                                   :choice  [:heal {:life 1}]
                                                                   :options [:players {:player-nos player-nos}]
                                                                   :min     1
                                                                   :max     1}]]})
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

(def bottled-vortex {:name    :bottled-vortex
                     :type    :relic
                     :cost    3
                     :text    ["Destroy this."
                               "Destroy up to two cards in your hand or discard pile."
                               "Draw a card."]
                     :effects [[:destroy-this]
                               [:give-choice {:title   :bottled-vortex
                                              :text    "Destroy up to two cards in your hand or discard pile."
                                              :choice  :destroy-from-area
                                              :options [:mixed
                                                        [:player :hand]
                                                        [:player :discard]]
                                              :max     2}]
                               [:draw 1]]
                     :quote   "'There are worlds within worlds, infinite realms cascading from the yawning nothing.' Phaedraxa, Breach Mage Seer"})

(def cairn-compass {:name    :cairn-compass
                    :type    :relic
                    :cost    4
                    :text    "Any ally may prep a spell in their discard pile to their opened or closed breach(es)."
                    :effects [[:give-choice {:title   :cairn-compass
                                             :text    "Any ally may prep a spell in their discard pile to their opened or closed breach(es)."
                                             :choice  [:prep-from-discard {:closed-breaches? true}]
                                             :options [:players :discard {:ally     true
                                                                          :type     :spell
                                                                          :can-prep {:closed-breaches? true}}]
                                             :max     1}]]
                    :quote   "'Being lost down there once was more than enough.' Indira, Breach Apprentice"})

(defn conclave-destroy [game {:keys [player-no]}]
  (let [{:keys [pile-size]} (ut/get-pile-idx game :conclave-scroll)]
    (cond-> game
            (zero? pile-size) (push-effect-stack {:player-no player-no
                                                  :effects   [[:give-choice {:title   :conclave-scroll
                                                                             :text    "You may destroy the top card of any ally's discard pile."
                                                                             :choice  [:destroy-from-discard {:destroyed-by player-no}]
                                                                             :options [:players :discard {:ally true :last true}]
                                                                             :max     1}]]}))))

(effects/register {::conclave-destroy conclave-destroy})

(def conclave-scroll {:name    :conclave-scroll
                      :type    :relic
                      :cost    3
                      :text    ["Gain 1 charge."
                                "If this card's supply pile is empty, you may destroy the top card of any ally's discard pile."]
                      :effects [[:gain-charge]
                                [::conclave-destroy]]
                      :quote   "'Brama and her ilk read the words and consider them answers. I see only questions.' Yan Magda, Enlightened Exile"})

(defn fiend-catcher-choice [game {:keys [player-no]}]
  (let [{:keys [type]} (-> game :turn-order :deck first)]
    (cond-> game
            (= :nemesis type) (push-effect-stack {:player-no player-no
                                                  :effects   [[:give-choice {:title   :fiend-catcher
                                                                             :text    "You may place the nemesis turn order card on the bottom of the turn order deck."
                                                                             :choice  :put-turn-order-top-to-bottom
                                                                             :options [:turn-order :revealed]
                                                                             :max     1}]]}))))

(effects/register {::fiend-catcher-choice fiend-catcher-choice})

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

(defn flexing-dagger-destroy [game {:keys [player-no card-name]}]
  (cond-> game
          card-name (push-effect-stack {:player-no player-no
                                        :effects   [[:move-card {:card-name card-name
                                                                 :from      :play-area
                                                                 :to        :trash}]
                                                    [:deal-damage 1]]})))

(defn flexing-dagger-cost-reduction [game {:keys [player-no]}]
  (update-in game [:players player-no :breach-cost-reduction] ut/plus 3))

(effects/register {::flexing-dagger-destroy        flexing-dagger-destroy
                   ::flexing-dagger-cost-reduction flexing-dagger-cost-reduction})

(def flexing-dagger {:name    :flexing-dagger
                     :type    :relic
                     :cost    2
                     :text    ["The next time you focus or open a breach this turn, it costs 3 Aether less."
                               "OR"
                               "Destroy this. Deal 1 damage."]
                     :effects [[:give-choice {:title     :flexing-dagger
                                              :text      "Destroy this. Deal 1 damage."
                                              :choice    ::flexing-dagger-destroy
                                              :options   [:player :play-area {:this true}]
                                              :or-choice {:text    "The next time you focus or open a breach this turn, it costs 3 Aether less."
                                                          :effects [[::flexing-dagger-cost-reduction]]}
                                              :max       1}]]
                     :quote   "'Forged from unstable void ore, the blade shifts with the will of its wielder.' Adelheim, Breach Mage Weaponsmith"})

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

(defn molten-hammer-give-choice [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title   :molten-hammer
                                                      :text    "You may destroy a card in hand or on top of any player's discard pile."
                                                      :choice  [:destroy-from-area {:destroyed-by player-no}]
                                                      :options [:mixed
                                                                [:player :hand]
                                                                [:players :discard {:last true}]]
                                                      :max     1}]]}))

(effects/register {::molten-hammer-give-choice molten-hammer-give-choice})

(def molten-hammer {:name    :molten-hammer
                    :type    :relic
                    :cost    5
                    :text    ["Gain 1 charge."
                              "You may destroy a card in hand or on top of any player's discard pile."]
                    :effects [[:gain-charge]
                              [::molten-hammer-give-choice]]
                    :quote   "'Some tools are meant for making, while others hold a more pernicious task.' Mist, Dagger Captain"})

(defn primordial-fetish-destroy [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:move-card {:card-name :primordial-fetish
                                                    :from      :play-area
                                                    :to        :trash}]
                                       [:heal {:life 3}]]}))

(effects/register {::primordial-fetish-destroy primordial-fetish-destroy})

(def primordial-fetish {:name    :primordial-fetish
                        :type    :relic
                        :cost    4
                        :text    ["Focus any player's breach."
                                  "OR"
                                  "Destroy this. Gain 3 life."]
                        :effects [[:give-choice {:title     :primordial-fetish
                                                 :text      "Focus any player's breach."
                                                 :choice    :focus-breach
                                                 :options   [:players :breaches {:opened false}]
                                                 :or-choice {:text    "Destroy this. Gain 3 life."
                                                             :effects [[::primordial-fetish-destroy]]}
                                                 :max       1}]]
                        :quote   "'The child rose from the dust and opened its eyes. The Conclave revere this child, for she was the first of their kind.' Mazahaedron, Henge Mystic"})

(defn riddlesphere-choice [game {:keys [player-no choice]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case choice
                                        :charge [[:gain-charge]]
                                        :aether [[:spend-charges 2]
                                                 [:gain-aether 5]])}))

(defn riddlesphere-give-choice [game {:keys [player-no]}]
  (let [{:keys [charges]} (get-in game [:players player-no :ability])]
    (push-effect-stack game {:player-no player-no
                             :effects   (if (>= charges 2)
                                          [[:give-choice {:title   :riddlesphere
                                                          :choice  ::riddlesphere-choice
                                                          :options [:special
                                                                    {:option :charge :text "Gain 1 charge."}
                                                                    {:option :aether :text "You may lose 2 charges. If you do, gain 5 Aether."}]
                                                          :min     1
                                                          :max     1}]]
                                          [[:gain-charge]])})))

(effects/register {::riddlesphere-choice      riddlesphere-choice
                   ::riddlesphere-give-choice riddlesphere-give-choice})

(def riddlesphere {:name    :riddlesphere
                   :type    :relic
                   :cost    3
                   :text    ["Gain 1 charge."
                             "OR"
                             "You may lose 2 charges. If you do, gain 5 Aether."]
                   :effects [[::riddlesphere-give-choice]]
                   :quote   "'Behold, the beginning and the end.' Dezmodia, Voidborn Prodigy"})

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
            bottled-vortex
            cairn-compass
            conclave-scroll
            fiend-catcher
            flexing-dagger
            focusing-orb
            mages-talisman
            mages-totem
            molten-hammer
            primordial-fetish
            riddlesphere
            temporal-helix
            vortex-gauntlet
            unstable-prism])
