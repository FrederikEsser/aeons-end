(ns aeons-end.cards.attack
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(defn afflict-damage-player [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:give-choice {:title   :afflict
                                                      :text    "You may place a card in your discard pile into your hand."
                                                      :choice  :take-from-discard
                                                      :options [:player :discard]
                                                      :max     1}]]}))

(effects/register {::afflict-damage-player afflict-damage-player})

(def afflict {:name    :afflict
              :type    :attack
              :tier    1
              :text    ["Unleash."
                        "Any player suffers 3 damage and may place a card in their discard pile into their hand."]
              :effects [[:unleash]
                        [:give-choice {:title   :afflict
                                       :text    "Any player suffers 3 damage and may place a card in their discard pile into their hand."
                                       :choice  ::afflict-damage-player
                                       :options [:players]
                                       :min     1
                                       :max     1}]]
              :quote   "'Such wisdom comes at a conciderable price.' Xaxos, Voidbringer"})

(def assail {:name    :assail
             :type    :attack
             :tier    2
             :text    ["Unleash twice."
                       "The player with the most expensive prepped spell places that spell on the top of their deck."]
             :effects [[:unleash]
                       [:unleash]
                       [:give-choice {:title   :assail
                                      :text    "The player with the most expensive prepped spell places that spell on the top of their deck."
                                      :choice  :topdeck-prepped-spell
                                      :options [:players :prepped-spells {:most-expensive true}]
                                      :min     1
                                      :max     1}]]
             :quote   "'For as massive as they are, they are quick.' Sparrow, Breach Mage Soldier"})

(def awaken {:name    :awaken
             :type    :attack
             :tier    2
             :text    ["Place the most recently discarded minion in the nemesis discard pile back into play."
                       "OR"
                       "Unleash twice and Gravehold suffers 3 damage."]
             :effects [[:give-choice {:title     :awaken
                                      :text      "Place the most recently discarded minion in the nemesis discard pile back into play."
                                      :choice    :revive-minion
                                      :or-choice {:text    "Unleash twice and Gravehold suffers 3 damage."
                                                  :effects [[:unleash]
                                                            [:unleash]
                                                            [:damage-gravehold 3]]}
                                      :options   [:nemesis :discard {:type :minion :most-recent true}]
                                      :max       1}]]
             :quote   "'They were always just outside the edge of our knowing, sitting there in some quiet abyss just waiting to visit ruin upon us.' Z'hana, Breach Mage Renegade"})

(defn banish-damage [{:keys [players] :as game} _]
  (let [most-prepped-spells (->> players
                                 (map ut/count-prepped-spells)
                                 (apply max))]
    (cond-> game
            (pos? most-prepped-spells) (push-effect-stack {:effects [[:give-choice {:title   :banish
                                                                                    :text    "The player with the most prepped spells suffers 1 damage for each of their prepped spells."
                                                                                    :choice  [:damage-player {:arg most-prepped-spells}]
                                                                                    :options [:players {:min-number-of-prepped-spells most-prepped-spells}]
                                                                                    :min     1
                                                                                    :max     1}]]}))))

(effects/register {::banish-damage banish-damage})

(def banish {:name    :banish
             :type    :attack
             :tier    3
             :text    ["Unleash twice."
                       "The player with the most prepped spells suffers 1 damage for each of their prepped spells."]
             :effects [[:unleash]
                       [:unleash]
                       [::banish-damage]]
             :quote   "'Get down! It's ejecting back at us!' Ohat, Dirt Merchant"})

(def dispel {:name    :dispel
             :type    :attack
             :tier    2
             :text    ["Unleash twice."
                       "The player with the most prepped spells discards their most expensive prepped spell."]
             :effects [[:unleash]
                       [:unleash]
                       [:give-choice {:title   :dispel
                                      :text    "The player with the most prepped spells discards their most expensive prepped spell."
                                      :choice  :discard-prepped-spells
                                      :options [:players :prepped-spells {:most-prepped-spells true
                                                                          :most-expensive      :per-player}]
                                      :min     1
                                      :max     1}]]
             :quote   "'The words and gestures needed left him, and the breach grew dim.' Nerva, Survivor"})

(defn encroach-damage [game _]
  (let [{:keys [player-no player-nos] :as type} (-> game :turn-order :deck first :type)]
    (cond
      (= :wild type) (push-effect-stack game {:effects [[:give-choice {:title   :encroach
                                                                       :text    "Any player suffers 2 damage."
                                                                       :choice  [:damage-player {:arg 2}]
                                                                       :options [:players]
                                                                       :min     1
                                                                       :max     1}]]})
      player-no (push-effect-stack game {:player-no player-no
                                         :effects   [[:damage-player 2]]})
      player-nos (push-effect-stack game {:effects [[:give-choice {:title   :encroach
                                                                   :text    (str "Player "
                                                                                 (case player-nos
                                                                                   #{0 1} "1 or 2"
                                                                                   #{2 3} "3 or 4")
                                                                                 " suffers 2 damage.")
                                                                   :choice  [:damage-player {:arg 2}]
                                                                   :options [:players {:player-nos player-nos}]
                                                                   :min     1
                                                                   :max     1}]]})
      :else (push-effect-stack game {:effects [[:damage-gravehold 3]]}))))

(effects/register {::encroach-damage encroach-damage})

(def encroach {:name    :encroach
               :type    :attack
               :tier    1
               :text    ["Unleash."
                         "Reveal the top card of the turn order deck. If a player turn order card was revealed, that player suffers 2 damage. Otherwise Gravehold suffers 3 damage."]
               :effects [[:unleash]
                         [:reveal-top-turn-order]
                         [::encroach-damage]]})

(defn engulf-attack [game _]
  (let [{:keys [id name effects] :as card} (->> (get-in game [:nemesis :discard])
                                                (filter (comp #{:attack} :type))
                                                last)]
    (cond-> game
            card (push-effect-stack {:effects (concat [[:set-resolving {:card-name name}]
                                                       [:move-card {:move-card-id id
                                                                    :from         :discard
                                                                    :to           :discard}]]
                                                      effects)}))))

(effects/register {::engulf-attack engulf-attack})

(def engulf {:name    :engulf
             :type    :attack
             :tier    3
             :text    "Resolve the most recently discarded attack card in the nemesis discard pile."
             :effects [[::engulf-attack]]
             :quote   "'The Nameless rarely attack in the same fashion twice. It would seem today is an exception.' Dezmodia, Voidborn Prodigy"})

(defn gathering-darkness-destroy [game {:keys [player-no]}]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   [[:shuffle-discard-into-deck]
                                      [:reveal-from-deck 4]
                                      [:give-choice {:title   :gathering-darkness
                                                     :text    "Destroy the top four cards of your deck."
                                                     :choice  :trash-from-revealed
                                                     :options [:player :revealed]
                                                     :min     4
                                                     :max     4}]]})))

(defn gathering-darkness-choice [{:keys [players] :as game} _]
  (let [max-deck+discard (->> players
                              (map ut/count-cards-in-deck-and-discard)
                              (apply max))]
    (push-effect-stack game {:effects [[:give-choice {:title   :gathering-darkness
                                                      :text    "Any player places their discard pile on top of their deck, shuffles it, and then destroys the top four cards of their deck."
                                                      :choice  ::gathering-darkness-destroy
                                                      :options [:players {:min-deck+discard (min 4 max-deck+discard)}]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::gathering-darkness-destroy gathering-darkness-destroy
                   ::gathering-darkness-choice  gathering-darkness-choice})

(def gathering-darkness {:name    :gathering-darkness
                         :type    :attack
                         :tier    3
                         :text    ["Any player places their discard pile on top of their deck, shuffles it, and then destroys the top four cards of their deck."
                                   "Then, Unleash twice."]
                         :effects [[::gathering-darkness-choice]
                                   [:unleash]
                                   [:unleash]]
                         :quote   "'The Nameless do not die, for they themselves are death.' Xaxos, Breach Mage Adept"})

(def lay-waste {:name    :lay-waste
                :type    :attack
                :tier    2
                :text    ["Unleash twice."
                          "Any player suffers 2 damage."]
                :effects [[:unleash]
                          [:unleash]
                          [:give-choice {:title   :lay-waste
                                         :text    "Any player suffers 2 damage."
                                         :choice  [:damage-player {:arg 2}]
                                         :options [:players]
                                         :min     1
                                         :max     1}]]
                :quote   "'Gravehold sat silent and forgotten for a thousand ages before The Nameless came. No one remembers for whom this vast city was built. Those stories lay in the ash outside the cave with the bones of those who once told them.'"})

(def mutilate {:name    :mutilate
               :type    :attack
               :tier    2
               :text    ["Unleash."
                         "The players collectively discard two prepped spells. Any player suffers 2 damage."]
               :effects [[:unleash]
                         [:give-choice {:title   :mutilate
                                        :text    "The players collectively discard two prepped spells."
                                        :choice  :collective-discard-prepped-spells
                                        :options [:players :prepped-spells]
                                        :min     2
                                        :max     2}]
                         [:give-choice {:title   :mutilate
                                        :text    "Any player suffers 2 damage."
                                        :choice  [:damage-player {:arg 2}]
                                        :options [:players]
                                        :min     1
                                        :max     1}]]
               :quote   "'They say that before The Nameless found Gravehold, nothing grew in these caves. It appears the cave burgeons through our blood.' Gex, Breach Mage Advisor"})

(defn nix-damage-player [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 1]
                                       [:give-choice {:title   :nix
                                                      :text    "Discard the most expensive card in your hand."
                                                      :choice  :discard-from-hand
                                                      :options [:player :hand {:most-expensive true}]
                                                      :min     1
                                                      :max     1}]]}))

(defn nix-choice [{:keys [players] :as game} _]
  (let [largest-hand (->> players
                          (map (comp count :hand))
                          (apply max 0))]
    (push-effect-stack game {:effects [[:give-choice {:title   :nix
                                                      :text    "Any player suffers 1 damage and discards their most expensive card in hand."
                                                      :choice  ::nix-damage-player
                                                      :options [:players {:min-hand (min 1 largest-hand)}]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::nix-damage-player nix-damage-player
                   ::nix-choice        nix-choice})

(def nix {:name    :nix
          :type    :attack
          :tier    1
          :text    ["Unleash."
                    "Any player suffers 1 damage and discards their most expensive card in hand."]
          :effects [[:unleash]
                    [::nix-choice]]
          :quote   "'It's as if the world itself is screaming.' Nerva, Survivor"})

(defn obliterate-player [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:give-choice {:title     :obliterate
                                                      :text      "Destroy four cards in hand."
                                                      :choice    :destroy-from-hand
                                                      :or-choice {:text    "Suffer 4 damage"
                                                                  :effects [[:damage-player 4]]}
                                                      :options   [:player :hand]
                                                      :min       4
                                                      :max       4
                                                      :optional? true}]]}))

(effects/register {::obliterate-player obliterate-player})

(def obliterate {:name    :obliterate
                 :type    :attack
                 :tier    3
                 :text    ["Unleash twice."
                           "The player with the most opened breaches destroys four cards in hand or suffers 4 damage."]
                 :effects [[:unleash]
                           [:unleash]
                           [:give-choice {:title   :obliterate
                                          :text    "The player with the most opened breaches destroys four cards in hand or suffers 4 damage."
                                          :choice  ::obliterate-player
                                          :options [:players {:most-opened-breaches true}]
                                          :min     1
                                          :max     1}]]
                 :quote   "'While the breaches amplify our magic a thousandfold, it comes at the highest ransom imaginable' Brama, Breach Mage Elder"})

(defn quell-choice [game {:keys [choice]}]
  (push-effect-stack game {:effects (case choice
                                      :damage [[:damage-gravehold 7]]
                                      :unleash [[:unleash]
                                                [:unleash]
                                                [:unleash]])}))

(effects/register {::quell-choice quell-choice})

(def quell {:name    :quell
            :type    :attack
            :tier    3
            :text    ["Gravehold suffers 7 damage."
                      "OR"
                      "Unleash three times."]
            :effects [[:give-choice {:title   :quell
                                     :choice  ::quell-choice
                                     :options [:special
                                               {:option :damage :text "Gravehold suffers 7 damage."}
                                               {:option :unleash :text "Unleash three times."}]
                                     :min     1
                                     :max     1}]]
            :quote   "'The Nameless hunger for the same thing we do; an end to this war.' Garu, Oathsworn Protector"})

(defn skewer-damage [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:draw 1]]}))

(effects/register {::skewer-damage skewer-damage})

(def skewer {:name    :skewer
             :type    :attack
             :tier    1
             :text    ["Unleash."
                       "Any player suffers 3 damage and draws a card."]
             :effects [[:unleash]
                       [:give-choice {:title   :skewer
                                      :text    "Any player suffers 3 damage and draws a card."
                                      :choice  ::skewer-damage
                                      :options [:players]
                                      :min     1
                                      :max     1}]]
             :quote   "'It takes years to train a breach mage properly, but only a second to snuff one out.' Malastar, Breach Mage Mentor"})

(def slaughter {:name    :slaughter
                :type    :attack
                :tier    1
                :text    ["Unleash."
                          "Gravehold suffers 3 damage."]
                :effects [[:unleash]
                          [:damage-gravehold 3]]
                :quote   "'We have nothing left to lose but our lives' Mist, Breach Mage Dagger Captain"})

(def smite {:name    :smite
            :type    :attack
            :tier    2
            :text    ["Unleash twice."
                      "Gravehold suffers 2 damage."]
            :effects [[:unleash]
                      [:unleash]
                      [:damage-gravehold 2]]
            :quote   "'One side of this struggle must fall for the other to truly live.' Xaxos, Voidbringer"})

(defn sunder-choice [game {:keys [area player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :players [[:damage-player 4]
                                                  [:damage-gravehold 4]]
                                        :discard (->> (get-in game [:turn-order :discard])
                                                      (filter (comp #{:nemesis} :type))
                                                      (map (fn [{:keys [name]}]
                                                             [:shuffle-into-turn-order-deck {:card-name name}]))))}))

(effects/register {::sunder-choice sunder-choice})

(def sunder {:name    :sunder
             :type    :attack
             :tier    3
             :text    ["The player with the lowest life suffers 4 damage and"
                       "Gravehold suffers 4 damage."
                       "OR"
                       "Shuffle all of the nemesis turn order cards into the turn order deck."]
             :effects [[:give-choice {:title   :sunder
                                      :text    ["The player with the lowest life suffers 4 damage and"
                                                "Gravehold suffers 4 damage."
                                                "OR"
                                                "Shuffle all of the nemesis turn order cards into the turn order deck."]
                                      :choice  ::sunder-choice
                                      :options [:mixed
                                                [:players {:lowest-life true}]
                                                [:turn-order :discard {:type :nemesis}]]
                                      :min     1
                                      :max     1}]]
             :quote   "'We fight for the lingering hope of returning to The World That Was, but in our hearts we know our future lies within these caves.' Z'hana, Breach Mage Renegade"})

(def thrash {:name    :thrash
             :type    :attack
             :tier    1
             :text    ["Unleash."
                       "The players collectively discard two cards in hand."]
             :effects [[:unleash]
                       [:give-choice {:title   :thrash
                                      :text    "The players collectively discard two cards in hand."
                                      :choice  :collective-discard-from-hand
                                      :options [:players :hand]
                                      :min     2
                                      :max     2}]]
             :quote   "'The creature thrashed as it burned, its massive body crashing into the South Wall like a hot blade through cloth.' Garu, Oathsworn Keeper"})

(defn throttle-destroy-cards [game {:keys [player-no]}]
  (let [sorted-hand          (->> (get-in game [:players player-no :hand])
                                  (sort-by :cost ut/gt))
        [_ _ cost-3 cost-4] (map :cost sorted-hand)
        auto-destroy-cards   (cond
                               (<= (count sorted-hand) 3) sorted-hand
                               (not= cost-3 cost-4) (take 3 sorted-hand)
                               :else (->> sorted-hand
                                          (filter (comp #(ut/gt % cost-3) :cost))))
        manual-destroy-count (- (min 3 (count sorted-hand))
                                (count auto-destroy-cards))]
    (push-effect-stack game {:player-no player-no
                             :effects   (concat
                                          (when (not-empty auto-destroy-cards)
                                            [[:move-cards {:card-names (map :name auto-destroy-cards)
                                                           :from       :hand
                                                           :to         :trash}]])
                                          (when (pos? manual-destroy-count)
                                            [[:give-choice {:title   :throttle
                                                            :text    (str "Destroy the "
                                                                          (when (> manual-destroy-count 1)
                                                                            (ut/number->text manual-destroy-count))
                                                                          " most expensive card"
                                                                          (when (> manual-destroy-count 1)
                                                                            "s")
                                                                          " in your hand.")
                                                            :choice  [:move-cards {:from :hand
                                                                                   :to   :trash}]
                                                            :options [:player :hand {:min-cost cost-3}]
                                                            :min     manual-destroy-count
                                                            :max     manual-destroy-count}]]))})))

(defn throttle-choice [{:keys [players] :as game} _]
  (let [largest-hand (->> players
                          (map (comp count :hand))
                          (apply max 0))]
    (push-effect-stack game {:effects [[:give-choice {:title   :throttle
                                                      :text    "Any player destroys their three most expensive cards in hand."
                                                      :choice  ::throttle-destroy-cards
                                                      :options [:players {:min-hand (min 3 largest-hand)}]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::throttle-destroy-cards throttle-destroy-cards
                   ::throttle-choice        throttle-choice})

(def throttle {:name    :throttle
               :type    :attack
               :tier    3
               :text    ["Unleash twice."
                         "Any player destroys their three most expensive cards in hand."]
               :effects [[:unleash]
                         [:unleash]
                         [::throttle-choice]]
               :quote   "'Were I made of muscle and blood like the others, the impact would have surely ended me.' Remnant, Aetherial Entity"})

(def topple {:name    :topple
             :type    :attack
             :tier    3
             :text    ["Unleash twice."
                       "Gravehold suffers 4 damage."]
             :effects [[:unleash]
                       [:unleash]
                       [:damage-gravehold 4]]
             :quote   "'LOOK OU--' Last words of Quilion Rafe, Far Hollow watchman"})

(defn generic [tier & [idx]]
  (let [name (str (case tier
                    1 "Weak"
                    2 "Medium"
                    3 "Strong")
                  " Attack"
                  (when idx
                    (str " (" idx ")")))]
    (merge
      {:name name
       :type :attack
       :tier tier}
      (case tier
        1 {:text    ["Unleash."
                     "Gravehold suffers 3 damage."]
           :effects [[:unleash]
                     [:damage-gravehold 3]]}
        2 {:text    ["Unleash twice."
                     "Any player suffers 2 damage."]
           :effects [[:unleash]
                     [:unleash]
                     [:give-choice {:title   name
                                    :text    "Any player suffers 2 damage."
                                    :choice  [:damage-player {:arg 2}]
                                    :options [:players]
                                    :min     1
                                    :max     1}]]}
        3 {:text    ["Unleash twice."
                     "Gravehold suffers 4 damage."]
           :effects [[:unleash]
                     [:unleash]
                     [:damage-gravehold 4]]}))))
