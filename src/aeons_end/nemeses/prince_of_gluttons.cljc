(ns aeons-end.nemeses.prince-of-gluttons
  (:require [aeons-end.operations :refer [push-effect-stack move-card add-card]]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]
            [aeons-end.cards.attack]
            [aeons-end.cards.power :as power]))

(defn devour [game {:keys [number-of-cards card-name card-names]
                    :or   {number-of-cards 1}}]
  (if card-names
    (push-effect-stack game {:effects (->> card-names
                                           (map (fn [card-name]
                                                  [::devour {:card-name card-name}])))})
    (let [{:keys [pile-size] :or {pile-size 0}} (ut/get-pile-idx game card-name)]
      (push-effect-stack game {:effects (concat (repeat (min number-of-cards pile-size)
                                                        [:move-card {:card-name card-name
                                                                     :from      :supply
                                                                     :to        :devoured}])
                                                (when (> number-of-cards pile-size)
                                                  [[:damage-gravehold (* 2 (- number-of-cards pile-size))]]))}))))

(effects/register {::devour devour})

(defn do-unleash [{:keys [players] :as game} args]
  (let [title          (keyword (or (:resolving args)
                                    (:resolving game))
                                "unleash")
        cards-devoured (if (<= (count players) 2) 3 2)]
    (push-effect-stack game {:effects [[:give-choice {:title      title
                                                      :text       (str "Devour " (ut/number->text cards-devoured) " cards from the least expensive supply pile.")
                                                      :choice     [::devour {:number-of-cards cards-devoured}]
                                                      :options    [:supply {:least-expensive true
                                                                            :devoured        false}]
                                                      :min        1
                                                      :max        1
                                                      :mandatory? true}]]})))

(effects/register {::unleash do-unleash})

(defn setup [{:keys [supply difficulty] :as game} _]
  (let [gems (->> supply
                  (map :card)
                  (filter (comp #{:gem} :type))
                  (group-by :cost)
                  (sort-by first >)
                  vals
                  (map (partial map :name)))]
    (push-effect-stack game {:effects (concat (->> gems
                                                   (mapv (fn [card-names]
                                                           [:give-choice {:title   :setup
                                                                          :text    "Place one gem from each gem supply, starting with the most expensive, on top of the devoured pile."
                                                                          :choice  ::devour
                                                                          :options [:supply {:names (set card-names)}]
                                                                          :min     (count card-names)
                                                                          :max     (count card-names)}])))
                                              (when (#{:expert :extinction} difficulty)
                                                (->> (range 1 10)
                                                     reverse
                                                     (map (fn [n]
                                                            [:give-choice {:title   :expert-setup
                                                                           :text    (if (= 1 n)
                                                                                      "Place one additional card from any of the supply piles on top of the devoured pile."
                                                                                      (str "Place an additional "
                                                                                           (ut/number->text n)
                                                                                           " cards from any of the supply piles on top of the devoured pile."))
                                                                           :choice  ::devour
                                                                           :options [:supply {:devoured false}]
                                                                           :min     1
                                                                           :max     1}])))))})))

(effects/register {::setup setup})

(defn victory-condition [{:keys [supply] :as game}]
  (when (->> supply (every? (comp zero? :pile-size)))
    {:conclusion :defeat
     :text       "All supply piles are empty. All memories of the history of Gravehold have been erased."}))

(effects/register-predicates {::victory-condition victory-condition})

(defn unleash-text [{:keys [players]}]
  (let [cards-devoured (if (<= (count players) 2) 3 2)]
    (str "Devour " (ut/number->text cards-devoured) " cards from the least expensive supply pile.")))

(effects/register-predicates {::unleash-text unleash-text})

(defn cerephage-devour [game _]
  (let [{{:keys [life]} :card} (ut/get-card-idx game [:nemesis :play-area] {:name :cerephage})]
    (push-effect-stack game {:effects [[:give-choice {:title      :cerephage
                                                      :text       (str "Devour " (ut/number->text life) " cards from one supply pile.")
                                                      :choice     [::devour {:number-of-cards life}]
                                                      :options    [:supply {:devoured false}]
                                                      :min        1
                                                      :max        1
                                                      :mandatory? true}]]})))

(effects/register {::cerephage-devour cerephage-devour})

(def cerephage {:name       :cerephage
                :type       :minion
                :tier       2
                :life       7
                :persistent {:text    ["Devour cards equal to this minion's life from one supply pile."
                                       "Then, this minion suffers 2 damage."]
                             :effects [[::cerephage-devour]
                                       [:deal-damage-to-minion {:card-name :cerephage
                                                                :damage    2}]]}
                :quote      "'Quickly. Before our heads are hollow!' Adelheim, Breach Mage Weaponsmith"})

(defn digest-give-choice [{:keys [supply] :as game} _]
  (let [empty-supply-piles (->> supply
                                (filter (comp zero? :pile-size))
                                count)]
    (push-effect-stack game {:effects [[:give-choice {:title     :digest
                                                      :text      (str "Any player suffers " empty-supply-piles " damage.")
                                                      :choice    [:damage-player {:arg empty-supply-piles}]
                                                      :or-choice {:text    "Unleash three times."
                                                                  :effects [[:unleash]
                                                                            [:unleash]
                                                                            [:unleash]]}
                                                      :options   [:players]
                                                      :max       1}]]})))

(effects/register {::digest-give-choice digest-give-choice})

(def digest {:name    :digest
             :type    :attack
             :tier    3
             :text    ["Unleash three times."
                       "OR"
                       "Any player suffers damage equal to the number of empty supply supply piles."]
             :effects [[::digest-give-choice]]})

(def godfeeders {:name       :godfeeders
                 :type       :minion
                 :tier       2
                 :life       8
                 :persistent {:text    ["Gravehold suffers 3 damage."
                                        "Devour a gem from the most expensive gem supply pile."]
                              :effects [[:damage-gravehold 3]
                                        [:give-choice {:title   :godfeeders
                                                       :text    "Devour a gem from the most expensive gem supply pile."
                                                       :choice  ::devour
                                                       :options [:supply {:type           :gem
                                                                          :most-expensive true
                                                                          :devoured       false}]
                                                       :min     1
                                                       :max     1}]]}
                 :quote      "'The Prince exploits these creatures to both mend its wounds and sate its hunger.'"})

(defn gorge-devour [{:keys [supply] :as game} _]
  (let [cost (->> supply
                  (filter (comp pos? :pile-size))
                  (map :card)
                  (filter (comp #{:gem} :type))
                  (map :cost)
                  sort
                  drop-last
                  last)]
    (push-effect-stack game {:effects [[:give-choice {:title      :gorge
                                                      :text       "Devour two cards from the second most expensive gem supply pile."
                                                      :choice     [::devour {:number-of-cards 2}]
                                                      :options    [:supply {:cost     (or cost :no-cost)
                                                                            :devoured false}]
                                                      :min        1
                                                      :max        1
                                                      :mandatory? true}]]})))

(effects/register {::gorge-devour gorge-devour})

(def gorge {:name    :gorge
            :type    :attack
            :tier    1
            :text    ["Devour two cards from the second most expensive gem supply pile."
                      "Any player suffers 2 damage."]
            :effects [[::gorge-devour]
                      [:give-choice {:title   :gorge
                                     :text    "Any player suffers 2 damage."
                                     :choice  [:damage-player {:arg 2}]
                                     :options [:players]
                                     :min     1
                                     :max     1}]]
            :quote   "'So much of our history was lost in the first Purge. Aeons of knowledge, art, and culture erased. It has been so very long that we know not even the true name of The World That Was.' Brama, Breach Mage Elder"})

(defn lobotomize-choice [game {:keys [area player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :players [[:damage-player 3]]
                                        :supply [[::devour {:number-of-cards 3
                                                            :card-name       card-name}]])}))

(effects/register {::lobotomize-choice lobotomize-choice})

(def lobotomize {:name    :lobotomize
                 :type    :attack
                 :tier    1
                 :text    ["Any player suffers 3 damage."
                           "OR"
                           "Devour three spells from any spell supply pile."]
                 :effects [[:give-choice {:title   :lobotomize
                                          :text    ["Any player suffers 3 damage."
                                                    "OR"
                                                    "Devour three spells from any spell supply pile."]
                                          :choice  ::lobotomize-choice
                                          :options [:mixed
                                                    [:players]
                                                    [:supply {:type     :spell
                                                              :devoured false}]]
                                          :min     1
                                          :max     1}]]
                 :quote   "'I saw the woman's eyes empty and everything she was fell away into nothing.' Nerva, Survivor"})

(defn memory-eater-choice [{:keys [supply] :as game} _]
  (let [empty-supply-piles (->> supply
                                (filter (comp zero? :pile-size))
                                count)]
    (push-effect-stack game {:effects [[:give-choice {:title   :digest
                                                      :text    (str "The player with the lowest life suffers " empty-supply-piles " damage.")
                                                      :choice  [:damage-player {:arg empty-supply-piles}]
                                                      :options [:players {:lowest-life true}]
                                                      :min     1
                                                      :max     1}]]})))

(effects/register {::memory-eater-choice memory-eater-choice})

(def memory-eater {:name       :memory-eater
                   :type       :minion
                   :tier       3
                   :life       16
                   :persistent {:text    "The player with the lowest life suffers damage equal to the number of empty supply piles."
                                :effects [[::memory-eater-choice]]}
                   :quote      "'Sometimes I think it a blessing to have forgotten so much of my past.' Phaedraxa, Breach Mage Seer"})

(defn mindguzzler-set-life [{:keys [supply] :as game} _]
  (let [empty-supply-piles (->> supply
                                (filter (comp zero? :pile-size))
                                count)]
    (ut/update-in-vec game [:nemesis :play-area] {:name :mindguzzler} assoc :life (+ 8 empty-supply-piles))))

(effects/register {::mindguzzler-set-life mindguzzler-set-life})

(def mindguzzler {:name        :mindguzzler
                  :type        :minion
                  :tier        2
                  :immediately {:text    ["Set this minion's life equal to 8 plus the number of empty supply piles."]
                                :effects [[::mindguzzler-set-life]]}
                  :persistent  {:text    "Unleash."
                                :effects [[:unleash]]}})

(defn thought-biter-choice [game {:keys [area player-no card-name]}]
  (push-effect-stack game {:player-no player-no
                           :effects   (case area
                                        :players [[:damage-player 2]]
                                        :supply [[::devour {:number-of-cards 2
                                                            :card-name       card-name}]])}))

(effects/register {::thought-biter-choice thought-biter-choice})

(def thought-biter {:name       :thought-biter
                    :type       :minion
                    :tier       1
                    :life       6
                    :persistent {:text    ["Any player suffers 2 damage."
                                           "OR"
                                           "Devour two relics from the least expensive relic supply pile."]
                                 :effects [[:give-choice {:title   :thought-biter
                                                          :text    ["Any player suffers 2 damage."
                                                                    "OR"
                                                                    "Devour two relics from the least expensive relic supply pile."]
                                                          :choice  ::thought-biter-choice
                                                          :options [:mixed
                                                                    [:players]
                                                                    [:supply {:type            :relic
                                                                              :least-expensive true
                                                                              :devoured        false}]]
                                                          :min     1
                                                          :max     1}]]}
                    :quote      "'Is it eating the cave wall?' Nym, Breach Mage Apprentice"})

(defn vile-feast-damage [{:keys [supply] :as game} _]
  (let [empty-supply-piles (->> supply
                                (filter (comp zero? :pile-size))
                                count)]
    (push-effect-stack game {:effects [[:damage-gravehold (* 2 empty-supply-piles)]]})))

(effects/register {::vile-feast-damage vile-feast-damage})

(def vile-feast {:name       :vile-feast
                 :type       :power
                 :tier       3
                 :to-discard {:text      "Spend 8 Aether."
                              :predicate [::power/can-afford? {:amount 8}]
                              :effects   [[:pay {:amount 8
                                                 :type   :discard-power-card}]]}
                 :power      {:power   2
                              :text    ["Gravehold suffers 2 damage for each empty supply pile."]
                              :effects [[::vile-feast-damage]]}
                 :quote      "'It aims to make us forget what is at stake.' Malastar, Breach Mage Mentor"})

(def prince-of-gluttons {:name              :prince-of-gluttons
                         :level             5
                         :life              70
                         :setup             [[::setup]]
                         :unleash           [[::unleash]]
                         :unleash-text      ::unleash-text
                         :additional-rules  ["- When a card is Devoured, place it on top of the devoured pile."
                                             "- Cards may be gained from the top of the devoured pile as if it were a supply pile."
                                             "- The players may look through the devoured pile at any time."
                                             "- When Prince of Gluttons would Devour a card from a supply pile that is empty Gravehold suffers 2 damage per card instead."
                                             "- If all supply piles are empty, the players lose."]
                         :victory-condition ::victory-condition
                         :cards             [gorge lobotomize thought-biter
                                             cerephage godfeeders mindguzzler
                                             digest vile-feast memory-eater]})
