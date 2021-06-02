(ns aeons-end.cards.spell
  (:require [aeons-end.nemesis]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(defn amplify-vision-damage [game {:keys [player-no] :as args}]
  (let [all-breaches-opened? (->> (get-in game [:players player-no :breaches])
                                  (remove (comp #{:destroyed} :status))
                                  (every? (comp #{:opened} :status)))]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (if all-breaches-opened? 3 2)]]})))

(effects/register {::amplify-vision-damage amplify-vision-damage})

(def amplify-vision {:name    :amplify-vision
                     :type    :spell
                     :cost    4
                     :text    "Cast: Focus your closed breach with the lowest focus cost.\nDeal 2 damage. If all of your breaches are opened, deal 1 additional damage."
                     :effects [[:focus-lowest-cost-breach]
                               [::amplify-vision-damage]]})

(defn dark-fire-discard [game {:keys [player-no card-name card-names] :as args}]
  (let [card-count (cond card-name 1
                         card-names (count card-names)
                         :else 0)]
    (push-effect-stack game (merge {:player-no player-no
                                    :args      args         ; bonus-damage
                                    :effects   [[:discard-from-hand args]
                                                [:deal-damage (* 3 card-count)]]}))))

(effects/register {::dark-fire-discard dark-fire-discard})

(def dark-fire {:name    :dark-fire
                :type    :spell
                :cost    5
                :text    "Cast: Discard up to two cards in hand.\nDeal 3 damage for each card discarded this way."
                :effects [[:give-choice {:title   :dark-fire
                                         :text    "Discard up to two cards in hand"
                                         :choice  ::dark-fire-discard
                                         :options [:player :hand]
                                         :max     2}]]})

(def ignite {:name    :ignite
             :type    :spell
             :cost    4
             :text    "Cast: Deal 2 damage.\nAny ally gains 1 charge."
             :effects [[:deal-damage 2]
                       [:give-choice {:title   :ignite
                                      :text    "Any ally gains 1 charge"
                                      :choice  :gain-charge
                                      :options [:players {:ally true}]
                                      :min     1
                                      :max     1}]]})

(defn phoenix-flame-damage [game {:keys [player-no] :as args}]
  (push-effect-stack game {:player-no player-no
                           :args      args                  ; bonus-damage
                           :effects   [[:spend-charges 1]
                                       [:deal-damage 4]]}))

(effects/register {::phoenix-flame-damage phoenix-flame-damage})

(def phoenix-flame {:name    :phoenix-flame
                    :type    :spell
                    :cost    3
                    :text    "Cast: Deal 2 damage.\nYou may lose 1 charge to deal 2 additional damage."
                    :effects [[:give-choice {:title     :phoenix-flame
                                             :text      "You may lose 1 charge to deal 4 damage."
                                             :choice    ::phoenix-flame-damage
                                             :or-choice {:text    "Deal 2 damage"
                                                         :effects [[:deal-damage 2]]}
                                             :options   [:player :charges]
                                             :max       1}]]})

(defn planar-insight-damage [game {:keys [player-no] :as args}]
  (let [opened-breaches (->> (get-in game [:players player-no :breaches])
                             (filter (comp #{:opened} :status))
                             count)]
    (push-effect-stack game {:player-no player-no
                             :args      args                ; bonus-damage
                             :effects   [[:deal-damage (+ 2 opened-breaches)]]})))

(effects/register {::planar-insight-damage planar-insight-damage})

(def planar-insight {:name    :planar-insight
                     :type    :spell
                     :cost    6
                     :text    "Cast: Deal 2 damage\nDeal 1 additional damage for each of your opened breaches."
                     :effects [[::planar-insight-damage]]})

(def radiance {:name    :radiance
               :type    :spell
               :cost    8
               :text    "Cast: Deal 5 damage.\nEach ally draws a card."
               :effects [[:deal-damage 5]
                         [:other-players {:effects [[:draw 1]]}]]})

(def cards [amplify-vision
            dark-fire
            ignite
            phoenix-flame
            planar-insight
            radiance])
