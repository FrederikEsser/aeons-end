(ns aeons-end.cards.spells
  (:require [aeons-end.nemeses :refer [deal-damage]]
            [aeons-end.operations :refer [push-effect-stack focus-breach]]
            [aeons-end.effects :as effects]))

(defn amplify-vision-focus [game {:keys [player-no]}]
  (let [breach-no (->> (get-in game [:players player-no :breaches])
                       (keep-indexed (fn [idx {:keys [status focus-cost]}]
                                       (when (not= :opened status)
                                         {:breach-no  idx
                                          :focus-cost focus-cost})))
                       (sort-by :focus-cost)
                       (map :breach-no)
                       first)]
    (cond-> game
            breach-no (focus-breach {:player-no player-no
                                     :breach-no breach-no}))))

(defn amplify-vision-damage [game {:keys [player-no]}]
  (let [all-breaches-opened? (->> (get-in game [:players player-no :breaches])
                                  (every? (comp #{:opened} :status)))]
    (deal-damage game {:player-no player-no
                       :arg       (if all-breaches-opened? 3 2)})))

(effects/register {::amplify-vision-focus  amplify-vision-focus
                   ::amplify-vision-damage amplify-vision-damage})

(def amplify-vision {:name    :amplify-vision
                     :type    :spell
                     :cost    4
                     :text    "Cast: Focus your closed breach with the lowest focus cost.\nDeal 2 damage. If all of your breaches are opened, deal 1 additional damage."
                     :effects [[::amplify-vision-focus]
                               [::amplify-vision-damage]]})

(defn dark-fire-discard [game {:keys [player-no card-name card-names] :as args}]
  (let [card-count (cond card-name 1
                         card-names (count card-names)
                         :else 0)]
    (push-effect-stack game {:player-no player-no
                             :effects   [[:discard-from-hand args]
                                         [:deal-damage (* 3 card-count)]]})))

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

(def radiance {:name    :radiance
               :type    :spell
               :cost    8
               :text    "Cast: Deal 5 damage.\nEach ally draws a card."
               :effects [[:deal-damage 5]
                         [:other-players {:effects [[:draw 1]]}]]})