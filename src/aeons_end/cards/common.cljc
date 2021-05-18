(ns aeons-end.cards.common
  (:require [aeons-end.operations :refer []]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]))

(defn gain-aether [{:keys [current-player] :as game} {:keys [player-no arg]}]
  (let [current-player? (or (nil? current-player)
                            (= current-player player-no))]
    (cond-> game
            (and (pos? arg)
                 current-player?) (update-in [:players player-no :aether] ut/plus arg))))

(effects/register {:gain-aether gain-aether})

(defn deal-damage [game {:keys [arg]}]
  (-> game
      (update-in [:nemesis :life] - arg)))

(effects/register {:deal-damage deal-damage})

(defn heal [game {:keys [player-no life]}]
  (let [amount (min life
                    (- 10 (get-in game [:players player-no :life])))]
    (-> game
       (update-in [:players player-no :life] ut/plus amount))))

(effects/register {:heal heal})
