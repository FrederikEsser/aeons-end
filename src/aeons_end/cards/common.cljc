(ns aeons-end.cards.common
  (:require [aeons-end.operations :refer []]
            [aeons-end.utils :as ut]
            [aeons-end.effects :as effects]))

(defn gain-aether [game {:keys [player-no arg]}]
  (cond-> game
          (pos? arg) (update-in [:players player-no :current :aether] ut/plus arg)))

(effects/register {:gain-aether gain-aether})

(defn deal-damage [game {:keys [arg]}]
  (-> game
      (update-in [:nemesis :life] - arg)))

(effects/register {:deal-damage deal-damage})
