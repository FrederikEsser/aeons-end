(ns aeons-end.cards.nemesis
  (:require [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]))

(def unleash-1 {:name        :unleash-1
                :text        "Unleash."
                :type        :attack
                :immediately [[:unleash]]
                :tier        1})

(def unleash-2 {:name        :unleash-2
                :text        "Unleash twice."
                :type        :attack
                :immediately [[:unleash]
                              [:unleash]]
                :tier        2})

(def unleash-3 {:name        :unleash-3
                :text        "Unleash three times."
                :type        :attack
                :immediately [[:unleash]
                              [:unleash]
                              [:unleash]]
                :tier        3})

(defn afflict-damage-player [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 3]
                                       [:give-choice {:title   :afflict
                                                      :text    "You may place a card in your discard pile into your hand."
                                                      :choice  :take-from-discard
                                                      :options [:player :discard]
                                                      :max     1}]]}))

(effects/register {::afflict-damage-player afflict-damage-player})

(def afflict {:name        :afflict
              :type        :attack
              :tier        1
              :text        "Unleash. Any player suffers 3 damage and may place a card in their discard pile into their hand."
              :immediately [[:unleash]
                            [:give-choice {:title   :afflict
                                           :text    "Any player suffers 3 damage and may place a card in their discard pile into their hand."
                                           :choice  ::afflict-damage-player
                                           :options [:players]
                                           :min     1
                                           :max     1}]]
              :quote       "'Such wisdom comes at a conciderable price.' Xaxos, Voidbringer"})

(defn nix-damage-player [game {:keys [player-no]}]
  (push-effect-stack game {:player-no player-no
                           :effects   [[:damage-player 1]
                                       [:give-choice {:title   :nix
                                                      :text    "Discards the most expensive card in your hand."
                                                      :choice  :discard-from-hand
                                                      :options [:player :hand {:most-expensive true}]
                                                      :min     1
                                                      :max     1}]]}))

(effects/register {::nix-damage-player nix-damage-player})

(def nix {:name        :nix
          :type        :attack
          :tier        1
          :text        "Unleash. Any player suffers 1 damage and discards their most expensive card in hand."
          :immediately [[:unleash]
                        [:give-choice {:title   :afflict
                                       :text    "Any player suffers 1 damage and discards their most expensive card in hand."
                                       :choice  ::nix-damage-player
                                       :options [:players]
                                       :min     1
                                       :max     1}]]
          :quote       "'It's as if the world itself is screaming.' Nerva, Survivor"})

(def cards (concat [afflict
                    nix]
                   (mapcat vector
                           (repeat unleash-1)
                           (repeat unleash-2)
                           (repeat unleash-3))))
