(ns aeons-end.cards.relic
  (:require [aeons-end.cards.common]
            [aeons-end.operations :refer [push-effect-stack]]
            [aeons-end.effects :as effects]
            [aeons-end.utils :as ut]))

(def cairn-compass {:name    :cairn-compass
                    :type    :relic
                    :cost    4
                    :text    "Any ally may prep a spell in their discard pile to their opened or closed breach(es)."
                    :effects [[:give-choice {:title   :cairn-compass
                                             :text    "Any ally may prep a spell in their discard pile to their opened or closed breach(es)."
                                             :choice  [:prep-from-discard {:closed-breaches? true}]
                                             :options [:players :discard {:ally               true
                                                                          :type               :spell
                                                                          :empty-breach-stati #{:opened :closed}}]
                                             :max     1}]]})

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
                     :effects [[::temporal-helix-choice]]})

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
                      :effects [[::vortex-gauntlet-choice]]})

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
                     :text    "Play a gem in hand twice and destroy it.\nOR\nGain 2 Aether."
                     :effects [[:give-choice {:title     :unstable-prism
                                              :text      "Play a gem in hand twice and destroy it."
                                              :choice    ::unstable-prism-play-gem
                                              :or-choice {:text    "Gain 2 Aether"
                                                          :effects [[:gain-aether 2]]}
                                              :options   [:player :hand {:type :gem}]
                                              :max       1}]]})

(def cards [cairn-compass
            temporal-helix
            vortex-gauntlet
            unstable-prism])
