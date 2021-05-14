(ns aeons-end.setup)

(def spark {:name    :spark
            :type    :spell
            :cost    0
            :text    "Cast: Deal 1 damage."
            :effects [[:deal-damage 1]]})

(def crystal {:name    :crystal
              :type    :gem
              :cost    0
              :text    "Gain 1 Aether."
              :effects [[:gain-aether 1]]})

(defn create-player [{:keys [breaches] :as mage}]
  (merge mage
         {:breaches (mapv merge
                          [{:status :opened}
                           {:status     :closed
                            :focus-cost 2
                            :open-costs [5 4 3 2]}
                           {:status       :closed
                            :focus-cost   3
                            :open-costs   [9 7 5 3]
                            :bonus-damage 1}
                           {:status       :closed
                            :focus-cost   4
                            :open-costs   [13 10 7 4]
                            :bonus-damage 1}]
                          breaches)
          :life     10
          :charges  0
          :phase    :out-of-turn}))
