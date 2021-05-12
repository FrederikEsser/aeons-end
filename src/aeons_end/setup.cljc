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

(def buried-light {:name    :buried-light
                   :type    :spell
                   :cost    0
                   :text    "Cast: Deal 1 damage. Gain 1 Aether."
                   :effects [[:deal-damage 1]
                             [:gain-aether 1]]})

(def brama {:name     :brama
            :title    "Breach Mage Elder"
            :breaches [{}
                       {:stage 1}
                       {:stage 0}
                       {:stage 2}]
            :hand     [buried-light crystal crystal crystal crystal]
            :deck     [crystal crystal crystal spark spark]
            :ability  {:name       :brink-siphon
                       :activation :your-main-phase
                       :cost       5
                       :text       "Any player gains 4 life."
                       :effects    []}})

(def garnet-shard {:name    :garnet-shard
                   :type    :gem
                   :cost    0
                   :text    "Gain 1 Aether. OR Cast any player's prepped spell."
                   :effects []})

(def mist {:name     :mist
           :title    "Dagger Captain"
           :breaches [{}
                      {:stage 2}
                      {:stage 1}
                      {:stage 1}]
           :hand     [garnet-shard crystal crystal crystal spark]
           :deck     [crystal crystal crystal spark spark]
           :ability  {:name       :divine-augury
                      :activation :your-main-phase
                      :cost       5
                      :text       "Any ally draws four cards."
                      :effects    []}})

(defn create-player [{:keys [breaches] :as mage}]
  (merge mage
         {:breaches (map merge
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
          :charges  0}))
