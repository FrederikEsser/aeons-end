(ns aeons-end.spell-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.spell :refer :all]
            [aeons-end.cards.relic :refer [cairn-compass]]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.mages :refer [garnet-shard]]))

(deftest amplify-vision-test
  (testing "Amplify Vision"
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      0}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status     :focused
                                   :focus-cost 2
                                   :stage      1}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status     :focused
                                       :focus-cost 2
                                       :stage      3}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status :opened}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      3}
                                      {:status :opened}
                                      {:status     :closed
                                       :focus-cost 4
                                       :stage      3}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status :opened}
                                  {:status :opened}
                                  {:status     :closed
                                   :focus-cost 4
                                   :stage      3}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status :opened}
                                      {:status :opened}
                                      {:status     :closed
                                       :focus-cost 4
                                       :stage      3}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status :opened}
                                  {:status :opened}
                                  {:status :opened}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status :opened}
                                      {:status :opened}
                                      {:status :destroyed}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status :opened}
                                  {:status :opened}
                                  {:status :destroyed}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [amplify-vision]}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      0}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}
                                  {:status     :focused
                                   :focus-cost 2
                                   :stage      1}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [amplify-vision]}
                                      {:status     :focused
                                       :focus-cost 2
                                       :stage      3}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}
                                  {:status :opened}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 46}}))))

(deftest dark-fire-test
  (testing "Dark Fire"
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [crystal crystal spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose nil))
           {:players [{:breaches [{}]
                       :hand     [crystal crystal spark]
                       :discard  [dark-fire]}]
            :nemesis {:life 50}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose :spark))
           {:players [{:breaches [{}]
                       :discard  [dark-fire spark]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [crystal crystal spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose [:crystal :crystal]))
           {:players [{:breaches [{}]
                       :hand     [spark]
                       :discard  [dark-fire crystal crystal]}]
            :nemesis {:life 44}}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                                          :hand     [crystal crystal spark]}]
                               :nemesis {:life 50}}
                              (cast-spell 0 0 :dark-fire)
                              (choose [:crystal :crystal :spark]))))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire))
           {:players [{:breaches [{}]
                       :discard  [dark-fire]}]
            :nemesis {:life 50}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [dark-fire]}]
                           :hand     [crystal]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose [:crystal]))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :discard  [dark-fire crystal]}]
            :nemesis {:life 46}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [dark-fire]}]
                           :hand     [crystal]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose nil))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :hand     [crystal]
                       :discard  [dark-fire]}]
            :nemesis {:life 49}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [dark-fire]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :discard  [dark-fire]}]
            :nemesis {:life 49}}))))

(deftest ignite-test
  (testing "Ignite"
    (is (= (-> {:players [{:breaches [{:prepped-spells [ignite]}]}
                          {:ability {:charges     0
                                     :charge-cost 4}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :ignite)
               (choose {:player-no 1}))
           {:players [{:breaches [{}]
                       :discard  [ignite]}
                      {:ability {:charges     1
                                 :charge-cost 4}}]
            :nemesis {:life 48}}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:players [{:breaches [{:prepped-spells [ignite]}]}
                                         {:ability {:charges     0
                                                    :charge-cost 4}}]
                               :nemesis {:life 50}}
                              (cast-spell 0 0 :ignite)
                              (choose {:player-no 0}))))
    (is (= (-> {:players [{:hand [garnet-shard]}
                          {:breaches [{:prepped-spells [ignite]}]
                           :ability  {:charges     0
                                      :charge-cost 4}}]
                :nemesis {:life 50}}
               (play 0 :garnet-shard)
               (choose {:player-no 1
                        :breach-no 0
                        :card-name :ignite})
               (choose {:player-no 1}))
           {:players [{:play-area [garnet-shard]}
                      {:breaches [{}]
                       :ability  {:charges     1
                                  :charge-cost 4}
                       :discard  [ignite]}]
            :nemesis {:life 48}}))))

(deftest nova-forge-test
  (testing "Nova Forge"
    (testing "While prepped"
      (let [nova-forge (assoc nova-forge :id 1)]
        (is (= (-> {:players [{:breaches [{:status         :opened
                                           :prepped-spells [nova-forge]}]
                               :phase    :casting}]}
                   (set-phase 0 :main))
               {:players [{:breaches         [{:status         :opened
                                               :prepped-spells [nova-forge]}]
                           :earmarked-aether {#{:spell} 2}
                           :phase            :main}]}))
        (is (= (-> {:players [{:hand     [nova-forge]
                               :breaches [{:status :opened}]
                               :phase    :main}]}
                   (prep-spell 0 0 :nova-forge))
               {:players [{:breaches         [{:status         :opened
                                               :prepped-spells [nova-forge]}]
                           :earmarked-aether {#{:spell} 2}
                           :phase            :main}]}))
        (is (= (-> {:players [{:hand     [nova-forge]
                               :breaches [{:status :opened}]
                               :phase    :casting}]}
                   (prep-spell 0 0 :nova-forge))
               {:players [{:breaches         [{:status         :opened
                                               :prepped-spells [nova-forge]}]
                           :earmarked-aether {#{:spell} 2}
                           :phase            :main}]}))
        (let [cairn-compass (assoc cairn-compass :id 2)]
          (is (= (-> {:players [{:hand [cairn-compass]}
                                {:discard  [nova-forge]
                                 :breaches [{:status :opened}]
                                 :phase    :out-of-turn}]}
                     (play 0 :cairn-compass)
                     (choose {:player-no 1 :card-name :nova-forge :card-id 1}))
                 {:players [{:play-area [cairn-compass]}
                            {:breaches [{:status         :opened
                                         :prepped-spells [nova-forge]}]
                             :phase    :out-of-turn}]}))
          (is (= (-> {:players [{:hand     [cairn-compass]
                                 :discard  [nova-forge]
                                 :breaches [{:status :opened}]
                                 :phase    :main}]}
                     (play 0 :cairn-compass)
                     (choose {:player-no 0 :card-name :nova-forge :card-id 1}))
                 {:players [{:play-area        [cairn-compass]
                             :breaches         [{:status         :opened
                                                 :prepped-spells [nova-forge]}]
                             :earmarked-aether {#{:spell} 2}
                             :phase            :main}]})))))))

(deftest phoenix-flame-test
  (testing "Phoenix Flame"
    (is (= (-> {:players [{:breaches [{:prepped-spells [phoenix-flame]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area :nemesis}))
           {:players [{:breaches [{}]
                       :discard  [phoenix-flame]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [phoenix-flame]}]
                           :ability  {:charges 1}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area :charges}))
           {:players [{:breaches [{}]
                       :ability  {:charges 0}
                       :discard  [phoenix-flame]}]
            :nemesis {:life 46}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [phoenix-flame]}]
                           :ability  {:charges 1}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area :nemesis}))
           {:players [{:breaches [{}]
                       :ability  {:charges 1}
                       :discard  [phoenix-flame]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [phoenix-flame]}]
                           :ability  {:charges 2}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area :charges}))
           {:players [{:breaches [{}]
                       :ability  {:charges 1}
                       :discard  [phoenix-flame]}]
            :nemesis {:life 46}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [phoenix-flame]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area :nemesis}))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :discard  [phoenix-flame]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [phoenix-flame]}]
                           :ability  {:charges 1}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area :charges}))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1
                                   }]
                       :ability  {:charges 0}
                       :discard  [phoenix-flame]}]
            :nemesis {:life 45}}))))

(deftest radiance-test
  (testing "Radiance"
    (is (= (-> {:players [{:breaches [{:prepped-spells [radiance]}]
                           :deck     [crystal]}
                          {:deck [crystal crystal]}
                          {:deck [crystal]}
                          {}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :radiance))
           {:players [{:breaches [{}]
                       :deck     [crystal]
                       :discard  [radiance]}
                      {:deck [crystal]
                       :hand [crystal]}
                      {:hand [crystal]}
                      {}]
            :nemesis {:life 45}}))))

(deftest planar-insight-test
  (testing "Planar Insight"
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [planar-insight]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :planar-insight))
           {:players [{:breaches [{:status :opened}]
                       :discard  [planar-insight]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :closed
                                       :prepped-spells [planar-insight]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :planar-insight))
           {:players [{:breaches [{:status :closed}]
                       :discard  [planar-insight]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:status :opened}
                                      {:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [planar-insight]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 1 :planar-insight))
           {:players [{:breaches [{:status :opened}
                                  {:status       :opened
                                   :bonus-damage 1}]
                       :discard  [planar-insight]}]
            :nemesis {:life 45}}))))
