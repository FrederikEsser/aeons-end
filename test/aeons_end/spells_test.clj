(ns aeons-end.spells-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.base :refer [crystal spark]]
            [aeons-end.cards.spells :refer :all]
            [aeons-end.mages :refer [garnet-shard]]))

(deftest dark-fire-test
  (testing "Dark Fire"
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [crystal crystal spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 :dark-fire 0)
               (choose nil))
           {:players [{:breaches [{}]
                       :hand     [crystal crystal spark]
                       :discard  [dark-fire]}]
            :nemesis {:life 50}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 :dark-fire 0)
               (choose :spark))
           {:players [{:breaches [{}]
                       :discard  [dark-fire spark]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [crystal crystal spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 :dark-fire 0)
               (choose [:crystal :crystal]))
           {:players [{:breaches [{}]
                       :hand     [spark]
                       :discard  [dark-fire crystal crystal]}]
            :nemesis {:life 44}}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                                          :hand     [crystal crystal spark]}]
                               :nemesis {:life 50}}
                              (cast-spell 0 :dark-fire 0)
                              (choose [:crystal :crystal :spark]))))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 :dark-fire 0))
           {:players [{:breaches [{}]
                       :discard  [dark-fire]}]
            :nemesis {:life 50}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [dark-fire]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 :dark-fire 0))
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
               (cast-spell 0 :ignite 0)
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
                              (cast-spell 0 :ignite 0)
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

(deftest radiance-test
  (testing "Radiance"
    (is (= (-> {:players [{:breaches [{:prepped-spells [radiance]}]
                           :deck     [crystal]}
                          {:deck [crystal crystal]}
                          {:deck [crystal]}
                          {}]
                :nemesis {:life 50}}
               (cast-spell 0 :radiance 0))
           {:players [{:breaches [{}]
                       :deck     [crystal]
                       :discard  [radiance]}
                      {:deck [crystal]
                       :hand [crystal]}
                      {:hand [crystal]}
                      {}]
            :nemesis {:life 45}}))))
