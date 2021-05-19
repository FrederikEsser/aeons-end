(ns aeons-end.spells-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.spells :refer :all]
            [aeons-end.mages :refer [garnet-shard]]))

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
               (choose :cast)
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
