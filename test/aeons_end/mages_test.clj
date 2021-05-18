(ns aeons-end.mages-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.cards.base :refer [spark]]
            [aeons-end.cards.common]
            [aeons-end.operations :refer [choose]]
            [aeons-end.mages :refer :all]))

(deftest brama-test
  (testing "Brama"
    (testing "Buried Light"
      (is (= (-> {:players [{:breaches [{:prepped-spells [buried-light]}]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 :buried-light 0))
             {:players [{:breaches [{}]
                         :discard  [buried-light]
                         :aether   1}]
              :nemesis {:life 49}})))
    (testing "Brink Siphon"
      (is (= (-> {:players [{:ability (assoc brink-siphon :charges 5)
                             :life    5}]}
                 (activate-ability 0)
                 (choose {:player-no 0}))
             {:players [{:ability (assoc brink-siphon :charges 0)
                         :life    9}]}))
      (is (= (-> {:players [{:ability (assoc brink-siphon :charges 5)
                             :life    7}]}
                 (activate-ability 0)
                 (choose {:player-no 0}))
             {:players [{:ability (assoc brink-siphon :charges 0)
                         :life    10}]}))
      (is (= (-> {:players [{:ability (assoc brink-siphon :charges 5)
                             :life    10}]}
                 (activate-ability 0)
                 (choose {:player-no 0}))
             {:players [{:ability (assoc brink-siphon :charges 0)
                         :life    10}]}))
      (is (= (-> {:players [{:ability (assoc brink-siphon :charges 5)}
                            {:life 1}]}
                 (activate-ability 0)
                 (choose {:player-no 1}))
             {:players [{:ability (assoc brink-siphon :charges 0)}
                        {:life 5}]})))))

(deftest mist-test
  (testing "Mist"
    (testing "Garnet Shard"
      (is (= (-> {:players [{:hand [garnet-shard]}]}
                 (play 0 :garnet-shard)
                 (choose :aether))
             {:players [{:play-area [garnet-shard]
                         :aether    1}]}))
      (is (= (-> {:players [{:hand [garnet-shard]}]}
                 (play 0 :garnet-shard)
                 (choose :cast))
             {:players [{:play-area [garnet-shard]}]}))
      (is (= (-> {:nemesis {:life 50}
                  :players [{:breaches [{:prepped-spells [spark]}]
                             :hand     [garnet-shard]}]}
                 (play 0 :garnet-shard)
                 (choose :cast)
                 (choose {:player-no 0
                          :breach-no 0
                          :card-name :spark}))
             {:nemesis {:life 49}
              :players [{:breaches  [{}]
                         :play-area [garnet-shard]
                         :discard   [spark]}]}))
      (is (= (-> {:current-player 0
                  :nemesis        {:life 50}
                  :players        [{:hand [garnet-shard]}
                                   {:breaches [{:prepped-spells [buried-light]}]
                                    :aether   0}]}
                 (play 0 :garnet-shard)
                 (choose :cast)
                 (choose {:player-no 1
                          :breach-no 0
                          :card-name :buried-light}))
             {:current-player 0
              :nemesis        {:life 49}
              :players        [{:play-area [garnet-shard]}
                               {:breaches [{}]
                                :discard  [buried-light]
                                :aether   0}]})))))