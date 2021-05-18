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
      (is (= (-> {:players [{:breaches [{:prepped-spells [buried-light]}]
                             :phase    :casting}]
                  :nemesis {:life 50}}
                 (cast-spell 0 :buried-light 0))
             {:players [{:breaches [{}]
                         :discard  [buried-light]
                         :aether   1
                         :phase    :casting}]
              :nemesis {:life 49}})))))

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
                 (choose {:area      :prepped-spells
                          :player-no 0
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
                 (choose {:area      :prepped-spells
                          :player-no 1
                          :breach-no 0
                          :card-name :buried-light}))
             {:current-player 0
              :nemesis        {:life 49}
              :players        [{:play-area [garnet-shard]}
                               {:breaches [{}]
                                :discard  [buried-light]
                                :aether   0}]})))))