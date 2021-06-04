(ns aeons-end.mages-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.starter :refer [crystal spark]]
            [aeons-end.cards.common]
            [aeons-end.nemesis]
            [aeons-end.mages :refer :all]
            [aeons-end.utils :as ut]))

(deftest brama-test
  (testing "Brama"
    (testing "Buried Light"
      (is (= (-> {:players [{:breaches [{:prepped-spells [buried-light]}]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :buried-light))
             {:players [{:breaches [{}]
                         :discard  [buried-light]
                         :aether   1}]
              :nemesis {:life 49}})))
    (testing "Brink Siphon"
      (is (= (-> {:players [{:ability (assoc brink-siphon :charges 5)
                             :life    3}]}
                 (activate-ability 0)
                 (choose {:player-no 0}))
             {:players [{:ability (assoc brink-siphon :charges 0)
                         :life    7}]}))
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
      (is (= (-> {:players [{:ability (assoc brink-siphon :charges 5)
                             :life    5}
                            {:life 1}]}
                 (activate-ability 0)
                 (choose {:player-no 1}))
             {:players [{:ability (assoc brink-siphon :charges 0)
                         :life    5}
                        {:life 5}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:players [{:ability (assoc brink-siphon :charges 5)
                                            :life    5}
                                           {:life 0}]}
                                (activate-ability 0)
                                (choose {:player-no 1})))))))

(deftest mist-test
  (testing "Mist"
    (testing "Garnet Shard"
      (is (= (-> {:players [{:hand [garnet-shard]}]}
                 (play 0 :garnet-shard))
             {:players [{:play-area [garnet-shard]
                         :aether    1}]}))
      (is (= (-> {:players [{:breaches [{:prepped-spells [spark]}]
                             :hand     [garnet-shard]}]}
                 (play 0 :garnet-shard)
                 (choose []))
             {:players [{:breaches  [{:prepped-spells [spark]}]
                         :play-area [garnet-shard]
                         :aether    1}]}))
      (is (= (-> {:nemesis {:life 50}
                  :players [{:breaches [{:prepped-spells [spark]}]
                             :hand     [garnet-shard]}]}
                 (play 0 :garnet-shard)
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
                                   {:breaches [{:status         :opened
                                                :bonus-damage   1
                                                :prepped-spells [buried-light]}]}]}
                 (play 0 :garnet-shard)
                 (choose {:player-no 1
                          :breach-no 0
                          :card-name :buried-light}))
             {:current-player 0
              :nemesis        {:life 48}
              :players        [{:play-area [garnet-shard]
                                :aether    1}
                               {:breaches [{:status       :opened
                                            :bonus-damage 1}]
                                :discard  [buried-light]}]})))
    (testing "Divine Augury"
      (is (= (-> {:players [{:ability (assoc divine-augury :charges 5)}
                            {:hand [crystal crystal crystal crystal crystal]
                             :deck [spark spark spark spark spark]}]}
                 (activate-ability 0)
                 (choose {:player-no 1}))
             {:players [{:ability (assoc divine-augury :charges 0)}
                        {:hand [crystal crystal crystal crystal crystal spark spark spark spark]
                         :deck [spark]}]}))
      (is (= (-> {:players [{:ability (assoc divine-augury :charges 5)}
                            {:hand [crystal crystal crystal crystal crystal]
                             :deck [spark spark spark]}]}
                 (activate-ability 0)
                 (choose {:player-no 1}))
             {:players [{:ability (assoc divine-augury :charges 0)}
                        {:hand [crystal crystal crystal crystal crystal spark spark spark]}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:players [{:ability (assoc divine-augury :charges 5)}
                                           {:hand [crystal crystal crystal crystal crystal]
                                            :deck [spark spark spark spark spark]}]}
                                (activate-ability 0)
                                (choose {:player-no 0}))))
      (is (= (-> {:players [{:ability (assoc divine-augury :charges 5)
                             :hand    [crystal crystal crystal crystal crystal]
                             :deck    [spark spark spark spark spark]}]}
                 (activate-ability 0)
                 (choose {:player-no 0}))
             {:players [{:ability (assoc divine-augury :charges 0)
                         :hand    [crystal crystal crystal crystal crystal spark spark spark spark]
                         :deck    [spark]}]})))))
