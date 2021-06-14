(ns aeons-end.mages-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.starter :refer [crystal spark]]
            [aeons-end.cards.relic :refer [blasting-staff]]
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

(deftest gex-test
  (testing "Gex"
    (testing "Shattered Geode"
      (let [shattered-geode (assoc shattered-geode :id 1)
            crystal         (assoc crystal :id 2)
            spark           (assoc spark :id 3)]
        (is (= (-> {:players [{:hand [shattered-geode]}
                              {:discard [spark crystal]}]}
                   (play 0 :shattered-geode)
                   (choose nil))
               {:players [{:play-area [shattered-geode]
                           :aether    1}
                          {:discard [spark crystal]}]}))
        (is (= (-> {:players [{:hand [shattered-geode]}
                              {}]}
                   (play 0 :shattered-geode))
               {:players [{:play-area [shattered-geode]
                           :aether    1}
                          {}]}))
        (is (= (-> {:players [{:hand [shattered-geode]}
                              {:discard [spark crystal]}
                              {:discard [crystal]}]}
                   (play 0 :shattered-geode)
                   (choose {:player-no 1 :card-name :crystal :card-id 2}))
               {:players [{:hand      [crystal]
                           :play-area [shattered-geode]
                           :aether    1}
                          {:discard [spark]}
                          {:discard [crystal]}]}))
        (is (= (-> {:players [{:hand [shattered-geode]}
                              {:discard [spark crystal]}
                              {:discard [crystal]}]}
                   (play 0 :shattered-geode)
                   (choose {:player-no 2 :card-name :crystal :card-id 2}))
               {:players [{:hand      [crystal]
                           :play-area [shattered-geode]
                           :aether    1}
                          {:discard [spark crystal]}
                          {}]}))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:players [{:hand [shattered-geode]}
                                             {:discard [crystal spark]}]}
                                  (play 0 :shattered-geode)
                                  (choose {:player-no 1 :card-name :crystal :card-id 2}))))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:players [{:hand    [shattered-geode]
                                              :discard [spark crystal]}
                                             {}]}
                                  (play 0 :shattered-geode)
                                  (choose {:player-no 0 :card-id 2 :card-name :crystal}))))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:players [{:hand    [shattered-geode]
                                              :discard [spark crystal]}
                                             {:discard [spark crystal]}]}
                                  (play 0 :shattered-geode)
                                  (choose {:player-no 0 :card-id 2 :card-name :crystal}))))
        (is (= (-> {:players [{:hand    [shattered-geode]
                               :discard [spark crystal]}]}
                   (play 0 :shattered-geode)
                   (choose {:player-no 0 :card-id 2 :card-name :crystal}))
               {:players [{:hand      [crystal]
                           :play-area [shattered-geode]
                           :discard   [spark]
                           :aether    1}]}))
        (is (= (-> {:players [{:hand    [shattered-geode]
                               :discard [spark crystal]}]}
                   (play-all-gems 0)
                   (choose {:player-no 0 :card-id 2 :card-name :crystal}))
               {:players [{:play-area [shattered-geode crystal]
                           :discard   [spark]
                           :aether    2}]}))))
    (let [crystal (assoc crystal :id 1)
          spark   (assoc spark :id 2)]
      (testing "Vimcraft Oath"
        (is (= (-> {:players [{:ability (assoc vimcraft-oath :charges 5)
                               :discard [crystal spark]}
                              {:deck [crystal crystal]
                               :life 7}]}
                   (activate-ability 0)
                   (choose [{:player-no 0 :card-id 1 :card-name :crystal}
                            {:player-no 0 :card-id 2 :card-name :spark}])
                   (choose {:player-no 1}))
               {:players [{:ability (assoc vimcraft-oath :charges 0)}
                          {:hand [crystal]
                           :deck [crystal]
                           :life 9}]
                :trash   [crystal spark]}))
        (testing "Destroy starters"
          (is (= (-> {:players [{:ability (assoc vimcraft-oath :charges 5)
                                 :discard [crystal spark (assoc crystal :id 3)]}
                                {:life 7}]}
                     (activate-ability 0)
                     (choose [{:player-no 0 :card-id 1 :card-name :crystal}])
                     (choose {:player-no 1}))
                 {:players [{:ability (assoc vimcraft-oath :charges 0)
                             :discard [spark (assoc crystal :id 3)]}
                            {:life 9}]
                  :trash   [crystal]}))
          (is (= (-> {:players [{:ability (assoc vimcraft-oath :charges 5)
                                 :discard [crystal spark (assoc crystal :id 3)]}
                                {:life 7}]}
                     (activate-ability 0)
                     (choose [{:player-no 0 :card-id 3 :card-name :crystal}])
                     (choose {:player-no 1}))
                 {:players [{:ability (assoc vimcraft-oath :charges 0)
                             :discard [crystal spark]}
                            {:life 9}]
                  :trash   [(assoc crystal :id 3)]}))
          (is (= (-> {:players [{:ability (assoc vimcraft-oath :charges 5)
                                 :discard [crystal]}
                                {:life 7}]}
                     (activate-ability 0)
                     (choose {:player-no 0 :card-id 1 :card-name :crystal})
                     (choose {:player-no 1}))
                 {:players [{:ability (assoc vimcraft-oath :charges 0)}
                            {:life 9}]
                  :trash   [crystal]}))
          (is (= (-> {:players [{:ability (assoc vimcraft-oath :charges 5)
                                 :discard [crystal]}
                                {:life 7}]}
                     (activate-ability 0)
                     (choose nil)
                     (choose {:player-no 1}))
                 {:players [{:ability (assoc vimcraft-oath :charges 0)
                             :discard [crystal]}
                            {:life 9}]}))
          (is (= (-> {:players [{:ability (assoc vimcraft-oath :charges 5)}
                                {:life 7}]}
                     (activate-ability 0)
                     (choose {:player-no 1}))
                 {:players [{:ability (assoc vimcraft-oath :charges 0)}
                            {:life 9}]})))))))

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

(deftest quilius-test
  (testing "Quilius"
    (let [extinguish (assoc extinguish :id 1)]
      (testing "Extinguish"
        (is (= (-> {:players [{:breaches [{:prepped-spells [extinguish]}]}]
                    :nemesis {:life 50}}
                   (cast-spell 0 0 :extinguish))
               {:players [{:breaches [{}]
                           :discard  [extinguish]}]
                :nemesis {:life 49}}))
        (is (= (-> {:players [{:breaches [{:prepped-spells [extinguish]}]}]
                    :nemesis {:play-area [{:name :caterpillar
                                           :type :minion
                                           :life 2}]}}
                   (cast-spell 0 0 :extinguish)
                   (choose {:area :minions :card-name :caterpillar}))
               {:players [{:breaches [{}]
                           :discard  [extinguish]}]
                :nemesis {:play-area [{:name :caterpillar
                                       :type :minion
                                       :life 1}]}}))
        (is (= (-> {:players [{:name     :quilius
                               :breaches [{:prepped-spells [extinguish]}]}]
                    :nemesis {:play-area [{:name :caterpillar
                                           :type :minion
                                           :life 1}]}}
                   (cast-spell 0 0 :extinguish)
                   (choose {:area :minions :card-name :caterpillar}))
               {:players [{:name     :quilius
                           :breaches [{}]
                           :discard  [extinguish]
                           :trophies 1}]
                :nemesis {:discard [{:name :caterpillar
                                     :type :minion
                                     :life 0}]}}))
        (is (= (-> {:players [{:name     :quilius
                               :breaches [{:status         :opened
                                           :bonus-damage   1
                                           :prepped-spells [extinguish]}]}]
                    :nemesis {:play-area [{:name :caterpillar
                                           :type :minion
                                           :life 2}]}}
                   (cast-spell 0 0 :extinguish)
                   (choose {:area :minions :card-name :caterpillar}))
               {:players [{:name     :quilius
                           :breaches [{:status       :opened
                                       :bonus-damage 1}]
                           :discard  [extinguish]
                           :trophies 1}]
                :nemesis {:discard [{:name :caterpillar
                                     :type :minion
                                     :life 0}]}}))
        (is (= (-> {:players [{:name      :quilius
                               :breaches  [{:status         :opened
                                            :bonus-damage   1
                                            :prepped-spells [extinguish]}]
                               :hand      [blasting-staff]
                               :this-turn [{:prep :extinguish :id 1}]}]
                    :nemesis {:play-area [{:name :caterpillar
                                           :type :minion
                                           :life 4}]}}
                   (play 0 :blasting-staff)
                   (choose {:player-no 0 :breach-no 0 :card-name :extinguish})
                   (choose {:area :minions :card-name :caterpillar}))
               {:players [{:name      :quilius
                           :breaches  [{:status       :opened
                                        :bonus-damage 1}]
                           :play-area [blasting-staff]
                           :discard   [extinguish]
                           :trophies  1
                           :this-turn [{:prep :extinguish :id 1}]}]
                :nemesis {:discard [{:name :caterpillar
                                     :type :minion
                                     :life 0}]}}))
        (is (= (-> {:players [{:breaches [{:prepped-spells [extinguish]}]}
                              {:name :quilius}]
                    :nemesis {:play-area [{:name :caterpillar
                                           :type :minion
                                           :life 1}]}}
                   (cast-spell 0 0 :extinguish)
                   (choose {:area :minions :card-name :caterpillar}))
               {:players [{:breaches [{}]
                           :discard  [extinguish]}
                          {:name     :quilius
                           :trophies 1}]
                :nemesis {:discard [{:name :caterpillar
                                     :type :minion
                                     :life 0}]}}))))
    (testing "Quietus Vow"
      (is (= (-> {:players [{:ability (assoc quietus-vow :charges 5)}]
                  :nemesis {:life 50}}
                 (activate-ability 0))
             {:players [{:ability (assoc quietus-vow :charges 0)}]
              :nemesis {:life 50}}))
      (is (= (-> {:players [{:ability  (assoc quietus-vow :charges 5)
                             :trophies 1}]
                  :nemesis {:life 50}}
                 (activate-ability 0))
             {:players [{:ability  (assoc quietus-vow :charges 0)
                         :trophies 1}]
              :nemesis {:life 48}}))
      (is (= (-> {:players [{:ability  (assoc quietus-vow :charges 5)
                             :trophies 4}]
                  :nemesis {:life 50}}
                 (activate-ability 0))
             {:players [{:ability  (assoc quietus-vow :charges 0)
                         :trophies 4}]
              :nemesis {:life 42}})))))
