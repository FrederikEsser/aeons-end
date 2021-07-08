(ns aeons-end.mages-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.starter :refer [crystal spark]]
            [aeons-end.cards.relic :refer [blasting-staff]]
            [aeons-end.cards.spell :refer [ignite]]
            [aeons-end.cards.common]
            [aeons-end.nemesis]
            [aeons-end.mages :refer :all]
            [aeons-end.turn-order :as turn-order]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest adelheim-test
  (testing "Adelheim"
    (testing "Amethyst Shard"
      (is (= (-> {:players [{:hand [amethyst-shard]}
                            {:hand [spark]
                             :deck [crystal crystal]}]}
                 (play 0 :amethyst-shard)
                 (choose {:player-no 1})
                 (choose :spark))
             {:players [{:play-area [amethyst-shard]
                         :aether    1}
                        {:hand    [crystal]
                         :deck    [crystal]
                         :discard [spark]}]}))
      (is (= (-> {:players [{:hand [amethyst-shard]}
                            {:hand    [spark]
                             :discard [crystal crystal]}]}
                 (play 0 :amethyst-shard)
                 (choose {:player-no 1})
                 (choose :spark))
             {:players [{:play-area [amethyst-shard]
                         :aether    1}
                        {:hand    [crystal]
                         :deck    [crystal]
                         :discard [spark]}]}))
      (is (= (-> {:players [{:hand [amethyst-shard]}
                            {:hand [spark]}]}
                 (play 0 :amethyst-shard)
                 (choose {:player-no 1})
                 (choose :spark))
             {:players [{:play-area [amethyst-shard]
                         :aether    1}
                        {:discard [spark]}]}))
      (is (= (-> {:players [{:hand [amethyst-shard]
                             :deck [crystal]}
                            {:hand [spark]
                             :deck [crystal crystal]}]}
                 (play 0 :amethyst-shard)
                 (choose nil))
             {:players [{:play-area [amethyst-shard]
                         :deck      [crystal]
                         :aether    1}
                        {:hand [spark]
                         :deck [crystal crystal]}]})))
    (testing "Aetherial Ward"
      (is (= (-> {:nemesis   {:deck [{:name    :fugazi
                                      :type    :attack
                                      :effects [[:damage-gravehold 5]]}]}
                  :gravehold {:life 30}
                  :players   [{:ability (assoc aethereal-ward :charges 5)}]}
                 (draw-nemesis-card :auto-resolve? false)
                 (choose {:area :ability :player-no 0}))
             {:nemesis   {:discard [{:name    :fugazi
                                     :type    :attack
                                     :effects [[:damage-gravehold 5]]}]}
              :gravehold {:life 30}
              :players   [{:ability (assoc aethereal-ward :charges 0)}]}))
      (is (= (-> {:nemesis   {:deck [{:name    :fugazi
                                      :type    :attack
                                      :effects [[:damage-gravehold 5]]}]}
                  :gravehold {:life 30}
                  :players   [{:ability (assoc aethereal-ward :charges 5)}]}
                 (draw-nemesis-card :auto-resolve? true))
             {:nemesis   {:discard [{:name    :fugazi
                                     :type    :attack
                                     :effects [[:damage-gravehold 5]]}]}
              :gravehold {:life 25}
              :players   [{:ability (assoc aethereal-ward :charges 5)}]}))
      (is (= (-> {:nemesis {:deck [{:name :iznogood
                                    :type :power}]}
                  :players [{:ability (assoc aethereal-ward :charges 5)}]}
                 (draw-nemesis-card :auto-resolve? false)
                 (choose {:area :ability :player-no 0}))
             {:nemesis {:discard [{:name :iznogood
                                   :type :power}]}
              :players [{:ability (assoc aethereal-ward :charges 0)}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis {:deck [{:name :bad-motherfucker
                                                   :type :minion
                                                   :life 2}]}
                                 :players [{:ability (assoc aethereal-ward :charges 5)}]}
                                (draw-nemesis-card :auto-resolve? false)
                                (choose {:area :ability :player-no 0})))))))

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

(deftest dezmodia-test
  (testing "Dezmodia"
    (testing "Oblivion Shard"
      (is (= (-> {:players [{:hand [oblivion-shard]}]}
                 (play 0 :oblivion-shard))
             {:players [{:play-area         [oblivion-shard]
                         :restricted-aether {#{:relic :spell} 2}}]})))
    (testing "Tempest Sigil"
      (is (= (-> {:players [{:ability  (assoc tempest-sigil :charges 6)
                             :breaches [{:status :opened}]}]}
                 (activate-ability 0)
                 (choose {:player-no 0 :breach-no 0}))
             {:players [{:ability  (assoc tempest-sigil :charges 0)
                         :breaches [{:status       :opened
                                     :type         :sigil
                                     :bonus-damage 2}]}]}))
      (is (= (-> {:players [{:ability  (assoc tempest-sigil :charges 6)
                             :breaches [{:status :destroyed}
                                        {:status :closed}
                                        {:status :opened}]}]}
                 (activate-ability 0))
             {:players [{:ability  (assoc tempest-sigil :charges 0)
                         :breaches [{:status :destroyed}
                                    {:status :closed}
                                    {:status :opened}]}]}))
      (is (= (-> {:players [{:ability  (assoc tempest-sigil :charges 6)
                             :breaches [{:status :destroyed}
                                        {:status         :opened
                                         :prepped-spells [spark]}
                                        {:status :closed}]}]}
                 (activate-ability 0)
                 (choose {:player-no 0 :breach-no 1})
                 (choose :spark))
             {:players [{:ability  (assoc tempest-sigil :charges 0)
                         :breaches [{:status :destroyed}
                                    {:status         :opened
                                     :type           :sigil
                                     :bonus-damage   2
                                     :prepped-spells [spark]}
                                    {:status :closed}]}]}))
      (is (= (-> {:players [{:ability  (assoc tempest-sigil :charges 6)
                             :breaches [{:status :destroyed}
                                        {:status         :opened
                                         :prepped-spells [spark]}
                                        {:status :closed}]}]}
                 (activate-ability 0)
                 (choose {:player-no 0 :breach-no 1})
                 (choose nil))
             {:players [{:ability  (assoc tempest-sigil :charges 0)
                         :breaches [{:status :destroyed}
                                    {:status       :opened
                                     :type         :sigil
                                     :bonus-damage 2}
                                    {:status :closed}]
                         :hand     [spark]}]}))
      (is (= (-> {:players [{:ability (assoc tempest-sigil :charges 6)}
                            {:breaches [{:status         :opened
                                         :prepped-spells [spark]}]
                             :hand     [ignite]}]}
                 (activate-ability 0)
                 (choose {:player-no 1 :breach-no 0})
                 (choose :ignite))
             {:players [{:ability (assoc tempest-sigil :charges 0)}
                        {:breaches [{:status         :opened
                                     :type           :sigil
                                     :bonus-damage   2
                                     :prepped-spells [ignite]}]
                         :hand     [spark]}]})))))

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
                   (choose {:player-no 1 :card-id 2}))
               {:players [{:hand      [crystal]
                           :play-area [shattered-geode]
                           :aether    1}
                          {:discard [spark]}
                          {:discard [crystal]}]}))
        (is (= (-> {:players [{:hand [shattered-geode]}
                              {:discard [spark crystal]}
                              {:discard [crystal]}]}
                   (play 0 :shattered-geode)
                   (choose {:player-no 2 :card-id 2}))
               {:players [{:hand      [crystal]
                           :play-area [shattered-geode]
                           :aether    1}
                          {:discard [spark crystal]}
                          {}]}))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:players [{:hand [shattered-geode]}
                                             {:discard [crystal spark]}]}
                                  (play 0 :shattered-geode)
                                  (choose {:player-no 1 :card-id 2}))))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:players [{:hand    [shattered-geode]
                                              :discard [spark crystal]}
                                             {}]}
                                  (play 0 :shattered-geode)
                                  (choose {:player-no 0 :card-id 2}))))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:players [{:hand    [shattered-geode]
                                              :discard [spark crystal]}
                                             {:discard [spark crystal]}]}
                                  (play 0 :shattered-geode)
                                  (choose {:player-no 0 :card-id 2}))))
        (is (= (-> {:players [{:hand    [shattered-geode]
                               :discard [spark crystal]}]}
                   (play 0 :shattered-geode)
                   (choose {:player-no 0 :card-id 2}))
               {:players [{:hand      [crystal]
                           :play-area [shattered-geode]
                           :discard   [spark]
                           :aether    1}]}))
        (is (= (-> {:players [{:hand    [shattered-geode]
                               :discard [spark crystal]}]}
                   (play-all-gems 0)
                   (choose {:player-no 0 :card-id 2}))
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
                   (choose [{:player-no 0 :card-id 1}
                            {:player-no 0 :card-id 2}])
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
                     (choose [{:player-no 0 :card-id 1}])
                     (choose {:player-no 1}))
                 {:players [{:ability (assoc vimcraft-oath :charges 0)
                             :discard [spark (assoc crystal :id 3)]}
                            {:life 9}]
                  :trash   [crystal]}))
          (is (= (-> {:players [{:ability (assoc vimcraft-oath :charges 5)
                                 :discard [crystal spark (assoc crystal :id 3)]}
                                {:life 7}]}
                     (activate-ability 0)
                     (choose [{:player-no 0 :card-id 3}])
                     (choose {:player-no 1}))
                 {:players [{:ability (assoc vimcraft-oath :charges 0)
                             :discard [crystal spark]}
                            {:life 9}]
                  :trash   [(assoc crystal :id 3)]}))
          (is (= (-> {:players [{:ability (assoc vimcraft-oath :charges 5)
                                 :discard [crystal]}
                                {:life 7}]}
                     (activate-ability 0)
                     (choose {:player-no 0 :card-id 1})
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

(deftest lash-test
  (testing "Lash"
    (testing "Quartz Shard"
      (is (= (-> {:players    [{:hand [quartz-shard]}]
                  :turn-order {:deck [turn-order/nemesis
                                      turn-order/player-1]}}
                 (play 0 :quartz-shard)
                 (choose :bottom))
             {:players    [{:play-area [quartz-shard]
                            :aether    1}]
              :turn-order {:deck [turn-order/player-1
                                  turn-order/nemesis]}}))
      (is (= (-> {:players    [{:hand [quartz-shard]}]
                  :turn-order {:deck [turn-order/player-1
                                      turn-order/nemesis]}}
                 (play 0 :quartz-shard)
                 (choose :top))
             {:players    [{:play-area [quartz-shard]
                            :aether    2}]
              :turn-order {:deck           [turn-order/player-1
                                            turn-order/nemesis]
                           :revealed-cards 1}}))
      (is (= (-> {:players    [{:hand [quartz-shard]}]
                  :turn-order {:deck [turn-order/wild
                                      turn-order/nemesis]}}
                 (play 0 :quartz-shard)
                 (choose :bottom))
             {:players    [{:play-area [quartz-shard]
                            :aether    2}]
              :turn-order {:deck [turn-order/nemesis
                                  turn-order/wild]}})))
    (testing "Quicken Thought"
      (is (= (-> {:players    [{:ability (assoc quicken-thought :charges 5)
                                :life    10}]
                  :turn-order {:deck    [turn-order/nemesis
                                         turn-order/player-2]
                               :discard [turn-order/player-1]}}
                 (activate-ability 0)
                 (choose :player-1))
             {:players    [{:ability (assoc quicken-thought :charges 0)
                            :life    9}]
              :turn-order {:deck [turn-order/player-2
                                  turn-order/nemesis
                                  turn-order/player-1]}}))
      (is (= (-> {:current-player 1
                  :players        [{:ability (assoc quicken-thought :charges 5)
                                    :life    10}
                                   {:life  10
                                    :phase :casting}]
                  :turn-order     {:deck    [turn-order/nemesis]
                                   :discard [turn-order/player-1
                                             turn-order/player-2]}}
                 (activate-ability 0)
                 (choose :player-2))
             {:current-player 1
              :players        [{:ability (assoc quicken-thought :charges 0)
                                :life    10}
                               {:life  9
                                :phase :main}]
              :turn-order     {:deck    [turn-order/nemesis
                                         turn-order/player-2]
                               :discard [turn-order/player-1]}}))
      (is (= (-> {:current-player 1
                  :players        [{:ability (assoc quicken-thought :charges 5)
                                    :life    10}
                                   {:life  10
                                    :phase :main}]
                  :turn-order     {:deck    [turn-order/nemesis]
                                   :discard [turn-order/player-1
                                             turn-order/player-2]}}
                 (activate-ability 0)
                 (choose :player-1))
             {:current-player 1
              :players        [{:ability (assoc quicken-thought :charges 0)
                                :life    9}
                               {:life  10
                                :phase :main}]
              :turn-order     {:deck    [turn-order/player-1
                                         turn-order/nemesis]
                               :discard [turn-order/player-2]}}))
      (is (= (-> {:current-player 0
                  :players        [{:ability (assoc quicken-thought :charges 5)
                                    :life    10
                                    :phase   :main}
                                   {:life 10}]
                  :turn-order     {:deck    [turn-order/player-1
                                             turn-order/player-2]
                                   :discard [turn-order/nemesis
                                             turn-order/wild]}}
                 (activate-ability 0))
             {:current-player 0
              :players        [{:ability (assoc quicken-thought :charges 0)
                                :life    10
                                :phase   :main}
                               {:life 10}]
              :turn-order     {:deck    [turn-order/player-1
                                         turn-order/player-2]
                               :discard [turn-order/nemesis
                                         turn-order/wild]}})))))

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
                   (choose {:area :minions :player-no 0 :card-name :caterpillar}))
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
                   (choose {:area :minions :player-no 0 :card-name :caterpillar}))
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
                   (choose {:area :minions :player-no 0 :card-name :caterpillar}))
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
                   (choose {:area :minions :player-no 0 :card-name :caterpillar}))
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
                   (choose {:area :minions :player-no 0 :card-name :caterpillar}))
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
