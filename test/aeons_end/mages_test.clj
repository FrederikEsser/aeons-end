(ns aeons-end.mages-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.starter :refer [crystal spark]]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.relic :refer [blasting-staff focusing-orb]]
            [aeons-end.cards.spell :refer [aurora blaze feral-lightning ignite pyrotechnic-surge radiance]]
            [aeons-end.cards.common]
            [aeons-end.nemesis]
            [aeons-end.cards.minion :refer [catacomb-drone cauterizer]]
            [aeons-end.mages :refer :all]
            [aeons-end.turn-order :as turn-order]
            [aeons-end.utils :as ut]))

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
                 (activate-ability 0))
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

(deftest garu-test
  (testing "Garu"
    (testing "Torch"
      (is (= (-> {:players [{:breaches [{:status         :closed
                                         :prepped-spells [torch]}]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :torch)
                 (choose {:area      :nemesis
                          :player-no 0
                          :card-name :nemesis}))
             {:players [{:breaches [{:status :closed}]
                         :discard  [torch]}]
              :nemesis {:life 49}}))
      (is (= (-> {:players [{:breaches [{:status         :opened
                                         :prepped-spells [torch]}]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :torch))
             {:players [{:breaches [{:status :opened}]
                         :discard  [torch]}]
              :nemesis {:life 49}}))
      (is (= (-> {:players [{:breaches [{:status         :closed
                                         :stage          0
                                         :prepped-spells [torch]}]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :torch)
                 (choose {:area :breaches :player-no 0 :breach-no 0}))
             {:players [{:breaches [{:status :focused
                                     :stage  1}]
                         :discard  [torch]}]
              :nemesis {:life 50}}))
      (is (= (-> {:players [{:breaches [{:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [torch]}
                                        {:status :closed
                                         :stage  0}]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :torch)
                 (choose {:area :breaches :player-no 0 :breach-no 1}))
             {:players [{:breaches [{:status       :opened
                                     :bonus-damage 1}
                                    {:status :focused
                                     :stage  1}]
                         :discard  [torch]}]
              :nemesis {:life 49}})))
    (testing "Colossal Force"
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}]}]
                  :nemesis {:life 50}}
                 (activate-ability 0)
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}]))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{}]
                         :discard  [spark]}]
              :nemesis {:life 44}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}]}]
                  :nemesis {:life 50}}
                 (activate-ability 0)
                 (choose nil))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{:prepped-spells [spark]}]}]
              :nemesis {:life 48}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{}]}]
                  :nemesis {:life 50}}
                 (activate-ability 0))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{}]}]
              :nemesis {:life 48}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [spark]}
                                        {:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [spark]}]}]
                  :nemesis {:life 50}}
                 (activate-ability 0)
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                          {:player-no 0 :breach-no 1 :card-name :spark}
                          {:player-no 0 :breach-no 2 :card-name :spark}
                          {:player-no 0 :breach-no 3 :card-name :spark}]))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{} {} {}
                                    {:status       :opened
                                     :bonus-damage 1}]
                         :discard  [spark spark spark spark]}]
              :nemesis {:life 32}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}]}]
                  :nemesis {:life      50
                            :play-area [{:name :minion-1
                                         :type :minion
                                         :life 5}]}}
                 (activate-ability 0)
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}])
                 (choose {:area      :minions
                          :player-no 0
                          :card-name :minion-1}))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{}]
                         :discard  [spark]}]
              :nemesis {:life    49
                        :discard [{:name :minion-1
                                   :type :minion
                                   :life 0}]}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}]}]
                  :nemesis {:life      50
                            :play-area [{:name       :minion-1
                                         :type       :minion
                                         :life       3
                                         :max-damage 1}]}}
                 (activate-ability 0)
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}])
                 (choose {:area      :minions
                          :player-no 0
                          :card-name :minion-1}))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{}]
                         :discard  [spark]}]
              :nemesis {:life      45
                        :play-area [{:name       :minion-1
                                     :type       :minion
                                     :life       2
                                     :max-damage 1}]}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}]}]
                  :nemesis {:life   50
                            :shield 3}}
                 (activate-ability 0)
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}]))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{}]
                         :discard  [spark]}]
              :nemesis {:life   47
                        :shield 3}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}]}]
                  :nemesis {:life      50
                            :play-area [{:name   :minion-1
                                         :type   :minion
                                         :life   3
                                         :shield 1}]}}
                 (activate-ability 0)
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}])
                 (choose {:area      :minions
                          :player-no 0
                          :card-name :minion-1}))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{}]
                         :discard  [spark]}]
              :nemesis {:life    48
                        :discard [{:name   :minion-1
                                   :type   :minion
                                   :life   0
                                   :shield 1}]}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}]}]
                  :nemesis {:life      50
                            :play-area [{:name   :minion-1
                                         :type   :minion
                                         :life   6
                                         :shield 1}]}}
                 (activate-ability 0)
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}])
                 (choose {:area      :minions
                          :player-no 0
                          :card-name :minion-1}))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{}]
                         :discard  [spark]}]
              :nemesis {:life      50
                        :play-area [{:name   :minion-1
                                     :type   :minion
                                     :life   1
                                     :shield 1}]}}))
      (is (= (-> {:players [{:ability  (assoc colossal-force :charges 5)
                             :breaches [{:prepped-spells [spark]}]}]
                  :nemesis {:husks {:number-of-husks 3}
                            :life  50}}
                 (activate-ability 0)
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}])
                 (choose {:area :minions :player-no 0 :card-name :husks}))
             {:players [{:ability  (assoc colossal-force :charges 0)
                         :breaches [{}]
                         :discard  [spark]}]
              :nemesis {:husks {:number-of-husks 0}
                        :life  47}})))))

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

(deftest indira-test
  (testing "Indira"
    (testing "Twin Opal"
      (is (= (-> {:players [{:hand [twin-opal spark]}]
                  :nemesis {:life 50}}
                 (play 0 :twin-opal)
                 (choose :spark))
             {:players [{:play-area [twin-opal]
                         :discard   [spark]
                         :aether    1}]
              :nemesis {:life 49}}))
      (is (= (-> {:players [{:hand [twin-opal spark]}]
                  :nemesis {:life 50}}
                 (play 0 :twin-opal)
                 (choose nil))
             {:players [{:hand      [spark]
                         :play-area [twin-opal]
                         :aether    1}]
              :nemesis {:life 50}}))
      (is (= (-> {:players [{:hand [twin-opal crystal]}]
                  :nemesis {:life 50}}
                 (play 0 :twin-opal))
             {:players [{:hand      [crystal]
                         :play-area [twin-opal]
                         :aether    1}]
              :nemesis {:life 50}})))
    (testing "Pyromancer's Guile"
      (let [spark   (assoc spark :id 1)
            crystal (assoc crystal :id 2)]
        (is (= (-> {:players [{:ability (assoc pyromancers-guile :charges 4)
                               :hand    [spark]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose :spark)
                   (choose nil))
               {:players [{:ability (assoc pyromancers-guile :charges 0)
                           :discard [spark]}]
                :nemesis {:life 48}}))
        (is (= (-> {:players [{:ability (assoc pyromancers-guile :charges 4)
                               :hand    [spark]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose :spark)
                   (choose {:player-no 0 :card-id 1}))
               {:players [{:ability (assoc pyromancers-guile :charges 0)}]
                :nemesis {:life 48}
                :trash   [spark]}))
        (is (= (-> {:players [{:ability (assoc pyromancers-guile :charges 4)
                               :hand    [spark]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose nil))
               {:players [{:ability (assoc pyromancers-guile :charges 0)
                           :hand    [spark]}]
                :nemesis {:life 50}}))
        (is (= (-> {:players [{:ability (assoc pyromancers-guile :charges 4)
                               :hand    [spark spark]
                               :discard [crystal]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose :spark)
                   (choose :spark)
                   (choose {:player-no 0 :card-id 2}))
               {:players [{:ability (assoc pyromancers-guile :charges 0)
                           :discard [spark spark]}]
                :nemesis {:life 46}
                :trash   [crystal]}))
        (is (= (-> {:players [{:ability (assoc pyromancers-guile :charges 4)
                               :hand    [spark spark]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose :spark)
                   (choose nil)
                   (choose nil))
               {:players [{:ability (assoc pyromancers-guile :charges 0)
                           :hand    [spark]
                           :discard [spark]}]
                :nemesis {:life 48}}))))))

(deftest kadir-test
  (testing "Kadir"
    (testing "Otherworldly Gate"
      (let [spark  (assoc spark :id 1)
            ignite (assoc ignite :id 2)]
        (is (= (-> {:current-player 1
                    :players        [{:ability (assoc otherworldly-gate :charges 5)}
                                     {:discard [crystal spark spark ignite ignite crystal]}]}
                   (activate-ability 0)
                   (choose [{:player-no 1 :card-id 2}
                            {:player-no 1 :card-id 2}
                            {:player-no 1 :card-id 1}]))
               {:current-player 1
                :players        [{:ability (assoc otherworldly-gate :charges 0)}
                                 {:hand            [ignite ignite spark]
                                  :discard         [crystal spark crystal]
                                  :breach-capacity 2}]}))
        (is (= (-> {:current-player 1
                    :players        [{:ability (assoc otherworldly-gate :charges 5)}
                                     {:discard [crystal spark crystal]}]}
                   (activate-ability 0)
                   (choose {:player-no 1 :card-id 1}))
               {:current-player 1
                :players        [{:ability (assoc otherworldly-gate :charges 0)}
                                 {:hand            [spark]
                                  :discard         [crystal crystal]
                                  :breach-capacity 2}]}))
        (is (= (-> {:current-player 1
                    :players        [{:ability (assoc otherworldly-gate :charges 5)}
                                     {:discard [crystal crystal]}]}
                   (activate-ability 0))
               {:current-player 1
                :players        [{:ability (assoc otherworldly-gate :charges 0)}
                                 {:discard         [crystal crystal]
                                  :breach-capacity 2}]}))
        (is (= (-> {:players [{:hand            [ignite]
                               :breach-capacity 2
                               :breaches        [{:status         :opened
                                                  :prepped-spells [spark]}]}]}
                   (prep-spell 0 0 :ignite))
               {:players [{:breach-capacity 2
                           :breaches        [{:status         :opened
                                              :prepped-spells [spark ignite]}]}]}))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand            [ignite]
                                              :breach-capacity 2
                                              :breaches        [{:status         :focused
                                                                 :prepped-spells [spark]}]}]}
                                  (prep-spell 0 0 :ignite))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand            [spark]
                                              :breach-capacity 2
                                              :breaches        [{:status         :opened
                                                                 :prepped-spells [spark ignite]}]}]}
                                  (prep-spell 0 0 :spark))))
        (is (= (-> {:players [{:hand            [ignite]
                               :breach-capacity 2
                               :breaches        [{:status         :opened
                                                  :prepped-spells [spark spark]}
                                                 {:status         :opened
                                                  :prepped-spells [spark]}]}]}
                   (prep-spell 0 1 :ignite))
               {:players [{:breach-capacity 2
                           :breaches        [{:status         :opened
                                              :prepped-spells [spark spark]}
                                             {:status         :opened
                                              :prepped-spells [spark ignite]}]}]}))
        (is (= (-> {:players [{:breach-capacity 2
                               :breaches        [{:status         :opened
                                                  :prepped-spells [spark spark]}]
                               :phase           :main}]}
                   (end-turn 0))
               {:players [{:breaches [{:status         :opened
                                       :prepped-spells [spark spark]}]
                           :phase    :out-of-turn}]}))))))

(deftest lash-test
  (testing "Lash"
    (testing "Quartz Shard"
      (is (= (-> {:players    [{:hand [quartz-shard]}]
                  :turn-order {:deck [turn-order/nemesis
                                      turn-order/player-1]}}
                 (play 0 :quartz-shard)
                 (choose :nemesis))
             {:players    [{:play-area [quartz-shard]
                            :aether    1}]
              :turn-order {:deck [turn-order/player-1
                                  turn-order/nemesis]}}))
      (is (= (-> {:players    [{:hand [quartz-shard]}]
                  :turn-order {:deck [turn-order/player-1
                                      turn-order/nemesis]}}
                 (play 0 :quartz-shard)
                 (choose nil))
             {:players    [{:play-area [quartz-shard]
                            :aether    2}]
              :turn-order {:deck           [turn-order/player-1
                                            turn-order/nemesis]
                           :revealed-cards 1}}))
      (is (= (-> {:players    [{:hand [quartz-shard]}]
                  :turn-order {:deck [turn-order/wild
                                      turn-order/nemesis]}}
                 (play 0 :quartz-shard)
                 (choose :wild))
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

(deftest malastar-test
  (testing "Malastar"
    (testing "Immolate"
      (testing "While prepped"
        (is (= (-> {:players [{:breaches [{:prepped-spells [immolate]}]
                               :ability  {:charges     0
                                          :charge-cost 6}
                               :life     10}]}
                   (damage-player 0 1))
               {:players [{:breaches [{:prepped-spells [immolate]}]
                           :ability  {:charges     1
                                      :charge-cost 6}
                           :life     9}]}))
        (is (= (-> {:players   [{:breaches [{:prepped-spells [immolate]}]
                                 :ability  {:charges     0
                                            :charge-cost 6}
                                 :life     0}]
                    :gravehold {:life 30}}
                   (damage-player 0 1))
               {:players   [{:breaches [{:prepped-spells [immolate]}]
                             :ability  {:charges     1
                                        :charge-cost 6}
                             :life     0}]
                :gravehold {:life 28}}))))
    (testing "Gift of Aether"
      (let [radiance-1          (assoc radiance :id 1)
            pyrotechnic-surge-1 (assoc pyrotechnic-surge :id 1)
            blaze-1             (assoc blaze :id 1)]
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card radiance :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status :opened}]}]}
                   (activate-ability 0)
                   (choose :radiance)
                   (choose {:player-no 0 :breach-no 0}))
               {:supply  [{:card radiance :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status         :opened
                                       :prepped-spells [radiance-1]}]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card radiance :pile-size 5}]
                    :players [{:ability (assoc gift-of-aether :charges 6)}
                              {:breaches [{:status :opened}]}]}
                   (activate-ability 0)
                   (choose :radiance)
                   (choose {:player-no 1 :breach-no 0}))
               {:supply  [{:card radiance :pile-size 4}]
                :players [{:ability (assoc gift-of-aether :charges 0)}
                          {:breaches [{:status         :opened
                                       :prepped-spells [radiance-1]}]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card radiance :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status :opened}]}]}
                   (activate-ability 0)
                   (choose :radiance)
                   (choose nil))
               {:supply  [{:card radiance :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status :opened}]
                           :discard  [radiance-1]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card radiance :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status         :opened
                                           :prepped-spells [spark]}]}]}
                   (activate-ability 0)
                   (choose :radiance))
               {:supply  [{:card radiance :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status         :opened
                                       :prepped-spells [spark]}]
                           :discard  [radiance-1]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card radiance :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status :focused}]}]}
                   (activate-ability 0)
                   (choose :radiance))
               {:supply  [{:card radiance :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status :focused}]
                           :discard  [radiance-1]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card pyrotechnic-surge :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status :opened}
                                          {:status :opened}]}]}
                   (activate-ability 0)
                   (choose :pyrotechnic-surge)
                   (choose {:player-no 0 :breach-no 0}))
               {:supply  [{:card pyrotechnic-surge :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status         :opened
                                       :prepped-spells [pyrotechnic-surge-1]}
                                      {:status :opened}]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card pyrotechnic-surge :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status :opened}
                                          {:status :focused}]}]}
                   (activate-ability 0)
                   (choose :pyrotechnic-surge))
               {:supply  [{:card pyrotechnic-surge :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status :opened}
                                      {:status :focused}]
                           :discard  [pyrotechnic-surge-1]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card pyrotechnic-surge :pile-size 4}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status         :opened
                                           :prepped-spells [pyrotechnic-surge]}
                                          {:status :opened}
                                          {:status :opened}]}]}
                   (activate-ability 0)
                   (choose :pyrotechnic-surge))
               {:supply  [{:card pyrotechnic-surge :pile-size 3}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status         :opened
                                       :prepped-spells [pyrotechnic-surge]}
                                      {:status :opened}
                                      {:status :opened}]
                           :discard  [pyrotechnic-surge-1]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card pyrotechnic-surge :pile-size 5}]
                    :players [{:ability         (assoc gift-of-aether :charges 6)
                               :breach-capacity 2
                               :breaches        [{:status :opened}
                                                 {:status         :opened
                                                  :prepped-spells [spark]}]}]}
                   (activate-ability 0)
                   (choose :pyrotechnic-surge)
                   (choose {:player-no 0 :breach-no 0}))
               {:supply  [{:card pyrotechnic-surge :pile-size 4}]
                :players [{:ability         (assoc gift-of-aether :charges 0)
                           :breach-capacity 2
                           :breaches        [{:status         :opened
                                              :prepped-spells [pyrotechnic-surge-1]}
                                             {:status         :opened
                                              :prepped-spells [spark]}]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card feral-lightning :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status :closed}]}]}
                   (activate-ability 0)
                   (choose :feral-lightning))
               {:supply  [{:card feral-lightning :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status :closed}]
                           :discard  [(assoc feral-lightning :id 1)]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card blaze :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status :opened}]}
                              {}]}
                   (activate-ability 0)
                   (choose :blaze)
                   (choose {:player-no 1}))
               {:supply  [{:card blaze :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status :opened}]}
                          {:discard [blaze-1]}]}))
        (ut/reset-ids! 0)
        (is (= (-> {:supply  [{:card blaze :pile-size 5}]
                    :players [{:ability  (assoc gift-of-aether :charges 6)
                               :breaches [{:status :opened}]}
                              {:breaches [{:status :opened}]}]}
                   (activate-ability 0)
                   (choose :blaze)
                   (choose {:player-no 0})
                   (choose {:player-no 1 :breach-no 0}))
               {:supply  [{:card blaze :pile-size 4}]
                :players [{:ability  (assoc gift-of-aether :charges 0)
                           :breaches [{:status :opened}]}
                          {:breaches [{:status         :opened
                                       :prepped-spells [blaze-1]}]}]}))))))

(deftest mazahaedron-test
  (testing "Mazahaedron"
    (testing "Worldheart Shard"
      (let [jade (assoc jade :id 1)]
        (is (= (-> {:players [{:hand [worldheart-shard]}]}
                   (play 0 :worldheart-shard)
                   (choose :gain-1))
               {:players [{:play-area [worldheart-shard]
                           :aether    1}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:hand [worldheart-shard]}
                              {}]}
                   (play 0 :worldheart-shard)
                   (choose :gain-2)
                   (buy-card 0 :jade)
                   (choose {:player-no 1}))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:play-area [worldheart-shard]}
                          {:discard [jade]}]}))
        (is (thrown-with-msg? AssertionError #"Pay error:"
                              (-> {:supply  [{:card jade :pile-size 7}]
                                   :players [{:hand    [worldheart-shard]
                                              :ability {:charge-cost 5}}
                                             {}]}
                                  (play 0 :worldheart-shard)
                                  (choose :gain-2)
                                  (charge-ability 0))))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:supply  [{:card jade :pile-size 7}]
                                   :players [{:hand [worldheart-shard]}
                                             {}]}
                                  (play 0 :worldheart-shard)
                                  (choose :gain-2)
                                  (buy-card 0 :jade)
                                  (choose {:player-no 0}))))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:hand [worldheart-shard]}]}
                   (play 0 :worldheart-shard)
                   (choose :gain-2)
                   (buy-card 0 :jade)
                   (choose {:player-no 0}))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:play-area [worldheart-shard]
                           :discard   [jade]}]}))))))

(deftest mist-test
  (testing "Mist (AE)"
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

(deftest mist-we-test
  (testing "Mist (WE)"
    (testing "Exalted Brand"
      (let [spark  (assoc spark :id 1)
            aurora (assoc aurora :id 2)]
        (is (= (-> {:players [{:ability  (assoc exalted-brand :charges 6)
                               :breaches [{:prepped-spells [spark]}]}
                              {:breaches [{:prepped-spells [spark]}
                                          {:prepped-spells [spark]}]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose {:player-no 1 :breach-no 0 :card-name :spark})
                   (choose {:player-no 1})
                   (choose {:player-no 1 :breach-no 1 :card-name :spark})
                   (choose {:player-no 1})
                   (choose {:player-no 0 :breach-no 0 :card-name :spark})
                   (choose {:player-no 1}))
               {:players [{:ability  (assoc exalted-brand :charges 0)
                           :breaches [{}]}
                          {:hand     [spark spark spark]
                           :breaches [{}
                                      {}]}]
                :nemesis {:life 47}}))
        (is (= (-> {:players [{:ability (assoc exalted-brand :charges 6)}
                              {:breaches [{:prepped-spells [aurora]}]}
                              {:breaches [{:prepped-spells [spark]}]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose {:player-no 1 :breach-no 0 :card-name :aurora})
                   (choose {:player-no 2})
                   (choose {:player-no 2 :breach-no 0 :card-name :spark})
                   (choose {:player-no 1}))
               {:players [{:ability (assoc exalted-brand :charges 0)}
                          {:hand     [spark]
                           :breaches [{}]}
                          {:hand     [aurora]
                           :breaches [{}]}]
                :nemesis {:life 46}}))
        (is (= (-> {:players [{:ability  (assoc exalted-brand :charges 6)
                               :breaches [{:prepped-spells [spark]}]}
                              {}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose {:player-no 0 :breach-no 0 :card-name :spark})
                   (choose {:player-no 1}))
               {:players [{:ability  (assoc exalted-brand :charges 0)
                           :breaches [{}]}
                          {:hand [spark]}]
                :nemesis {:life 49}}))
        (is (= (-> {:players [{:ability  (assoc exalted-brand :charges 6)
                               :breaches [{:prepped-spells [spark]}
                                          {:prepped-spells [spark]}
                                          {:prepped-spells [spark]}]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose {:player-no 0 :breach-no 0 :card-name :spark})
                   (choose {:player-no 0})
                   (choose {:player-no 0 :breach-no 1 :card-name :spark})
                   (choose {:player-no 0})
                   (choose {:player-no 0 :breach-no 2 :card-name :spark})
                   (choose {:player-no 0}))
               {:players [{:ability  (assoc exalted-brand :charges 0)
                           :hand     [spark spark spark]
                           :breaches [{} {} {}]}]
                :nemesis {:life 47}}))
        (is (= (-> {:players [{:ability (assoc exalted-brand :charges 6)}
                              {:breaches [{:prepped-spells [radiance]}]
                               :discard  [crystal]}]
                    :nemesis {:life 50}}
                   (activate-ability 0)
                   (choose {:player-no 1 :breach-no 0 :card-name :radiance}))
               {:players [{:ability (assoc exalted-brand :charges 0)}
                          {:breaches [{}]
                           :hand     [crystal]
                           :deck     [radiance]}]
                :nemesis {:life 45}}))))))

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

(deftest reeve-test
  (testing "Reeve"
    (testing "Obsidian Shard"
      (is (= (-> {:players [{:hand [obsidian-shard]
                             :life 10}]}
                 (play 0 :obsidian-shard)
                 (choose nil))
             {:players [{:play-area [obsidian-shard]
                         :aether    1
                         :life      10}]}))
      (is (= (-> {:players [{:hand [obsidian-shard]
                             :life 10}]}
                 (play 0 :obsidian-shard)
                 (choose {:player-no 0}))
             {:players [{:play-area [obsidian-shard]
                         :aether    3
                         :life      9}]})))
    (testing "Quelling Blade"
      (is (= (-> {:players [{:ability (assoc quelling-blade :charges 4)}]
                  :nemesis {:play-area [{:name :minion-1
                                         :type :minion
                                         :life 10}
                                        {:name :minion-2
                                         :type :minion
                                         :life 10}]}}
                 (activate-ability 0)
                 (choose :minion-1)
                 (choose :minion-2))
             {:players [{:ability (assoc quelling-blade :charges 0)}]
              :nemesis {:play-area [{:name :minion-1
                                     :type :minion
                                     :life 5}
                                    {:name :minion-2
                                     :type :minion
                                     :life 7}]}}))
      (is (= (-> {:players [{:ability (assoc quelling-blade :charges 4)}]
                  :nemesis {:play-area [{:name :minion-1
                                         :type :minion
                                         :life 10}]}}
                 (activate-ability 0)
                 (choose :minion-1))
             {:players [{:ability (assoc quelling-blade :charges 0)}]
              :nemesis {:play-area [{:name :minion-1
                                     :type :minion
                                     :life 5}]}}))
      (is (= (-> {:players [{:ability (assoc quelling-blade :charges 4)}]
                  :nemesis {:life 50}}
                 (activate-ability 0))
             {:players [{:ability (assoc quelling-blade :charges 0)}]
              :nemesis {:life 50}}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:players [{:ability (assoc quelling-blade :charges 4)}]
                                 :nemesis {:play-area [{:name :minion-1
                                                        :type :minion
                                                        :life 10}
                                                       {:name :minion-2
                                                        :type :minion
                                                        :life 10}]}}
                                (activate-ability 0)
                                (choose :minion-1)
                                (choose :minion-1))))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:players [{:ability (assoc quelling-blade :charges 4)}]
                                 :nemesis {:life      50
                                           :play-area [{:name :minion-1
                                                        :type :minion
                                                        :life 10}
                                                       {:name :minion-2
                                                        :type :minion
                                                        :life 10}]}}
                                (activate-ability 0)
                                (choose :minion-1)
                                (choose :nemesis)))))))

(deftest sparrow-test
  (testing "Sparrow"
    (testing "Eldritch Tether"
      (is (= (-> {:current-player 0
                  :players        [{:ability  (assoc eldritch-tether :charges 5)
                                    :breaches [{:status :closed
                                                :stage  2}]}
                                   {:breaches [{:status :closed
                                                :stage  2}
                                               {:status :closed
                                                :stage  0}]}]}
                 (activate-ability 0)
                 (choose {:area :breaches :player-no 0 :breach-no 0})
                 (choose {:player-no 0 :breach-no 0})
                 (choose {:player-no 1 :breach-no 1})
                 (choose {:player-no 1 :breach-no 0}))
             {:current-player 0
              :players        [{:ability  (assoc eldritch-tether :charges 0)
                                :breaches [{:status :opened}]}
                               {:breaches [{:status :closed
                                            :stage  3}
                                           {:status :closed
                                            :stage  1}]}]}))
      (is (= (-> {:current-player 0
                  :players        [{:ability  (assoc eldritch-tether :charges 5)
                                    :breaches [{:status :closed
                                                :stage  2}]}
                                   {:breaches [{:status :closed
                                                :stage  3}]}]}
                 (activate-ability 0)
                 (choose {:area :breaches :player-no 0 :breach-no 0})
                 (choose {:player-no 0 :breach-no 0})
                 (choose {:player-no 1 :breach-no 0}))
             {:current-player 0
              :players        [{:ability  (assoc eldritch-tether :charges 0)
                                :breaches [{:status :opened}]}
                               {:breaches [{:status :opened}]}]}))
      (is (= (-> {:players [{:ability  (assoc eldritch-tether :charges 5)
                             :breaches [{:status :closed
                                         :stage  2}]}
                            {:breaches [{:status :opened}
                                        {:status :destroyed}
                                        {:status :opened}
                                        {:status :opened}]
                             :hand     [spark ignite]
                             :deck     [crystal spark spark crystal]}]}
                 (activate-ability 0)
                 (choose {:area :players :player-no 1})
                 (choose :spark)
                 (choose :spark)
                 (choose :ignite))
             {:players [{:ability  (assoc eldritch-tether :charges 0)
                         :breaches [{:status :closed
                                     :stage  2}]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}
                                    {:status :destroyed}
                                    {:status         :opened
                                     :prepped-spells [ignite]}
                                    {:status         :opened
                                     :prepped-spells [spark]}]
                         :hand     [crystal spark]
                         :deck     [crystal]}]}))
      (is (= (-> {:players [{:ability  (assoc eldritch-tether :charges 5)
                             :breaches [{:status :closed
                                         :stage  2}]}
                            {:breaches [{:status :opened}
                                        {:status :destroyed}
                                        {:status :opened}
                                        {:status :opened}]
                             :hand     [spark ignite]
                             :deck     [crystal spark spark crystal]}]}
                 (activate-ability 0)
                 (choose {:area :players :player-no 1})
                 (choose :ignite)
                 (choose nil))
             {:players [{:ability  (assoc eldritch-tether :charges 0)
                         :breaches [{:status :closed
                                     :stage  2}]}
                        {:breaches [{:status :opened}
                                    {:status :destroyed}
                                    {:status :opened}
                                    {:status         :opened
                                     :prepped-spells [ignite]}]
                         :hand     [spark crystal spark spark]
                         :deck     [crystal]}]}))
      (is (= (-> {:players [{:ability  (assoc eldritch-tether :charges 5)
                             :breaches [{:status :closed
                                         :stage  2}]}
                            {:breaches [{:status :opened}
                                        {:status :destroyed}
                                        {:status :opened}
                                        {:status         :opened
                                         :prepped-spells [radiance]}]
                             :hand     [spark ignite]
                             :deck     [crystal spark spark crystal]}]}
                 (activate-ability 0)
                 (choose {:area :players :player-no 1})
                 (choose :ignite)
                 (choose :spark))
             {:players [{:ability  (assoc eldritch-tether :charges 0)
                         :breaches [{:status :closed
                                     :stage  2}]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [ignite]}
                                    {:status :destroyed}
                                    {:status         :opened
                                     :prepped-spells [spark]}
                                    {:status         :opened
                                     :prepped-spells [radiance]}]
                         :hand     [crystal spark spark]
                         :deck     [crystal]}]}))
      (is (= (-> {:players [{:ability  (assoc eldritch-tether :charges 5)
                             :breaches [{:status :closed
                                         :stage  2}]}
                            {:breaches [{:status :opened}
                                        {:status :destroyed}
                                        {:status :opened}
                                        {:status :opened}]
                             :hand     []
                             :deck     [crystal spark spark crystal]}]}
                 (activate-ability 0)
                 (choose {:area :players :player-no 1})
                 (choose :spark)
                 (choose :spark))
             {:players [{:ability  (assoc eldritch-tether :charges 0)
                         :breaches [{:status :closed
                                     :stage  2}]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}
                                    {:status :destroyed}
                                    {:status :opened}
                                    {:status         :opened
                                     :prepped-spells [spark]}]
                         :hand     [crystal]
                         :deck     [crystal]}]})))))

(deftest ulgimor-test
  (testing "Ulgimor"
    (testing "Coal Shard"
      (let [coal-shard (assoc coal-shard :id 1)]
        (is (= (-> {:players [{:hand    [coal-shard]
                               :ability {:charge-cost 6
                                         :charges     0}
                               :life    3}]}
                   (play 0 :coal-shard))
               {:players [{:play-area [coal-shard]
                           :ability   {:charge-cost 6
                                       :charges     1}
                           :life      1
                           :aether    3}]}))
        (is (= (-> {:players [{:hand    [coal-shard]
                               :ability {:charge-cost 6
                                         :charges     0}
                               :life    2}]}
                   (play 0 :coal-shard))
               {:players [{:ability {:charge-cost 6
                                     :charges     0}
                           :life    2}]
                :trash   [coal-shard]}))))
    (testing "Eidolon Shroud"
      (is (= (-> {:players [{:ability (assoc eidolon-shroud :charges 6)
                             :life    3}
                            {:life 3}]}
                 (activate-ability 0))
             {:players [{:ability (assoc eidolon-shroud :charges 0)
                         :life    9}
                        {:life 3}]}))
      (is (= (-> {:players [{:ability (assoc eidolon-shroud :charges 6)
                             :life    0}
                            {:life 3}]}
                 (activate-ability 0)
                 (choose {:player-no 1}))
             {:players [{:ability (assoc eidolon-shroud :charges 0)
                         :life    0}
                        {:life 8}]})))))

(deftest xaxos-test
  (testing "Xaxos"
    (testing "Metaphysical Link"
      (is (= (-> {:players    [{:ability (assoc metaphysical-link :charges 5)}
                               {:ability {:charges     0
                                          :charge-cost 5}}]
                  :turn-order {:deck    [turn-order/player-1
                                         turn-order/player-2
                                         turn-order/nemesis
                                         turn-order/player-2]
                               :discard [turn-order/nemesis
                                         turn-order/player-1]}}
                 (activate-ability 0)
                 (choose {:player-no 1})                    ; gain charge
                 (choose {:player-no 1})                    ; gain charge
                 (choose {:player-no 1})                    ; gain charge
                 (choose {:player-no 1})                    ; gain charge
                 (choose [:player-2 :player-1 :player-2 :nemesis]))
             {:players    [{:ability (assoc metaphysical-link :charges 0)}
                           {:ability {:charges     4
                                      :charge-cost 5}}]
              :turn-order {:deck           [turn-order/player-2
                                            turn-order/player-1
                                            turn-order/player-2
                                            turn-order/nemesis]
                           :discard        [turn-order/nemesis
                                            turn-order/player-1]
                           :revealed-cards 4}}))
      (is (= (-> {:players    [{:ability (assoc metaphysical-link :charges 5)}
                               {:ability {:charges     5
                                          :charge-cost 5}}]
                  :turn-order {:deck    [turn-order/player-2]
                               :discard [turn-order/nemesis
                                         turn-order/player-1
                                         turn-order/player-1
                                         turn-order/player-2
                                         turn-order/nemesis]}}
                 (activate-ability 0)
                 (choose :player-2))
             {:players    [{:ability (assoc metaphysical-link :charges 0)}
                           {:ability {:charges     5
                                      :charge-cost 5}}]
              :turn-order {:deck           [turn-order/player-2]
                           :discard        [turn-order/nemesis
                                            turn-order/player-1
                                            turn-order/player-1
                                            turn-order/player-2
                                            turn-order/nemesis]
                           :revealed-cards 1}}))
      (is (= (-> {:players    [{:ability (assoc metaphysical-link :charges 5)}]
                  :turn-order {:discard [turn-order/nemesis
                                         turn-order/player-1
                                         turn-order/player-1
                                         turn-order/nemesis
                                         turn-order/player-1]}}
                 (activate-ability 0)
                 (choose {:player-no 0})                    ; gain charge
                 (choose {:player-no 0})                    ; gain charge
                 (choose {:player-no 0})                    ; gain charge
                 (choose {:player-no 0})                    ; gain charge)
                 {:players    [{:ability (assoc metaphysical-link :charges 4)}]
                  :turn-order {:discard        [turn-order/nemesis
                                                turn-order/player-1
                                                turn-order/player-1
                                                turn-order/nemesis
                                                turn-order/player-1]
                               :revealed-cards 0}}))))))

(deftest yan-magda-test
  (testing "Yan Magda"
    (testing "Illuminate"
      (is (= (-> {:current-player 0
                  :players        [{:breaches [{:status         :closed
                                                :focus-cost     2
                                                :open-costs     [5 4 3 2]
                                                :stage          0
                                                :prepped-spells [illuminate]}]
                                    :hand     [focusing-orb]}]
                  :nemesis        {:life 50}}
                 (play 0 :focusing-orb)
                 (choose {:player-no 0 :breach-no 0}))
             {:current-player 0
              :players        [{:breaches  [{:status         :focused
                                             :focus-cost     2
                                             :open-costs     [5 4 3 2]
                                             :stage          1
                                             :prepped-spells [illuminate]}]
                                :play-area [focusing-orb]}]
              :nemesis        {:life 49}}))
      (is (= (-> {:current-player 0
                  :players        [{:breaches [{:status         :closed
                                                :focus-cost     2
                                                :open-costs     [5 4 3 2]
                                                :stage          3
                                                :prepped-spells [illuminate]}]
                                    :hand     [focusing-orb]}]
                  :nemesis        {:life 50}}
                 (play 0 :focusing-orb)
                 (choose {:player-no 0 :breach-no 0}))
             {:current-player 0
              :players        [{:breaches  [{:status         :opened
                                             :prepped-spells [illuminate]}]
                                :play-area [focusing-orb]}]
              :nemesis        {:life 49}}))
      (is (= (-> {:current-player 1
                  :players        [{:breaches [{:status         :closed
                                                :focus-cost     2
                                                :open-costs     [5 4 3 2]
                                                :stage          0
                                                :prepped-spells [illuminate]}]}
                                   {:hand [focusing-orb]}]
                  :nemesis        {:life 50}}
                 (play 1 :focusing-orb)
                 (choose {:player-no 0 :breach-no 0}))
             {:current-player 1
              :players        [{:breaches [{:status         :closed
                                            :focus-cost     2
                                            :open-costs     [5 4 3 2]
                                            :stage          1
                                            :prepped-spells [illuminate]}]}
                               {:play-area [focusing-orb]}]
              :nemesis        {:life 50}}))
      (is (= (-> {:current-player 0
                  :players        [{:breaches [{:status         :closed
                                                :focus-cost     2
                                                :open-costs     [5 4 3 2]
                                                :stage          0
                                                :prepped-spells [illuminate]}]
                                    :aether   5}]
                  :nemesis        {:life 50}}
                 (open-breach 0 0))
             {:current-player 0
              :players        [{:breaches [{:status         :opened
                                            :prepped-spells [illuminate]}]
                                :aether   0}]
              :nemesis        {:life 49}})))
    (testing "Imperium Ritual"
      (let [radiance (assoc radiance :id 1)]
        (is (= (-> {:supply  [{:card radiance :pile-size 5}]
                    :players [{:ability (assoc imperium-ritual :charges 5)}]}
                   (activate-ability 0)
                   (choose :radiance))
               {:supply  [{:card radiance :pile-size 4}]
                :players [{:ability (assoc imperium-ritual :charges 0)
                           :discard [radiance]}]}))
        (is (= (-> {:supply  [{:card radiance :pile-size 5}]
                    :players [{:ability  (assoc imperium-ritual :charges 5)
                               :breaches (repeat 4 {:status :opened})}
                              {}]}
                   (activate-ability 0)
                   (choose :radiance)
                   (choose {:player-no 1})
                   (choose :radiance))
               {:supply  [{:card radiance :pile-size 3}]
                :players [{:ability  (assoc imperium-ritual :charges 0)
                           :breaches (repeat 4 {:status :opened})
                           :discard  [radiance]}
                          {:deck           [radiance]
                           :revealed-cards 1}]}))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:supply  [{:card radiance :pile-size 5}]
                                   :players [{:ability  (assoc imperium-ritual :charges 5)
                                              :breaches (repeat 4 {:status :opened})}
                                             {}]}
                                  (activate-ability 0)
                                  (choose :radiance)
                                  (choose {:player-no 0}))))
        (is (= (-> {:supply  [{:card radiance :pile-size 5}]
                    :players [{:ability  (assoc imperium-ritual :charges 5)
                               :breaches (repeat 4 {:status :opened})}]}
                   (activate-ability 0)
                   (choose :radiance)
                   (choose {:player-no 0})
                   (choose :radiance))
               {:supply  [{:card radiance :pile-size 3}]
                :players [{:ability        (assoc imperium-ritual :charges 0)
                           :breaches       (repeat 4 {:status :opened})
                           :discard        [radiance]
                           :deck           [radiance]
                           :revealed-cards 1}]}))))))
