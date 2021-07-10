(ns aeons-end.spell-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.mages :refer [black-mirror]]
            [aeons-end.cards.spell :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.relic :refer [cairn-compass temporal-helix]]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.mages :refer [garnet-shard]])
  (:refer-clojure :exclude [char]))

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

(deftest blaze-test
  (let [blaze (assoc blaze :id 1)]
    (testing "Blaze"
      (testing "Cast"
        (is (= (-> {:real-game? true
                    :players    [{:breaches [{:prepped-spells [blaze]}]}]
                    :nemesis    {:life 50
                                 :deck [{}]}}
                   (cast-spell 0 0 :blaze))
               {:real-game? true
                :players    [{:breaches  [{}]
                              :discard   [blaze]
                              :this-turn [{:cast :blaze}]}]
                :nemesis    {:life 48
                             :deck [{}]}}))
        (is (= (-> {:real-game? true
                    :players    [{:breaches  [{:prepped-spells [blaze]}]
                                  :this-turn [{:cast :blaze}]}]
                    :nemesis    {:life 50
                                 :deck [{}]}}
                   (cast-spell 0 0 :blaze))
               {:real-game? true
                :players    [{:breaches  [{}]
                              :discard   [blaze]
                              :this-turn [{:cast :blaze}
                                          {:cast :blaze}]}]
                :nemesis    {:life 47
                             :deck [{}]}}))
        (is (= (-> {:real-game? true
                    :players    [{:breaches [{:prepped-spells [blaze]}
                                             {:prepped-spells [(assoc blaze :id 2)]}]}]
                    :nemesis    {:life 50
                                 :deck [{}]}}
                   (cast-spell 0 0 :blaze))
               {:real-game? true
                :players    [{:breaches  [{}
                                          {:prepped-spells [(assoc blaze :id 2)]}]
                              :discard   [blaze]
                              :this-turn [{:cast :blaze}]}]
                :nemesis    {:life 47
                             :deck [{}]}}))
        (is (= (-> {:real-game? true
                    :players    [{:breaches [{:prepped-spells [blaze]}]
                                  :hand     [temporal-helix]}]
                    :nemesis    {:life 50
                                 :deck [{}]}}
                   (play 0 :temporal-helix)
                   (choose {:player-no 0 :breach-no 0 :card-name :blaze}))
               {:real-game? true
                :players    [{:breaches  [{:prepped-spells [blaze]}]
                              :play-area [temporal-helix]
                              :this-turn [{:cast :blaze}]}]
                :nemesis    {:life 48
                             :deck [{}]}}))
        (is (= (-> {:real-game? true
                    :players    [{:ability  (assoc black-mirror :charges 4)
                                  :breaches [{:prepped-spells [blaze]}]}]
                    :nemesis    {:life 50
                                 :deck [{}]}}
                   (activate-ability 0)
                   (choose {:player-no 0 :breach-no 0 :card-name :blaze}))
               {:real-game? true
                :players    [{:ability   (assoc black-mirror :charges 0)
                              :breaches  [{}]
                              :discard   [blaze]
                              :this-turn [{:cast :blaze}
                                          {:cast :blaze}]}]
                :nemesis    {:life 45
                             :deck [{}]}}))
        (is (= (-> {:real-game? true
                    :players    [{:breaches  [{:status         :opened
                                               :bonus-damage   1
                                               :prepped-spells [blaze]}
                                              {:prepped-spells [(assoc blaze :id 2)]}]
                                  :this-turn [{:cast :blaze}]}]
                    :nemesis    {:life 50
                                 :deck [{}]}}
                   (cast-spell 0 0 :blaze))
               {:real-game? true
                :players    [{:breaches  [{:status       :opened
                                           :bonus-damage 1}
                                          {:prepped-spells [(assoc blaze :id 2)]}]
                              :discard   [blaze]
                              :this-turn [{:cast :blaze}
                                          {:cast :blaze}]}]
                :nemesis    {:life 45
                             :deck [{}]}})))
      (testing "On gain"
        (is (= (-> {:players [{:aether 4}
                              {}]
                    :supply  [{:card blaze :pile-size 5}]}
                   (buy-card 0 :blaze)
                   (choose {:player-no 0}))
               {:players [{:aether  0
                           :discard [blaze]}
                          {}]
                :supply  [{:card blaze :pile-size 4}]}))
        (is (= (-> {:players [{:aether 4}
                              {}]
                    :supply  [{:card blaze :pile-size 5}]}
                   (buy-card 0 :blaze)
                   (choose {:player-no 1}))
               {:players [{:aether 0}
                          {:discard [blaze]}]
                :supply  [{:card blaze :pile-size 4}]}))
        (is (= (-> {:players [{:aether 4}
                              {}]
                    :supply  [{:card blaze :pile-size 5}]}
                   (buy-card 0 :blaze)
                   (choose nil))
               {:players [{:aether  0
                           :discard [blaze]}
                          {}]
                :supply  [{:card blaze :pile-size 4}]}))))))

(deftest celestial-spire-test
  (testing "Celestial Spire"
    (is (= (-> {:supply  [{:card celestial-spire :pile-size 1}]
                :players [{:breaches [{:prepped-spells [celestial-spire]}]}
                          {:deck [crystal crystal]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :celestial-spire))
           {:supply  [{:card celestial-spire :pile-size 1}]
            :players [{:breaches [{}]
                       :discard  [celestial-spire]}
                      {:deck [crystal crystal]}]
            :nemesis {:life 47}}))
    (is (= (-> {:supply  [{:card celestial-spire :pile-size 0}]
                :players [{:breaches [{:prepped-spells [celestial-spire]}]}
                          {:deck [crystal crystal]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :celestial-spire)
               (choose {:player-no 1}))
           {:supply  [{:card celestial-spire :pile-size 0}]
            :players [{:breaches [{}]
                       :discard  [celestial-spire]}
                      {:hand [crystal]
                       :deck [crystal]}]
            :nemesis {:life 47}}))))

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

(deftest jagged-lightning-test
  (testing "Jagged Lightning"
    (is (= (-> {:current-player 0
                :players        [{:breaches [{:status         :opened
                                              :prepped-spells [jagged-lightning]}
                                             {:status :opened}
                                             {:status     :closed
                                              :focus-cost 3
                                              :stage      1}]}
                                 {:breaches [{:status :opened}
                                             {:status     :closed
                                              :focus-cost 2
                                              :stage      2}]}]
                :nemesis        {:life 50}}
               (cast-spell 0 0 :jagged-lightning))
           {:current-player 0
            :players        [{:breaches [{:status :opened}
                                         {:status :opened}
                                         {:status     :closed
                                          :focus-cost 3
                                          :stage      1}]
                              :discard  [jagged-lightning]}
                             {:breaches [{:status :opened}
                                         {:status     :closed
                                          :focus-cost 2
                                          :stage      2}]}]
            :nemesis        {:life 47}}))
    (is (= (-> {:current-player 0
                :players        [{:breaches [{:status         :opened
                                              :prepped-spells [jagged-lightning]}
                                             {:status :opened}
                                             {:status     :closed
                                              :focus-cost 3
                                              :stage      1}]
                                  :hand     [spark]}
                                 {:breaches [{:status :opened}
                                             {:status     :closed
                                              :focus-cost 2
                                              :stage      2}]}]
                :nemesis        {:life 50}}
               (cast-spell 0 0 :jagged-lightning)
               (choose nil))
           {:current-player 0
            :players        [{:breaches [{:status :opened}
                                         {:status :opened}
                                         {:status     :closed
                                          :focus-cost 3
                                          :stage      1}]
                              :hand     [spark]
                              :discard  [jagged-lightning]}
                             {:breaches [{:status :opened}
                                         {:status     :closed
                                          :focus-cost 2
                                          :stage      2}]}]
            :nemesis        {:life 47}}))
    (is (= (-> {:current-player 0
                :players        [{:breaches [{:status         :opened
                                              :prepped-spells [jagged-lightning]}
                                             {:status :opened}
                                             {:status     :closed
                                              :focus-cost 3
                                              :stage      1}]
                                  :hand     [spark]}
                                 {:breaches [{:status :opened}
                                             {:status     :closed
                                              :focus-cost 2
                                              :stage      2}]}]
                :nemesis        {:life 50}}
               (cast-spell 0 0 :jagged-lightning)
               (choose :spark)
               (choose {:player-no 1 :breach-no 1}))
           {:current-player 0
            :players        [{:breaches [{:status :opened}
                                         {:status :opened}
                                         {:status     :closed
                                          :focus-cost 3
                                          :stage      1}]
                              :discard  [jagged-lightning spark]}
                             {:breaches [{:status :opened}
                                         {:status     :closed
                                          :focus-cost 2
                                          :stage      3}]}]
            :nemesis        {:life 47}}))
    (is (= (-> {:current-player 0
                :players        [{:breaches [{:status         :opened
                                              :prepped-spells [jagged-lightning]}
                                             {:status :opened}
                                             {:status     :closed
                                              :focus-cost 3
                                              :stage      1}]
                                  :hand     [spark]}
                                 {:breaches [{:status :opened}
                                             {:status     :closed
                                              :focus-cost 2
                                              :stage      3}]}]
                :nemesis        {:life 50}}
               (cast-spell 0 0 :jagged-lightning)
               (choose :spark)
               (choose {:player-no 1 :breach-no 1}))
           {:current-player 0
            :players        [{:breaches [{:status :opened}
                                         {:status :opened}
                                         {:status     :closed
                                          :focus-cost 3
                                          :stage      1}]
                              :discard  [jagged-lightning spark]}
                             {:breaches [{:status :opened}
                                         {:status :opened}]}]
            :nemesis        {:life 47}}))
    (is (= (-> {:current-player 0
                :players        [{:breaches [{:status         :opened
                                              :prepped-spells [jagged-lightning]}
                                             {:status :opened}
                                             {:status     :closed
                                              :focus-cost 3
                                              :stage      1}]
                                  :hand     [spark]}
                                 {:breaches [{:status :opened}
                                             {:status     :closed
                                              :focus-cost 2
                                              :stage      2}]}]
                :nemesis        {:life 50}}
               (cast-spell 0 0 :jagged-lightning)
               (choose :spark)
               (choose {:player-no 0 :breach-no 2}))
           {:current-player 0
            :players        [{:breaches [{:status :opened}
                                         {:status :opened}
                                         {:status     :focused
                                          :focus-cost 3
                                          :stage      2}]
                              :discard  [jagged-lightning spark]}
                             {:breaches [{:status :opened}
                                         {:status     :closed
                                          :focus-cost 2
                                          :stage      2}]}]
            :nemesis        {:life 47}}))
    (is (= (-> {:current-player 0
                :players        [{:breaches [{:status         :opened
                                              :prepped-spells [jagged-lightning]}
                                             {:status :opened}]
                                  :hand     [spark]}
                                 {:breaches [{:status :destroyed}
                                             {:status :opened}]}]
                :nemesis        {:life 50}}
               (cast-spell 0 0 :jagged-lightning)
               (choose :spark))
           {:current-player 0
            :players        [{:breaches [{:status :opened}
                                         {:status :opened}]
                              :discard  [jagged-lightning spark]}
                             {:breaches [{:status :destroyed}
                                         {:status :opened}]}]
            :nemesis        {:life 47}}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:current-player 0
                               :players        [{:breaches [{:status         :opened
                                                             :prepped-spells [jagged-lightning]}
                                                            {:status :opened}]
                                                 :hand     [spark]}
                                                {:breaches [{:status :opened}
                                                            {:status :opened}
                                                            {:status     :closed
                                                             :focus-cost 3
                                                             :stage      1}
                                                            {:status     :closed
                                                             :focus-cost 4
                                                             :stage      3}]
                                                 :hand     [spark]}]
                               :nemesis        {:life 50}}
                              (cast-spell 0 0 :jagged-lightning)
                              (choose :spark)
                              (choose {:player-no 1 :breach-no 3}))))))

(deftest nether-conduit-test
  (testing "Nether Conduit"
    (let [nether-conduit (assoc nether-conduit :id 1)]
      (is (= (-> {:supply  [{:card nether-conduit :pile-size 3}]
                  :players [{:breaches [{:prepped-spells [nether-conduit]}]
                             :hand     [nether-conduit]}
                            {}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :nether-conduit)
                 (choose :nether-conduit)
                 (choose {:player-no 1}))
             {:supply  [{:card nether-conduit :pile-size 2}]
              :players [{:breaches [{}]
                         :hand     [nether-conduit]
                         :discard  [nether-conduit]}
                        {:discard [nether-conduit]}]
              :nemesis {:life 48}}))
      (is (= (-> {:supply  [{:card nether-conduit :pile-size 0}]
                  :players [{:breaches [{:prepped-spells [nether-conduit]}]
                             :hand     [nether-conduit]}
                            {}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :nether-conduit)
                 (choose :nether-conduit))
             {:supply  [{:card nether-conduit :pile-size 0}]
              :players [{:breaches [{}]
                         :hand     [nether-conduit]
                         :discard  [nether-conduit]}
                        {}]
              :nemesis {:life 45}}))
      (is (= (-> {:supply  [{:card jade :pile-size 3}]
                  :players [{:breaches [{:prepped-spells [nether-conduit]}]
                             :hand     [jade]}
                            {}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :nether-conduit)
                 (choose :jade)
                 (choose nil))
             {:supply  [{:card jade :pile-size 3}]
              :players [{:breaches [{}]
                         :hand     [jade]
                         :discard  [nether-conduit]}
                        {}]
              :nemesis {:life 46}}))
      (is (= (-> {:players [{:breaches [{:prepped-spells [nether-conduit]}]
                             :hand     [crystal spark]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :nether-conduit))
             {:players [{:breaches [{}]
                         :hand     [crystal spark]
                         :discard  [nether-conduit]}]
              :nemesis {:life 50}}))
      (is (= (-> {:supply  [{:card nether-conduit :pile-size 1}]
                  :players [{:breaches [{:prepped-spells [nether-conduit]}]
                             :hand     [nether-conduit]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :nether-conduit)
                 (choose :nether-conduit)
                 (choose {:player-no 0}))
             {:supply  [{:card nether-conduit :pile-size 0}]
              :players [{:breaches [{}]
                         :hand     [nether-conduit]
                         :discard  [nether-conduit nether-conduit]}]
              :nemesis {:life 46}}))
      (is (= (-> {:supply  [{:card jade :pile-size 1}]
                  :players [{:breaches [{:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [nether-conduit]}]
                             :hand     [jade]}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :nether-conduit)
                 (choose :jade)
                 (choose nil))
             {:supply  [{:card jade :pile-size 1}]
              :players [{:breaches [{:status       :opened
                                     :bonus-damage 1}]
                         :hand     [jade]
                         :discard  [nether-conduit]}]
              :nemesis {:life 43}})))))

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
                     (choose {:player-no 1 :card-id 1}))
                 {:players [{:play-area [cairn-compass]}
                            {:breaches [{:status         :opened
                                         :prepped-spells [nova-forge]}]
                             :phase    :out-of-turn}]}))
          (is (= (-> {:players [{:hand     [cairn-compass]
                                 :discard  [nova-forge]
                                 :breaches [{:status :opened}]
                                 :phase    :main}]}
                     (play 0 :cairn-compass)
                     (choose {:player-no 0 :card-id 1}))
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
               (choose {:area      :nemesis
                        :player-no 0
                        :card-name :nemesis}))
           {:players [{:breaches [{}]
                       :discard  [phoenix-flame]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [phoenix-flame]}]
                           :ability  {:charges 1}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area      :ability
                        :player-no 0}))
           {:players [{:breaches [{}]
                       :ability  {:charges 0}
                       :discard  [phoenix-flame]}]
            :nemesis {:life 46}}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:players [{:breaches [{:prepped-spells [phoenix-flame]}]
                                          :ability  {:charges 0}}]
                               :nemesis {:life 50}}
                              (cast-spell 0 0 :phoenix-flame)
                              (choose {:area      :ability
                                       :player-no 0}))))
    (is (= (-> {:players [{:breaches [{:prepped-spells [phoenix-flame]}]
                           :ability  {:charges 1}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area      :nemesis
                        :player-no 0
                        :card-name :nemesis}))
           {:players [{:breaches [{}]
                       :ability  {:charges 1}
                       :discard  [phoenix-flame]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [phoenix-flame]}]
                           :ability  {:charges 2}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area      :ability
                        :player-no 0}))
           {:players [{:breaches [{}]
                       :ability  {:charges 1}
                       :discard  [phoenix-flame]}]
            :nemesis {:life 46}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [phoenix-flame]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :phoenix-flame)
               (choose {:area      :nemesis
                        :player-no 0
                        :card-name :nemesis}))
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
               (choose {:area      :ability
                        :player-no 0}))
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

(deftest scorch-test
  (testing "Scorch"
    (is (= (-> {:players [{:breaches [{:prepped-spells [scorch]}]}
                          {:ability {:charge-cost 5}}]
                :nemesis {:play-area [{:name :caterpillar
                                       :type :minion
                                       :life 5}]}}
               (cast-spell 0 0 :scorch)
               (choose {:area :minions :player-no 0 :card-name :caterpillar}))
           {:players [{:breaches [{}]
                       :discard  [scorch]}
                      {:ability {:charge-cost 5}}]
            :nemesis {:play-area [{:name :caterpillar
                                   :type :minion
                                   :life 1}]}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [scorch]}]}
                          {:ability {:charges     0
                                     :charge-cost 5}}]
                :nemesis {:play-area [{:name :caterpillar
                                       :type :minion
                                       :life 4}]}}
               (cast-spell 0 0 :scorch)
               (choose {:area :minions :player-no 0 :card-name :caterpillar})
               (choose {:player-no 1}))
           {:players [{:breaches [{}]
                       :discard  [scorch]}
                      {:ability {:charges     2
                                 :charge-cost 5}}]
            :nemesis {:discard [{:name :caterpillar
                                 :type :minion
                                 :life 0}]}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [scorch]}]}
                          {:ability {:charges     4
                                     :charge-cost 5}}]
                :nemesis {:play-area [{:name :caterpillar
                                       :type :minion
                                       :life 4}]}}
               (cast-spell 0 0 :scorch)
               (choose {:area :minions :player-no 0 :card-name :caterpillar})
               (choose {:player-no 1}))
           {:players [{:breaches [{}]
                       :discard  [scorch]}
                      {:ability {:charges     5
                                 :charge-cost 5}}]
            :nemesis {:discard [{:name :caterpillar
                                 :type :minion
                                 :life 0}]}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [scorch]}]
                           :ability  {:charges     0
                                      :charge-cost 5}}
                          {:ability {:charges     5
                                     :charge-cost 5}}]
                :nemesis {:play-area [{:name :caterpillar
                                       :type :minion
                                       :life 4}]}}
               (cast-spell 0 0 :scorch)
               (choose {:area :minions :player-no 0 :card-name :caterpillar}))
           {:players [{:breaches [{}]
                       :discard  [scorch]
                       :ability  {:charges     0
                                  :charge-cost 5}}
                      {:ability {:charges     5
                                 :charge-cost 5}}]
            :nemesis {:discard [{:name :caterpillar
                                 :type :minion
                                 :life 0}]}}))))
