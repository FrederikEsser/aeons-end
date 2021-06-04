(ns aeons-end.relic-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.relic :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [dark-fire radiance]]))

(deftest blasting-staff-test
  (let [spark (assoc spark :id 1)]
    (testing "Blasting Staff"
      (is (= (-> {:players [{:hand     [blasting-staff]
                             :breaches [{:prepped-spells [spark]}]}]}
                 (play 0 :blasting-staff))
             {:players [{:play-area [blasting-staff]
                         :breaches  [{:prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand      [blasting-staff]
                             :breaches  [{:prepped-spells [spark]}]
                             :this-turn [{:gain :spark :id 1}]}]}
                 (play 0 :blasting-staff))
             {:players [{:play-area [blasting-staff]
                         :breaches  [{:prepped-spells [spark]}]
                         :this-turn [{:gain :spark :id 1}]}]}))
      (is (= (-> {:players [{:hand      [blasting-staff]
                             :breaches  [{:prepped-spells [spark]}]
                             :this-turn [{:gain :spark :id 2}]}]}
                 (play 0 :blasting-staff))
             {:players [{:play-area [blasting-staff]
                         :breaches  [{:prepped-spells [spark]}]
                         :this-turn [{:gain :spark :id 2}]}]}))
      (is (= (-> {:players [{:hand      [blasting-staff]
                             :breaches  [{:prepped-spells [spark]}]
                             :this-turn [{:prep :spark :id 1}]}]}
                 (play 0 :blasting-staff)
                 (choose nil))
             {:players [{:play-area [blasting-staff]
                         :breaches  [{:prepped-spells [spark]}]
                         :this-turn [{:prep :spark :id 1}]}]}))
      (is (= (-> {:players [{:hand      [blasting-staff]
                             :breaches  [{:prepped-spells [spark]}]
                             :this-turn [{:prep :spark :id 1}]}]
                  :nemesis {:life 50}}
                 (play 0 :blasting-staff)
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:players [{:play-area [blasting-staff]
                         :discard   [spark]
                         :breaches  [{}]
                         :this-turn [{:prep :spark :id 1}]}]
              :nemesis {:life 47}}))
      (is (= (-> {:players [{:hand      [blasting-staff]
                             :breaches  [{:status         :opened
                                          :bonus-damage   1
                                          :prepped-spells [spark]}]
                             :this-turn [{:prep :spark :id 1}]}]
                  :nemesis {:life 50}}
                 (play 0 :blasting-staff)
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:players [{:play-area [blasting-staff]
                         :discard   [spark]
                         :breaches  [{:status       :opened
                                      :bonus-damage 1}]
                         :this-turn [{:prep :spark :id 1}]}]
              :nemesis {:life 46}})))))

(deftest cairn-compass-test
  (testing "Cairn Compass"
    (let [cairn-compass (assoc cairn-compass :id 1)
          spark         (assoc spark :id 2)]
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}]}]}
                 (play 0 :cairn-compass)
                 (choose nil))
             {:players [{:play-area [cairn-compass]}
                        {:discard  [spark]
                         :breaches [{:status :opened}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2 :card-name :spark}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2 :card-name :spark}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :closed}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2 :card-name :spark}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :closed
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :destroyed}]}]}
                 (play 0 :cairn-compass))
             {:players [{:play-area [cairn-compass]}
                        {:discard  [spark]
                         :breaches [{:status :destroyed}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status         :opened
                                         :prepped-spells [spark]}]}]}
                 (play 0 :cairn-compass))
             {:players [{:play-area [cairn-compass]}
                        {:discard  [spark]
                         :breaches [{:status         :opened
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:breaches [{:status :opened}]}]}
                 (play 0 :cairn-compass))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status :opened}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2 :card-name :spark}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}
                                        {:status :closed}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2 :card-name :spark}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}
                                    {:status :closed}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :closed}
                                        {:status :opened}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2 :card-name :spark}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status :closed}
                                    {:status         :opened
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}
                                        {:status       :closed
                                         :bonus-damage 1}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2 :card-name :spark}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}
                                    {:status       :closed
                                     :bonus-damage 1}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}
                                        {:status       :opened
                                         :bonus-damage 1}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2 :card-name :spark}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status :opened}
                                    {:status         :opened
                                     :bonus-damage   1
                                     :prepped-spells [spark]}]}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:players [{:hand     [cairn-compass]
                                            :discard  [spark]
                                            :breaches [{:status :opened}]}
                                           {:discard  [spark]
                                            :breaches [{:status :opened}]}]}
                                (play 0 :cairn-compass)
                                (choose {:player-no 0 :card-name :spark :card-id 2})))))))

(deftest temporal-helix-test
  (testing "Temporal Helix"
    (let [spark (assoc spark :id 1)]
      (is (= (-> {:nemesis {:life 50}
                  :players [{:breaches [{:prepped-spells [spark]}]
                             :hand     [temporal-helix]}]}
                 (play 0 :temporal-helix)
                 (choose {:player-no 0
                          :breach-no 0
                          :card-name :spark}))
             {:nemesis {:life 49}
              :players [{:breaches  [{:prepped-spells [spark]}]
                         :play-area [temporal-helix]}]}))
      (is (= (-> {:nemesis {:life 50}
                  :players [{:hand [temporal-helix]}]}
                 (play 0 :temporal-helix))
             {:nemesis {:life 50}
              :players [{:play-area [temporal-helix]}]})))
    (let [radiance (assoc radiance :id 1)]
      (is (= (-> {:nemesis {:life 50}
                  :players [{:hand [temporal-helix]}
                            {:breaches [{:prepped-spells [radiance]}]
                             :discard  [crystal]}]}
                 (play 0 :temporal-helix)
                 (choose {:player-no 1
                          :breach-no 0
                          :card-name :radiance}))
             {:nemesis {:life 45}
              :players [{:play-area [temporal-helix]}
                        {:breaches [{:prepped-spells [radiance]}]
                         :hand     [crystal]}]})))))

(deftest unstable-prism-test
  (testing "Unstable Prism"
    (is (= (-> {:players [{:hand [unstable-prism]}]}
               (play 0 :unstable-prism))
           {:players [{:play-area [unstable-prism]
                       :aether    2}]}))
    (is (= (-> {:players [{:hand [unstable-prism crystal]}]}
               (play 0 :unstable-prism)
               (choose []))
           {:players [{:hand      [crystal]
                       :play-area [unstable-prism]
                       :aether    2}]}))
    (is (= (-> {:players [{:hand [unstable-prism crystal]}]}
               (play 0 :unstable-prism)
               (choose :crystal))
           {:players [{:play-area [unstable-prism]
                       :aether    2}]
            :trash   [crystal]}))
    (is (= (-> {:players [{:hand [unstable-prism jade]}]}
               (play 0 :unstable-prism)
               (choose :jade))
           {:players [{:play-area [unstable-prism]
                       :aether    4}]
            :trash   [jade]}))
    (is (= (-> {:players [{:hand [unstable-prism unstable-prism]}]}
               (play 0 :unstable-prism))
           {:players [{:hand      [unstable-prism]
                       :play-area [unstable-prism]
                       :aether    2}]}))))

(deftest vortex-gauntlet-test
  (testing "Vortex Gauntlet"
    (let [spark (assoc spark :id 1)]
      (is (= (-> {:nemesis {:life 50}
                  :players [{:breaches [{:prepped-spells [spark]}]
                             :hand     [vortex-gauntlet]}]}
                 (play 0 :vortex-gauntlet)
                 (choose {:player-no 0
                          :breach-no 0
                          :card-name :spark}))
             {:nemesis {:life 49}
              :players [{:breaches  [{}]
                         :hand      [spark]
                         :play-area [vortex-gauntlet]}]}))
      (is (= (-> {:nemesis {:life 50}
                  :players [{:hand [vortex-gauntlet]}]}
                 (play 0 :vortex-gauntlet))
             {:nemesis {:life 50}
              :players [{:play-area [vortex-gauntlet]}]}))
      (is (= (-> {:nemesis {:life 50}
                  :players [{:hand [vortex-gauntlet]}
                            {:breaches [{:prepped-spells [spark]}]}]}
                 (play 0 :vortex-gauntlet)
                 (choose {:player-no 1
                          :breach-no 0
                          :card-name :spark}))
             {:nemesis {:life 49}
              :players [{:play-area [vortex-gauntlet]}
                        {:breaches [{}]
                         :hand     [spark]}]})))
    (let [dark-fire-1 (assoc dark-fire :id 1)
          dark-fire-2 (assoc dark-fire :id 2)]
      (is (= (-> {:nemesis {:life 50}
                  :players [{:breaches [{:prepped-spells [dark-fire-1]}]
                             :hand     [vortex-gauntlet dark-fire-2 crystal]}]}
                 (play 0 :vortex-gauntlet)
                 (choose {:player-no 0
                          :breach-no 0
                          :card-name :dark-fire})
                 (choose [:crystal :dark-fire]))            ; discard 2 cards to make the cast Dark Fire deal damage
             {:nemesis {:life 44}
              :players [{:breaches  [{}]
                         :hand      [dark-fire-1]
                         :play-area [vortex-gauntlet]
                         :discard   [crystal dark-fire-2]}]})))
    (let [radiance (assoc radiance :id 1)]
      (is (= (-> {:nemesis {:life 50}
                  :players [{:hand [vortex-gauntlet]}
                            {:breaches [{:prepped-spells [radiance]}]
                             :discard  [crystal]}]}
                 (play 0 :vortex-gauntlet)
                 (choose {:player-no 1
                          :breach-no 0
                          :card-name :radiance}))
             {:nemesis {:life 45}
              :players [{:play-area [vortex-gauntlet]}
                        {:breaches [{}]
                         :hand     [crystal]
                         :deck     [radiance]}]})))))
