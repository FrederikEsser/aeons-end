(ns aeons-end.gem-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.gem :refer :all]
            [aeons-end.cards.relic :refer [unstable-prism]]
            [aeons-end.cards.spell :refer [pyrotechnic-surge]]
            [aeons-end.cards.starter :refer [crystal spark]]))

(deftest alien-element-test
  (testing "Alien Element"
    (is (= (-> {:players [{:breaches [{:status :opened}]
                           :hand     [alien-element]}]}
               (play 0 :alien-element))
           {:players [{:breaches  [{:status :opened}]
                       :play-area [alien-element]
                       :aether    1}]}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [spark]}
                                      {:status :closed}]
                           :hand     [alien-element]}]}
               (play 0 :alien-element))
           {:players [{:breaches  [{:prepped-spells [spark]}
                                   {:status :closed}]
                       :play-area [alien-element]
                       :aether    2}]}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [pyrotechnic-surge]}
                                      {:status :closed}]
                           :hand     [alien-element]}]}
               (play 0 :alien-element))
           {:players [{:breaches  [{:prepped-spells [pyrotechnic-surge]}
                                   {:status :closed}]
                       :play-area [alien-element]
                       :aether    3}]}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [spark spark]}
                                      {:status         :closed
                                       :prepped-spells [spark]}]
                           :hand     [alien-element]}]}
               (play 0 :alien-element))
           {:players [{:breaches  [{:prepped-spells [spark spark]}
                                   {:status         :closed
                                    :prepped-spells [spark]}]
                       :play-area [alien-element]
                       :aether    3}]}))))

(deftest bloodstone-jewel-test
  (testing "Bloodstone Jewel"
    (let [bloodstone-jewel (assoc bloodstone-jewel :id 1)]
      (is (= (-> {:real-game? true
                  :players    [{:aether 6}]
                  :supply     [{:card bloodstone-jewel :pile-size 7}]}
                 (buy-card 0 :bloodstone-jewel))
             {:real-game? true
              :players    [{:aether    3
                            :discard   [bloodstone-jewel]
                            :this-turn [{:gain :bloodstone-jewel}]}]
              :supply     [{:card bloodstone-jewel :pile-size 6}]}))
      (is (= (-> {:real-game? true
                  :players    [{:aether    6
                                :discard   [bloodstone-jewel]
                                :this-turn [{:gain :bloodstone-jewel}]}]
                  :supply     [{:card bloodstone-jewel :pile-size 6}]}
                 (buy-card 0 :bloodstone-jewel))
             {:real-game? true
              :players    [{:discard   [bloodstone-jewel
                                        bloodstone-jewel]
                            :aether    0
                            :this-turn [{:gain :bloodstone-jewel}
                                        {:gain :bloodstone-jewel}]}]
              :supply     [{:card bloodstone-jewel :pile-size 5}]})))))

(deftest breach-ore-test
  (testing "Breach Ore"
    (is (= (-> {:players [{:hand     [breach-ore]
                           :breaches [{:status :opened}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      0}]}]}
               (play 0 :breach-ore)
               (choose nil))
           {:players [{:play-area [breach-ore]
                       :aether    2
                       :breaches  [{:status :opened}
                                   {:status     :closed
                                    :focus-cost 2
                                    :stage      0}]}]}))
    (is (= (-> {:players [{:hand     [breach-ore]
                           :breaches [{:status :opened}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      0}]}]}
               (play 0 :breach-ore)
               (choose {:player-no 0
                        :breach-no 1}))
           {:players [{:play-area [breach-ore]
                       :breaches  [{:status :opened}
                                   {:status     :focused
                                    :focus-cost 2
                                    :stage      1}]}]}))
    (is (= (-> {:players [{:hand     [breach-ore]
                           :breaches [{:status :opened}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      3}]}]}
               (play 0 :breach-ore)
               (choose {:player-no 0
                        :breach-no 1}))
           {:players [{:play-area [breach-ore]
                       :breaches  [{:status :opened}
                                   {:status :opened}]}]}))
    (is (= (-> {:players [{:hand     [breach-ore]
                           :breaches [{:status :opened}
                                      {:status :opened}]}]}
               (play 0 :breach-ore))
           {:players [{:play-area [breach-ore]
                       :breaches  [{:status :opened}
                                   {:status :opened}]
                       :aether    2}]}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:players [{:hand     [breach-ore]
                                          :breaches [{:status :opened}
                                                     {:status     :closed
                                                      :focus-cost 2
                                                      :stage      0}
                                                     {:status     :closed
                                                      :focus-cost 3
                                                      :stage      3}]}]}
                              (play 0 :breach-ore)
                              (choose {:breach-no 2}))))))

(deftest burning-opal-test
  (testing "Burning Opal"
    (is (= (-> {:players [{:hand [burning-opal crystal]}
                          {:deck [crystal]}]}
               (play-all-gems 0)
               (choose :crystal)
               (choose {:player-no 1}))
           {:players [{:play-area [burning-opal]
                       :discard   [crystal]
                       :aether    3}
                      {:hand [crystal]}]}))))

(deftest diamond-cluster-test
  (testing "Diamond Cluster"
    (is (= (-> {:real-game? true
                :players    [{:hand [diamond-cluster]}]}
               (play 0 :diamond-cluster))
           {:real-game? true
            :players    [{:play-area [diamond-cluster]
                          :aether    2
                          :this-turn [{:play :diamond-cluster}]}]}))
    (is (= (-> {:real-game? true
                :players    [{:hand [diamond-cluster diamond-cluster]}]}
               (play 0 :diamond-cluster)
               (play 0 :diamond-cluster))
           {:real-game? true
            :players    [{:play-area [diamond-cluster diamond-cluster]
                          :aether    6
                          :this-turn [{:play :diamond-cluster}
                                      {:play :diamond-cluster}]}]}))
    (is (= (-> {:real-game? true
                :players    [{:hand [diamond-cluster diamond-cluster]}]}
               (play-all-gems 0))
           {:real-game? true
            :players    [{:play-area [diamond-cluster diamond-cluster]
                          :aether    6
                          :this-turn [{:play :diamond-cluster}
                                      {:play :diamond-cluster}]}]}))
    (is (= (-> {:real-game? true
                :players    [{:hand [diamond-cluster diamond-cluster diamond-cluster]}]}
               (play 0 :diamond-cluster)
               (play 0 :diamond-cluster)
               (play 0 :diamond-cluster))
           {:real-game? true
            :players    [{:play-area [diamond-cluster diamond-cluster diamond-cluster]
                          :aether    8
                          :this-turn [{:play :diamond-cluster}
                                      {:play :diamond-cluster}
                                      {:play :diamond-cluster}]}]}))
    (is (= (-> {:real-game? true
                :players    [{:hand [unstable-prism diamond-cluster]}]}
               (play 0 :unstable-prism)
               (choose :diamond-cluster))
           {:real-game? true
            :players    [{:play-area [unstable-prism]
                          :aether    6
                          :this-turn [{:play :unstable-prism}
                                      {:play :diamond-cluster}
                                      {:play :diamond-cluster}]}]
            :trash      [diamond-cluster]}))))

(deftest haunted-berylite-test
  (testing "Haunted Berylite"
    (is (= (-> {:players [{:hand    [haunted-berylite crystal]
                           :ability {:charges     0
                                     :charge-cost 5}}]}
               (play 0 :haunted-berylite)
               (choose :crystal))
           {:players [{:play-area [haunted-berylite]
                       :discard   [crystal]
                       :ability   {:charges     2
                                   :charge-cost 5}}]}))
    (is (= (-> {:players [{:hand [haunted-berylite crystal]}]}
               (play 0 :haunted-berylite)
               (choose nil))
           {:players [{:hand      [crystal]
                       :play-area [haunted-berylite]
                       :aether    2}]}))))

(deftest pain-stone-test
  (testing "Pain Stone"
    (is (= (-> {:players [{:hand [pain-stone]}]
                :nemesis {:life 50}}
               (play 0 :pain-stone)
               (choose :aether))
           {:players [{:play-area [pain-stone]
                       :aether    3}]
            :nemesis {:life 50}}))
    (is (= (-> {:players [{:hand [pain-stone]}]
                :nemesis {:life 50}}
               (play 0 :pain-stone)
               (choose :damage))
           {:players [{:play-area [pain-stone]
                       :aether    2}]
            :nemesis {:life 49}}))))

(deftest searing-ruby-test
  (testing "Searing Ruby"
    (is (= (-> {:players [{:hand [searing-ruby]}]}
               (play 0 :searing-ruby))
           {:players [{:play-area        [searing-ruby]
                       :aether           2
                       :earmarked-aether {#{:spell} 1}}]}))
    (is (= (-> {:players [{:hand [searing-ruby searing-ruby]}]}
               (play 0 :searing-ruby)
               (play 0 :searing-ruby))
           {:players [{:play-area        [searing-ruby searing-ruby]
                       :aether           4
                       :earmarked-aether {#{:spell} 2}}]}))))

(deftest sifters-pearl-test
  (let [sifters-pearl (assoc sifters-pearl :id 1)]
    (testing "Sifter's Pearl"
      (is (= (-> {:players [{:hand [sifters-pearl]
                             :deck [crystal spark]}]}
                 (play 0 :sifter's-pearl)
                 (choose nil))
             {:players [{:play-area      [sifters-pearl]
                         :aether         2
                         :deck           [crystal spark]
                         :revealed-cards 1}]}))
      (is (= (-> {:players [{:hand [sifters-pearl]
                             :deck [crystal spark]}]}
                 (play 0 :sifter's-pearl)
                 (choose :crystal))
             {:players [{:play-area [sifters-pearl]
                         :aether    2
                         :deck      [spark]
                         :discard   [crystal]}]}))
      (is (= (-> {:players [{:hand    [sifters-pearl]
                             :discard [crystal crystal]}]}
                 (play 0 :sifter's-pearl)
                 (choose :crystal))
             {:players [{:play-area [sifters-pearl]
                         :aether    2
                         :deck      [crystal]
                         :discard   [crystal]}]}))
      (is (= (-> {:players [{:hand [sifters-pearl]}]}
                 (play 0 :sifter's-pearl))
             {:players [{:play-area [sifters-pearl]
                         :aether    2}]}))
      (is (= (-> {:players [{:hand [sifters-pearl]
                             :deck [crystal spark]}
                            {:deck [crystal spark]}]}
                 (play 0 :sifter's-pearl)
                 (choose nil)
                 (choose :crystal))
             {:players [{:play-area      [sifters-pearl]
                         :aether         2
                         :deck           [crystal spark]
                         :revealed-cards 1}
                        {:deck    [spark]
                         :discard [crystal]}]})))))

(deftest volcanic-glass-test
  (testing "Volcanic Glass"
    (let [volcanic-glass (assoc volcanic-glass :id 1)]
      (is (= (-> {:current-player 0
                  :players        [{:aether 5}
                                   {:deck [spark]}]
                  :supply         [{:card volcanic-glass :pile-size 7}]}
                 (buy-card 0 :volcanic-glass)
                 (choose nil))
             {:current-player 0
              :players        [{:discard [volcanic-glass]
                                :aether  2}
                               {:deck [spark]}]
              :supply         [{:card volcanic-glass :pile-size 6}]}))
      (is (= (-> {:current-player 0
                  :players        [{:aether 5}
                                   {:deck [spark]}]
                  :supply         [{:card volcanic-glass :pile-size 7}]}
                 (buy-card 0 :volcanic-glass)
                 (choose {:player-no 1}))
             {:current-player 0
              :players        [{:discard [volcanic-glass]
                                :aether  0}
                               {:deck           [volcanic-glass spark]
                                :revealed-cards 1}]
              :supply         [{:card volcanic-glass :pile-size 5}]}))
      (is (= (-> {:current-player 0
                  :players        [{:aether 4}
                                   {:deck [spark]}]
                  :supply         [{:card volcanic-glass :pile-size 7}]}
                 (buy-card 0 :volcanic-glass))
             {:current-player 0
              :players        [{:discard [volcanic-glass]
                                :aether  1}
                               {:deck [spark]}]
              :supply         [{:card volcanic-glass :pile-size 6}]}))
      (is (= (-> {:current-player 0
                  :players        [{:aether 5}
                                   {:deck [spark]}]
                  :supply         [{:card volcanic-glass :pile-size 1}]}
                 (buy-card 0 :volcanic-glass))
             {:current-player 0
              :players        [{:discard [volcanic-glass]
                                :aether  2}
                               {:deck [spark]}]
              :supply         [{:card volcanic-glass :pile-size 0}]}))
      (is (= (-> {:current-player 0
                  :players        [{:aether 5}]
                  :supply         [{:card volcanic-glass :pile-size 7}]}
                 (buy-card 0 :volcanic-glass)
                 (choose {:player-no 0}))
             {:current-player 0
              :players        [{:deck           [volcanic-glass]
                                :discard        [volcanic-glass]
                                :revealed-cards 1
                                :aether         0}]
              :supply         [{:card volcanic-glass :pile-size 5}]}))
      (is (= (-> {:current-player 0
                  :players        [{:aether 5}]
                  :supply         [{:card volcanic-glass :pile-size 7}]}
                 (buy-card 0 :volcanic-glass)
                 (choose nil))
             {:current-player 0
              :players        [{:discard [volcanic-glass]
                                :aether  2}]
              :supply         [{:card volcanic-glass :pile-size 6}]}))
      (is (= (-> {:current-player 0
                  :players        [{:aether 7}]
                  :supply         [{:card volcanic-glass :pile-size 7}]}
                 (buy-card 0 :volcanic-glass)
                 (choose {:player-no 0})
                 (choose {:player-no 0}))
             {:current-player 0
              :players        [{:deck           [volcanic-glass volcanic-glass]
                                :discard        [volcanic-glass]
                                :revealed-cards 2
                                :aether         0}]
              :supply         [{:card volcanic-glass :pile-size 4}]})))))

(deftest vriswood-amber-test
  (let [vriswood-amber (assoc vriswood-amber :id 1)]
    (testing "V'riswood Amber"
      (is (= (-> {:players [{:aether 3
                             :deck   [spark]}]
                  :supply  [{:card vriswood-amber :pile-size 7}]}
                 (buy-card 0 :v'riswood-amber)
                 (choose nil))
             {:players [{:aether  0
                         :deck    [spark]
                         :discard [vriswood-amber]}]
              :supply  [{:card vriswood-amber :pile-size 6}]}))
      (is (= (-> {:players [{:aether 3
                             :deck   [spark]}]
                  :supply  [{:card vriswood-amber :pile-size 7}]}
                 (buy-card 0 :v'riswood-amber)
                 (choose :v'riswood-amber))
             {:players [{:aether         0
                         :deck           [vriswood-amber spark]
                         :revealed-cards 1}]
              :supply  [{:card vriswood-amber :pile-size 6}]})))))
