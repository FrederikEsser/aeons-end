(ns aeons-end.relic-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.relic :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [dark-fire radiance]]
            [aeons-end.turn-order :as turn-order]))

(deftest astral-cube-test
  (testing "Astral Cube"
    (is (= (-> {:players    [{:hand      [astral-cube]
                              :play-area [crystal]
                              :life      8}]
                :turn-order {:deck [turn-order/player-1]}}
               (play 0 :astral-cube)
               (choose :crystal))
           {:players    [{:hand      [crystal]
                          :play-area [astral-cube]
                          :life      9}]
            :turn-order {:deck           [turn-order/player-1]
                         :revealed-cards 1}}))
    (is (= (-> {:players    [{:hand      [astral-cube]
                              :play-area [astral-cube]
                              :life      8}]
                :turn-order {:deck [turn-order/nemesis]}}
               (play 0 :astral-cube))
           {:players    [{:play-area [astral-cube astral-cube]
                          :life      8}]
            :turn-order {:deck           [turn-order/nemesis]
                         :revealed-cards 1}}))
    (is (= (-> {:players    [{:hand      [astral-cube]
                              :play-area [jade]
                              :life      10}]
                :turn-order {:deck [turn-order/player-1]}}
               (play 0 :astral-cube)
               (choose :jade))
           {:players    [{:hand      [jade]
                          :play-area [astral-cube]
                          :life      10}]
            :turn-order {:deck           [turn-order/player-1]
                         :revealed-cards 1}}))
    (is (= (-> {:players    [{:hand      [astral-cube]
                              :play-area [jade]
                              :life      8}
                             {:life 5}]
                :turn-order {:deck [turn-order/player-2]}}
               (play 0 :astral-cube)
               (choose :jade))
           {:players    [{:hand      [jade]
                          :play-area [astral-cube]
                          :life      8}
                         {:life 6}]
            :turn-order {:deck           [turn-order/player-2]
                         :revealed-cards 1}}))
    (is (= (-> {:players    [{:hand [astral-cube]
                              :life 8}
                             {:life 0}]
                :turn-order {:deck [turn-order/player-2]}}
               (play 0 :astral-cube))
           {:players    [{:play-area [astral-cube]
                          :life      8}
                         {:life 0}]
            :turn-order {:deck           [turn-order/player-2]
                         :revealed-cards 1}}))
    (is (= (-> {:players    [{:hand [astral-cube]
                              :life 0}]
                :turn-order {:deck [turn-order/player-1]}}
               (play 0 :astral-cube))
           {:players    [{:play-area [astral-cube]
                          :life      0}]
            :turn-order {:deck           [turn-order/player-1]
                         :revealed-cards 1}}))
    (is (= (-> {:players    [{:hand [astral-cube]
                              :life 8}
                             {:life 0}
                             {:life 1}]
                :turn-order {:deck [turn-order/wild]}}
               (play 0 :astral-cube)
               (choose {:player-no 2}))
           {:players    [{:play-area [astral-cube]
                          :life      8}
                         {:life 0}
                         {:life 2}]
            :turn-order {:deck           [turn-order/wild]
                         :revealed-cards 1}}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:players    [{:hand [astral-cube]
                                             :life 8}
                                            {:life 0}
                                            {:life 1}]
                               :turn-order {:deck [turn-order/wild]}}
                              (play 0 :astral-cube)
                              (choose {:player-no 1}))))))

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
                 (choose {:player-no 1 :card-id 2}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :closed}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2}))
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
                 (choose {:player-no 1 :card-id 2}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}
                                        {:status :closed}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}
                                    {:status :closed}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :closed}
                                        {:status :opened}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2}))
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
                 (choose {:player-no 1 :card-id 2}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status         :opened
                                     :prepped-spells [spark]}
                                    {:status       :closed
                                     :bonus-damage 1}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :focused}
                                        {:status :closed}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2}))
             {:players [{:play-area [cairn-compass]}
                        {:breaches [{:status :focused}
                                    {:status         :closed
                                     :prepped-spells [spark]}]}]}))
      (is (= (-> {:players [{:hand [cairn-compass]}
                            {:discard  [spark]
                             :breaches [{:status :opened}
                                        {:status       :opened
                                         :bonus-damage 1}]}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 1 :card-id 2}))
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
                                (choose {:player-no 0 :card-id 2})))))))

(deftest eternity-charm-test
  (testing "Eternity Charm"
    (let [spark     (assoc spark :id 1)
          dark-fire (assoc dark-fire :id 2)]
      (is (= (-> {:players [{:hand     [eternity-charm]
                             :deck     [dark-fire crystal spark]
                             :breaches [{:status     :closed
                                         :stage      1
                                         :focus-cost 2}]}]}
                 (play 0 :eternity-charm)
                 (choose :dark-fire))
             {:players [{:play-area      [eternity-charm]
                         :deck           [crystal spark]
                         :breaches       [{:status         :focused
                                           :stage          2
                                           :focus-cost     2
                                           :prepped-spells [dark-fire]}]
                         :revealed-cards 2}]})))
    (is (= (-> {:players [{:hand     [eternity-charm]
                           :deck     [dark-fire crystal spark]
                           :breaches [{:status     :closed
                                       :stage      1
                                       :focus-cost 2}]}]}
               (play 0 :eternity-charm)
               (choose :spark))
           {:players [{:play-area      [eternity-charm]
                       :deck           [dark-fire crystal]
                       :breaches       [{:status         :focused
                                         :stage          2
                                         :focus-cost     2
                                         :prepped-spells [spark]}]
                       :revealed-cards 2}]}))
    (is (= (-> {:players [{:hand     [eternity-charm]
                           :deck     [spark spark crystal]
                           :breaches [{:status     :closed
                                       :stage      1
                                       :focus-cost 2}]}]}
               (play 0 :eternity-charm)
               (choose nil))
           {:players [{:play-area      [eternity-charm]
                       :deck           [spark spark crystal]
                       :breaches       [{:status     :focused
                                         :stage      2
                                         :focus-cost 2}]
                       :revealed-cards 3}]}))
    (is (= (-> {:players [{:hand     [eternity-charm]
                           :deck     [crystal crystal jade spark]
                           :breaches [{:status     :closed
                                       :stage      1
                                       :focus-cost 2}]}]}
               (play 0 :eternity-charm))
           {:players [{:play-area      [eternity-charm]
                       :deck           [crystal crystal jade spark]
                       :breaches       [{:status     :focused
                                         :stage      2
                                         :focus-cost 2}]
                       :revealed-cards 3}]}))))

(deftest fiend-catcher-test
  (testing "Fiend Catcher"
    (let [fiend-catcher (assoc fiend-catcher :id 1)
          spark         (assoc spark :id 2)]
      (is (= (-> {:players    [{:hand    [fiend-catcher crystal]
                                :discard [spark]}]
                  :turn-order {:deck [turn-order/player-1]}}
                 (play 0 :fiend-catcher)
                 (choose {:area :discard :player-no 0 :card-id 2}))
             {:players    [{:hand      [crystal]
                            :play-area [fiend-catcher]}]
              :turn-order {:deck           [turn-order/player-1]
                           :revealed-cards 1}
              :trash      [spark]}))
      (is (= (-> {:players    [{:hand    [fiend-catcher crystal]
                                :discard [spark]}]
                  :turn-order {:deck [turn-order/player-1]}}
                 (play 0 :fiend-catcher)
                 (choose {:area :hand :player-no 0 :card-name :crystal}))
             {:players    [{:discard   [spark]
                            :play-area [fiend-catcher]}]
              :turn-order {:deck           [turn-order/player-1]
                           :revealed-cards 1}
              :trash      [crystal]}))
      (is (= (-> {:players    [{:hand    [fiend-catcher crystal]
                                :discard [spark]}]
                  :turn-order {:deck [turn-order/player-1]}}
                 (play 0 :fiend-catcher)
                 (choose nil))
             {:players    [{:hand      [crystal]
                            :discard   [spark]
                            :play-area [fiend-catcher]}]
              :turn-order {:deck           [turn-order/player-1]
                           :revealed-cards 1}}))
      (is (= (-> {:players    [{:hand [fiend-catcher]}]
                  :turn-order {:deck [turn-order/nemesis
                                      turn-order/player-1]}}
                 (play 0 :fiend-catcher)
                 (choose :nemesis))
             {:players    [{:play-area [fiend-catcher]}]
              :turn-order {:deck [turn-order/player-1
                                  turn-order/nemesis]}}))
      (is (= (-> {:players    [{:hand [fiend-catcher]}]
                  :turn-order {:deck [turn-order/nemesis
                                      turn-order/player-1]}}
                 (play 0 :fiend-catcher)
                 (choose nil))
             {:players    [{:play-area [fiend-catcher]}]
              :turn-order {:deck           [turn-order/nemesis
                                            turn-order/player-1]
                           :revealed-cards 1}}))
      (is (= (-> {:players    [{:hand    [fiend-catcher crystal]
                                :discard [spark
                                          crystal
                                          (assoc spark :id 3)]}]
                  :turn-order {:deck [turn-order/player-1]}}
                 (play 0 :fiend-catcher)
                 (choose {:area :discard :player-no 0 :card-id 2}))
             {:players    [{:hand      [crystal]
                            :play-area [fiend-catcher]
                            :discard   [crystal
                                        (assoc spark :id 3)]}]
              :turn-order {:deck           [turn-order/player-1]
                           :revealed-cards 1}
              :trash      [spark]})))))

(deftest flexing-dagger-test
  (testing "Flexing Dagger"
    (let [flexing-dagger (assoc flexing-dagger :id 1)]
      (is (= (-> {:players [{:hand [flexing-dagger]}]
                  :nemesis {:life 50}}
                 (play 0 :flexing-dagger)
                 (choose :flexing-dagger))
             {:players [{}]
              :nemesis {:life 49}
              :trash   [flexing-dagger]}))
      (is (= (-> {:players [{:hand [flexing-dagger]}]
                  :nemesis {:life 50}}
                 (play 0 :flexing-dagger)
                 (choose nil))
             {:players [{:play-area             [flexing-dagger]
                         :breach-cost-reduction 3}]
              :nemesis {:life 50}}))
      (is (= (-> {:players [{:hand                  [flexing-dagger]
                             :breach-cost-reduction 3}]
                  :nemesis {:life 50}}
                 (play 0 :flexing-dagger)
                 (choose nil))
             {:players [{:play-area             [flexing-dagger]
                         :breach-cost-reduction 6}]
              :nemesis {:life 50}})))))

(deftest mages-totem-test
  (testing "Mage's Totem"
    (let [mages-totem (assoc mages-totem :id 1)]
      (is (= (-> {:players [{:hand      [mages-totem]
                             :play-area [crystal]}]}
                 (play 0 :mage's-totem)
                 (choose :crystal))
             {:players [{:play-area [mages-totem]}]
              :trash   [crystal]}))
      (is (= (-> {:players   [{:hand      [mages-totem]
                               :play-area [crystal]}]
                  :gravehold {:life 20}}
                 (play 0 :mage's-totem)
                 (choose :mage's-totem))
             {:players   [{:play-area [crystal]}]
              :gravehold {:life 21}
              :trash     [mages-totem]}))
      (is (= (-> {:players   [{:hand      [mages-totem]
                               :play-area [crystal]}]
                  :gravehold {:life 30}}
                 (play 0 :mage's-totem)
                 (choose :mage's-totem))
             {:players   [{:play-area [crystal]}]
              :gravehold {:life 30}
              :trash     [mages-totem]})))))

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

(deftest transmogrifier-test
  (testing "Transmogrifier"
    (let [transmogrifier  (assoc transmogrifier :id 1)
          unstable-prism  (assoc unstable-prism :id 2)
          vortex-gauntlet (assoc vortex-gauntlet :id 3)]
      (is (= (-> {:supply  [{:card unstable-prism :pile-size 5}]
                  :players [{:hand [transmogrifier crystal]}]}
                 (play 0 :transmogrifier)
                 (choose :crystal)
                 (choose :unstable-prism))
             {:supply  [{:card unstable-prism :pile-size 4}]
              :players [{:play-area [transmogrifier]
                         :discard   [unstable-prism]}]
              :trash   [crystal]}))
      (is (= (-> {:supply  [{:card vortex-gauntlet :pile-size 5}]
                  :players [{:hand [transmogrifier transmogrifier]}]}
                 (play 0 :transmogrifier)
                 (choose :transmogrifier)
                 (choose :vortex-gauntlet))
             {:supply  [{:card vortex-gauntlet :pile-size 4}]
              :players [{:play-area [transmogrifier]
                         :discard   [vortex-gauntlet]}]
              :trash   [transmogrifier]}))
      (is (= (-> {:supply  [{:card transmogrifier :pile-size 4}]
                  :players [{:hand [transmogrifier crystal]}]}
                 (play 0 :transmogrifier)
                 (choose :crystal))
             {:supply  [{:card transmogrifier :pile-size 4}]
              :players [{:play-area [transmogrifier]}]
              :trash   [crystal]}))
      (is (= (-> {:supply  [{:card unstable-prism :pile-size 5}]
                  :players [{:hand [transmogrifier crystal]}]}
                 (play 0 :transmogrifier)
                 (choose :crystal)
                 (choose nil))
             {:supply  [{:card unstable-prism :pile-size 5}]
              :players [{:play-area [transmogrifier]}]
              :trash   [crystal]}))
      (is (= (-> {:supply  [{:card unstable-prism :pile-size 5}]
                  :players [{:hand [transmogrifier]}]}
                 (play 0 :transmogrifier))
             {:supply  [{:card unstable-prism :pile-size 5}]
              :players [{:play-area [transmogrifier]}]})))))

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
