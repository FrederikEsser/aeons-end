(ns aeons-end.core-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack]]
            [aeons-end.cards.base :refer [crystal spark]]
            [aeons-end.cards.gems :refer [jade]]
            [aeons-end.mages :refer [buried-light]]
            [aeons-end.turn-order :as turn-order]))

(defn fixture [f]
  #_(ut/reset-ids!)
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest gem-test
  (testing "Gems"
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :main}]}
               (play 0 :crystal))
           {:players [{:play-area [crystal]
                       :aether    1
                       :phase     :main}]}))
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :casting}]}
               (play 0 :crystal))
           {:players [{:play-area [crystal]
                       :aether    1
                       :phase     :main}]}))
    (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                          (-> {:players [{:hand  [crystal]
                                          :phase :draw}]}
                              (play 0 :crystal))))
    (is (thrown-with-msg? AssertionError #"Play error: You can't play Spell cards"
                          (-> {:players [{:hand [spark]}]}
                              (play 0 :spark)))))
  (testing "Play all gems"
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :main}]}
               (play-all-gems 0))
           {:players [{:play-area [crystal]
                       :aether    1
                       :phase     :main}]}))
    (is (= (-> {:players [{:hand  [crystal crystal]
                           :phase :casting}]}
               (play-all-gems 0))
           {:players [{:play-area [crystal crystal]
                       :aether    2
                       :phase     :main}]}))
    (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                          (-> {:players [{:hand  [crystal]
                                          :phase :draw}]}
                              (play-all-gems 0))))))

(deftest spell-test
  (testing "Spell"
    (testing "Prepping"
      (is (= (-> {:players [{:hand     [spark]
                             :breaches [{:status :opened}]
                             :phase    :main}]}
                 (prep-spell 0 :spark 0))
             {:players [{:breaches [{:status         :opened
                                     :prepped-spells [spark]}]
                         :phase    :main}]}))
      (is (= (-> {:players [{:hand     [spark]
                             :breaches [{:status :opened}]
                             :phase    :casting}]}
                 (prep-spell 0 :spark 0))
             {:players [{:breaches [{:status         :opened
                                     :prepped-spells [spark]}]
                         :phase    :main}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status :opened}]
                                            :phase    :draw}]}
                                (prep-spell 0 :spark 0))))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Crystal, which has type Gem"
                            (-> {:players [{:hand     [crystal]
                                            :breaches [{:status :opened}]}]}
                                (prep-spell 0 :crystal 0))))
      (is (= (-> {:players [{:hand     [spark]
                             :breaches [{:status :focused}]
                             :phase    :main}]}
                 (prep-spell 0 :spark 0))
             {:players [{:breaches [{:status         :focused
                                     :prepped-spells [spark]}]
                         :phase    :main}]}))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Spark to breach 0 with status Closed"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status :closed}]}]}
                                (prep-spell 0 :spark 0))))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Spark to breach 0 which already has prepped spells"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status         :opened
                                                        :prepped-spells [spark]}]}]}
                                (prep-spell 0 :spark 0)))))
    (testing "Casting"
      (is (= (-> {:players [{:breaches [{:prepped-spells [spark]}]
                             :phase    :casting}]
                  :nemesis {:life 50}}
                 (cast-spell 0 :spark 0))
             {:players [{:breaches [{}]
                         :discard  [spark]
                         :phase    :casting}]
              :nemesis {:life 49}}))
      (is (= (-> {:players [{:breaches [{:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [spark]}]
                             :phase    :casting}]
                  :nemesis {:life 50}}
                 (cast-spell 0 :spark 0))
             {:players [{:breaches [{:status       :opened
                                     :bonus-damage 1}]
                         :discard  [spark]
                         :phase    :casting}]
              :nemesis {:life 48}}))
      (is (= (-> {:players [{:breaches [{:status         :closed
                                         :bonus-damage   1
                                         :prepped-spells [spark]}]
                             :phase    :casting}]
                  :nemesis {:life 50}}
                 (cast-spell 0 :spark 0))
             {:players [{:breaches [{:status       :closed
                                     :bonus-damage 1}]
                         :discard  [spark]
                         :phase    :casting}]
              :nemesis {:life 49}}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Main phase to the Casting phase"
                            (-> {:players [{:breaches [{:prepped-spells [spark]}]
                                            :phase    :main}]
                                 :nemesis {:life 50}}
                                (cast-spell 0 :spark 0))))
      (is (= (-> {:players [{:hand     [crystal]
                             :breaches [{:status         :opened
                                         :prepped-spells [spark]}]
                             :phase    :casting}]}
                 (play 0 :crystal))
             {:players [{:play-area [crystal]
                         :breaches  [{:status         :opened
                                      :prepped-spells [spark]}]
                         :aether    1
                         :phase     :main}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go to the Main phase while you have prepped spells in closed breaches: Spark"
                            (-> {:players [{:hand     [crystal]
                                            :breaches [{:status         :closed
                                                        :prepped-spells [spark]}]
                                            :phase    :casting}]}
                                (play 0 :crystal)))))))

(deftest buy-card-test
  (testing "Buy card"
    (let [jade (assoc jade :id 1)]
      (is (= (-> {:supply  [{:card jade :pile-size 7}]
                  :players [{:aether 2
                             :phase  :main}]}
                 (buy-card 0 :jade))
             {:supply  [{:card jade :pile-size 6}]
              :players [{:discard [jade]
                         :aether  0
                         :phase   :main}]}))
      (is (= (-> {:supply  [{:card jade :pile-size 7}]
                  :players [{:aether 2
                             :phase  :casting}]}
                 (buy-card 0 :jade))
             {:supply  [{:card jade :pile-size 6}]
              :players [{:discard [jade]
                         :aether  0
                         :phase   :main}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                            (-> {:supply  [{:card jade :pile-size 7}]
                                 :players [{:aether 2
                                            :phase  :draw}]}
                                (buy-card 0 :jade))))
      (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 2 aether, when you only have 1"
                            (-> {:supply  [{:card jade :pile-size 7}]
                                 :players [{:aether 1
                                            :phase  :main}]}
                                (buy-card 0 :jade))))
      (is (thrown-with-msg? AssertionError #"Buy error: Jade supply is empty"
                            (-> {:supply  [{:card jade :pile-size 0}]
                                 :players [{:aether 2
                                            :phase  :main}]}
                                (buy-card 0 :jade)))))))

(deftest ability-test
  (testing "Ability"
    (testing "Charge"
      (is (= (-> {:players [{:ability {:charges     0
                                       :charge-cost 4}
                             :aether  2
                             :phase   :main}]}
                 (charge-ability 0))
             {:players [{:ability {:charges     1
                                   :charge-cost 4}
                         :aether  0
                         :phase   :main}]}))
      (is (= (-> {:players [{:ability {:charges     0
                                       :charge-cost 4}
                             :aether  2
                             :phase   :casting}]}
                 (charge-ability 0))
             {:players [{:ability {:charges     1
                                   :charge-cost 4}
                         :aether  0
                         :phase   :main}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                            (-> {:players [{:ability {:charges     0
                                                      :charge-cost 4}
                                            :aether  2
                                            :phase   :draw}]}
                                (charge-ability 0))))
      (is (= (-> {:players [{:ability {:charges     3
                                       :charge-cost 4}
                             :aether  2}]}
                 (charge-ability 0))
             {:players [{:ability {:charges     4
                                   :charge-cost 4}
                         :aether  0}]}))
      (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 2 aether, when you only have 1"
                            (-> {:players [{:ability {:charges     0
                                                      :charge-cost 4}
                                            :aether  1}]}
                                (charge-ability 0))))
      (is (thrown-with-msg? AssertionError #"Charge error: You already have 4 charges"
                            (-> {:players [{:ability {:charges     4
                                                      :charge-cost 4}
                                            :aether  2}]}
                                (charge-ability 0)))))))

(deftest breach-test
  (testing "Breaches"
    (testing "Focus"
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      0}]
                             :aether   2}]}
                 (focus-breach 0 0))
             {:players [{:breaches [{:status     :focused
                                     :focus-cost 2
                                     :open-costs [5 4 3 2]
                                     :stage      1}]
                         :aether   0}]}))
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      0}]
                             :aether   4}]}
                 (focus-breach 0 0)
                 (focus-breach 0 0))
             {:players [{:breaches [{:status     :focused
                                     :focus-cost 2
                                     :open-costs [5 4 3 2]
                                     :stage      2}]
                         :aether   0}]}))
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      2}]
                             :aether   2}]}
                 (focus-breach 0 0))
             {:players [{:breaches [{:status     :focused
                                     :focus-cost 2
                                     :open-costs [5 4 3 2]
                                     :stage      3}]
                         :aether   0}]}))
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      3}]
                             :aether   2}]}
                 (focus-breach 0 0))
             {:players [{:breaches [{:status :opened}]
                         :aether   0}]}))
      (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 2 aether, when you only have 1"
                            (-> {:players [{:breaches [{:status     :closed
                                                        :focus-cost 2
                                                        :open-costs [5 4 3 2]
                                                        :stage      0}]
                                            :aether   1}]}
                                (focus-breach 0 0))))
      (is (= (-> {:players [{:breaches [{:status :opened}
                                        {:status     :closed
                                         :focus-cost 3
                                         :open-costs [9 7 5 3]
                                         :stage      0}]
                             :aether   3}]}
                 (focus-breach 0 1))
             {:players [{:breaches [{:status :opened}
                                    {:status     :focused
                                     :focus-cost 3
                                     :open-costs [9 7 5 3]
                                     :stage      1}]
                         :aether   0}]})))
    (testing "Open"
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      0}]
                             :aether   5}]}
                 (open-breach 0 0))
             {:players [{:breaches [{:status :opened}]
                         :aether   0}]}))
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      1}]
                             :aether   5}]}
                 (open-breach 0 0))
             {:players [{:breaches [{:status :opened}]
                         :aether   1}]}))
      (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 3 aether, when you only have 2"
                            (-> {:players [{:breaches [{:status     :closed
                                                        :focus-cost 2
                                                        :open-costs [5 4 3 2]
                                                        :stage      2}]
                                            :aether   2}]}
                                (open-breach 0 0))))
      (is (= (-> {:players [{:breaches [{:status :opened}
                                        {:status     :closed
                                         :focus-cost 3
                                         :open-costs [9 7 5 3]
                                         :stage      2}]
                             :aether   5}]}
                 (open-breach 0 1))
             {:players [{:breaches [{:status :opened}
                                    {:status :opened}]
                         :aether   0}]})))))

(deftest discard-test
  (testing "Discard"
    (is (= (-> {:players [{:play-area [crystal jade]
                           :phase     :main}]}
               (discard 0 :jade))
           {:players [{:play-area [crystal]
                       :discard   [jade]
                       :phase     :draw}]}))
    (is (= (-> {:players [{:play-area [crystal]
                           :discard   [jade]
                           :phase     :draw}]}
               (discard 0 :crystal))
           {:players [{:discard [jade crystal]
                       :phase   :draw}]}))
    (is (thrown-with-msg? AssertionError #"Move error: There is no Jade in \[:players 0 :play-area\]"
                          (-> {:players [{:play-area [crystal]
                                          :phase     :main}]}
                              (discard 0 :jade)))))
  (testing "Discard all"
    (is (= (-> {:players [{:play-area [crystal jade]
                           :phase     :main}]}
               (discard-all 0))
           {:players [{:discard [jade crystal]
                       :phase   :draw}]}))
    (is (= (-> {:players [{:play-area [crystal buried-light jade]
                           :phase     :main}]}
               (discard-all 0))
           {:players [{:discard [jade buried-light crystal]
                       :phase   :draw}]}))
    (is (= (-> {:players [{:play-area [jade]
                           :discard   [crystal]
                           :phase     :draw}]}
               (discard-all 0))
           {:players [{:discard [crystal jade]
                       :phase   :draw}]}))
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :casting}]}
               (discard-all 0))
           {:players [{:hand  [crystal]
                       :phase :draw}]}))))

(deftest end-turn-test
  (testing "End turn"
    (is (= (-> {:players [{:deck     [crystal crystal crystal spark spark]
                           :discard  [crystal crystal crystal crystal]
                           :breaches [{:status         :opened
                                       :prepped-spells [spark]}]
                           :ability  {:charges 2}
                           :life     8
                           :aether   1
                           :phase    :draw}]}
               (end-turn 0))
           {:players [{:hand     [crystal crystal crystal spark spark]
                       :discard  [crystal crystal crystal crystal]
                       :breaches [{:status         :opened
                                   :prepped-spells [spark]}]
                       :ability  {:charges 2}
                       :life     8
                       :phase    :out-of-turn}]}))
    (is (= (-> {:players [{:hand    [spark]
                           :deck    [crystal crystal crystal spark spark]
                           :discard [crystal crystal crystal crystal]
                           :phase   :draw}]}
               (end-turn 0))
           {:players [{:hand    [spark crystal crystal crystal spark]
                       :deck    [spark]
                       :discard [crystal crystal crystal crystal]
                       :phase   :out-of-turn}]}))
    (is (= (-> {:players [{:deck    [spark]
                           :discard [crystal crystal crystal spark crystal]
                           :phase   :draw}]}
               (end-turn 0))
           {:players [{:hand  [spark crystal crystal crystal spark]
                       :deck  [crystal]
                       :phase :out-of-turn}]}))
    (is (= (-> {:players [{:hand  [crystal crystal crystal spark crystal spark]
                           :deck  [spark]
                           :phase :draw}]}
               (end-turn 0))
           {:players [{:hand  [crystal crystal crystal spark crystal spark]
                       :deck  [spark]
                       :phase :out-of-turn}]}))
    (is (= (-> {:players [{:breaches [{:status         :focused
                                       :prepped-spells [spark]
                                       :stage          2}]
                           :phase    :draw}]}
               (end-turn 0))
           {:players [{:breaches [{:status         :closed
                                   :prepped-spells [spark]
                                   :stage          2}]
                       :phase    :out-of-turn}]}))
    (is (= (-> {:current-player 0
                :turn-order     {:deck    [turn-order/player-1
                                           turn-order/player-2]
                                 :discard [turn-order/player-0]}
                :players        [{} {} {}]}
               (end-turn 0))
           {:current-player 1
            :turn-order     {:deck    [turn-order/player-2]
                             :discard [turn-order/player-0
                                       turn-order/player-1]}
            :players        [{} {} {}]}))
    (is (= (-> {:current-player 1
                :turn-order     {:deck    []
                                 :discard [turn-order/player-2
                                           turn-order/player-0
                                           turn-order/player-3
                                           turn-order/player-1]}
                :players        [{} {} {} {}]}
               (end-turn 1))
           {:current-player 2
            :turn-order     {:deck    [turn-order/player-0
                                       turn-order/player-1
                                       turn-order/player-3]
                             :discard [turn-order/player-2]}
            :players        [{} {} {} {}]}))))

(deftest draw-nemesis-card-test
  (testing "Draw nemesis card"
    (is (= (-> {:nemesis   {:deck    [{:name        :fugazi
                                       :type        :attack
                                       :immediately [[:unleash]]}]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:discard [{:name        :fugazi
                                   :type        :attack
                                   :immediately [[:unleash]]}]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}}))
    (is (= (-> {:nemesis {:deck [{:name    :iznogood
                                  :type    :power
                                  :power   3
                                  :effects [[:damage-gravehold 1]]}]}}
               draw-nemesis-card)
           {:nemesis {:play-area [{:name    :iznogood
                                   :type    :power
                                   :power   3
                                   :effects [[:damage-gravehold 1]]}]}}))))

(deftest resolve-nemesis-card-test
  (testing "Resolve nemesis card"
    (testing "Power"
      (is (= (-> {:nemesis {:play-area [{:name  :iznogood
                                         :type  :power
                                         :power {:power   3
                                                 :effects [[:damage-gravehold 1]]}}]}}
                 resolve-nemesis-cards-in-play)
             {:nemesis {:play-area [{:name  :iznogood
                                     :type  :power
                                     :power {:power   2
                                             :effects [[:damage-gravehold 1]]}}]}}))
      (is (= (-> {:nemesis {:play-area [{:name  :iznogood
                                         :type  :power
                                         :power {:power   2
                                                 :effects [[:damage-gravehold 1]]}}]}}
                 resolve-nemesis-cards-in-play)
             {:nemesis {:play-area [{:name  :iznogood
                                     :type  :power
                                     :power {:power   1
                                             :effects [[:damage-gravehold 1]]}}]}}))
      (is (= (-> {:nemesis   {:play-area [{:name  :iznogood
                                           :type  :power
                                           :power {:power   1
                                                   :effects [[:damage-gravehold 1]]}}]}
                  :gravehold {:life 30}}
                 resolve-nemesis-cards-in-play)
             {:nemesis   {:discard [{:name  :iznogood
                                     :type  :power
                                     :power {:power   0
                                             :effects [[:damage-gravehold 1]]}}]}
              :gravehold {:life 29}})))))