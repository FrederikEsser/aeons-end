(ns aeons-end.core-test
  (:require [clojure.test :refer :all]
            [aeons-end.operations :refer :all]
            [aeons-end.cards.common :refer []]
            [aeons-end.setup :refer :all]
            [aeons-end.cards.gems :refer [jade]]))

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
                              (play 0 :crystal))
                          {:players [{:play-area [crystal]
                                      :aether    1
                                      :phase     :main}]}))
    (is (thrown-with-msg? AssertionError #"Play error: You can't play Spell cards"
                          (-> {:players [{:hand [spark]}]}
                              (play 0 :spark))))))

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
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Main phase to the Casting phase"
                            (-> {:players [{:breaches [{:prepped-spells [spark]}]
                                            :phase    :main}]
                                 :nemesis {:life 50}}
                                (cast-spell 0 :spark 0)))))))

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

(deftest buy-charge-test
  (testing "Buy charge"
    (is (= (-> {:players [{:ability {:cost 4}
                           :aether  2
                           :charges 0
                           :phase   :main}]}
               (buy-charge 0))
           {:players [{:ability {:cost 4}
                       :aether  0
                       :charges 1
                       :phase   :main}]}))
    (is (= (-> {:players [{:ability {:cost 4}
                           :aether  2
                           :charges 0
                           :phase   :casting}]}
               (buy-charge 0))
           {:players [{:ability {:cost 4}
                       :aether  0
                       :charges 1
                       :phase   :main}]}))
    (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                          (-> {:players [{:ability {:cost 4}
                                          :aether  2
                                          :charges 0
                                          :phase   :draw}]}
                              (buy-charge 0))))
    (is (= (-> {:players [{:ability {:cost 4}
                           :aether  2
                           :charges 3}]}
               (buy-charge 0))
           {:players [{:ability {:cost 4}
                       :aether  0
                       :charges 4}]}))
    (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 2 aether, when you only have 1"
                          (-> {:players [{:ability {:cost 4}
                                          :aether  1
                                          :charges 0}]}
                              (buy-charge 0))))
    (is (thrown-with-msg? AssertionError #"Charge error: You already have 4 charges"
                          (-> {:players [{:ability {:cost 4}
                                          :aether  2
                                          :charges 4}]}
                              (buy-charge 0))))))

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

(deftest buried-light-test
  (testing "Buried Light"
    (is (= (-> {:players [{:breaches [{:prepped-spells [buried-light]}]
                           :phase    :casting}]
                :nemesis {:life 50}}
               (cast-spell 0 :buried-light 0))
           {:players [{:breaches [{}]
                       :discard  [buried-light]
                       :aether   1
                       :phase    :casting}]
            :nemesis {:life 49}}))))
