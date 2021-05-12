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
    (is (thrown-with-msg? AssertionError #"Play error: You're in the Draw phase"
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
                 (prep-spell {:player-no  0
                              :breach-no  0
                              :spell-name :spark}))
             {:players [{:breaches [{:status         :opened
                                     :prepped-spells [spark]}]
                         :phase    :main}]}))
      (is (= (-> {:players [{:hand     [spark]
                             :breaches [{:status :opened}]
                             :phase    :casting}]}
                 (prep-spell {:player-no  0
                              :breach-no  0
                              :spell-name :spark}))
             {:players [{:breaches [{:status         :opened
                                     :prepped-spells [spark]}]
                         :phase    :main}]}))
      (is (thrown-with-msg? AssertionError #"Prep error:"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status :opened}]
                                            :phase    :draw}]}
                                (prep-spell {:player-no  0
                                             :breach-no  0
                                             :spell-name :spark}))))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Gem cards"
                            (-> {:players [{:hand     [crystal]
                                            :breaches [{:status :opened}]}]}
                                (prep-spell {:player-no  0
                                             :breach-no  0
                                             :spell-name :crystal}))))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Spark to breach 0 with status Closed"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status :closed}]}]}
                                (prep-spell {:player-no  0
                                             :breach-no  0
                                             :spell-name :spark}))))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Spark to breach 0 which already has prepped spells"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status         :opened
                                                        :prepped-spells [spark]}]}]}
                                (prep-spell {:player-no  0
                                             :breach-no  0
                                             :spell-name :spark})))))
    (testing "Casting"
      (is (= (-> {:players [{:breaches [{:prepped-spells [spark]}]
                             :phase    :casting}]
                  :nemesis {:life 50}}
                 (cast-spell {:player-no  0
                              :breach-no  0
                              :spell-name :spark}))
             {:players [{:breaches [{}]
                         :discard  [spark]
                         :phase    :casting}]
              :nemesis {:life 49}}))
      (is (thrown-with-msg? AssertionError #"Cast error: You can't cast Spark when you're in the Main phase"
                            (-> {:players [{:breaches [{:prepped-spells [spark]}]
                                            :phase    :main}]
                                 :nemesis {:life 50}}
                                (cast-spell {:player-no  0
                                             :breach-no  0
                                             :spell-name :spark})))))))

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
      (is (thrown-with-msg? AssertionError #"Buy error:"
                            (-> {:supply  [{:card jade :pile-size 7}]
                                 :players [{:aether 2
                                            :phase  :draw}]}
                                (buy-card 0 :jade))))
      (is (thrown-with-msg? AssertionError #"Buy error:"
                            (-> {:supply  [{:card jade :pile-size 7}]
                                 :players [{:aether 1
                                            :phase  :main}]}
                                (buy-card 0 :jade))))
      (is (thrown-with-msg? AssertionError #"Buy error:"
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
               (buy-charge {:player-no 0}))
           {:players [{:ability {:cost 4}
                       :aether  0
                       :charges 1
                       :phase   :main}]}))
    (is (= (-> {:players [{:ability {:cost 4}
                           :aether  2
                           :charges 0
                           :phase   :casting}]}
               (buy-charge {:player-no 0}))
           {:players [{:ability {:cost 4}
                       :aether  0
                       :charges 1
                       :phase   :main}]}))
    (is (thrown-with-msg? AssertionError #"Buy-charge error: You're in the Draw phase"
                          (-> {:players [{:ability {:cost 4}
                                          :aether  2
                                          :charges 0
                                          :phase   :draw}]}
                              (buy-charge {:player-no 0}))))
    (is (= (-> {:players [{:ability {:cost 4}
                           :aether  2
                           :charges 3}]}
               (buy-charge {:player-no 0}))
           {:players [{:ability {:cost 4}
                       :aether  0
                       :charges 4}]}))
    (is (thrown-with-msg? AssertionError #"Buy-charge error: You only have 1 aether."
                          (-> {:players [{:ability {:cost 4}
                                          :aether  1
                                          :charges 0}]}
                              (buy-charge {:player-no 0}))))
    (is (thrown-with-msg? AssertionError #"Buy-charge error: You already have 4 charges."
                          (-> {:players [{:ability {:cost 4}
                                          :aether  2
                                          :charges 4}]}
                              (buy-charge {:player-no 0}))))))

(deftest buried-light-test
  (testing "Buried Light"
    (is (= (-> {:players [{:breaches [{:prepped-spells [buried-light]}]
                           :phase    :casting}]
                :nemesis {:life 50}}
               (cast-spell {:player-no  0
                            :breach-no  0
                            :spell-name :buried-light}))
           {:players [{:breaches [{}]
                       :discard  [buried-light]
                       :aether   1
                       :phase    :casting}]
            :nemesis {:life 49}}))))
