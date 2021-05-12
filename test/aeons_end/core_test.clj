(ns aeons-end.core-test
  (:require [clojure.test :refer :all]
            [aeons-end.operations :refer :all]
            [aeons-end.cards.common :refer []]
            [aeons-end.setup :refer :all]))

(deftest gem-test
  (testing "Gems"
    (is (= (-> {:players [{:hand [crystal]}]}
               (play 0 :crystal))
           {:players [{:play-area [crystal]
                       :current   {:aether 1}}]}))
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :casting}]}
               (play 0 :crystal))
           {:players [{:play-area [crystal]
                       :current   {:aether 1}
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
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Gem cards"
                            (-> {:players [{:hand     [crystal]
                                            :breaches [{:status :opened}]
                                            :phase    :main}]}
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
