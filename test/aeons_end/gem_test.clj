(ns aeons-end.gem-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.gem :refer :all]
            [aeons-end.cards.starter :refer [crystal spark]]))

(deftest alien-element-test
  (testing "Alien Element"
    (is (= (-> {:players [{:breaches [{:status :opened}]
                           :hand     [alien-element]}]}
               (play 0 :alien-element))
           {:players [{:breaches  [{:status :opened}]
                       :play-area [alien-element]
                       :aether    1}]}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [spark]}]
                           :hand     [alien-element]}]}
               (play 0 :alien-element))
           {:players [{:breaches  [{:prepped-spells [spark]}]
                       :play-area [alien-element]
                       :aether    2}]}))
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
               (choose {:breach-no 1}))
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
               (choose {:breach-no 1}))
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
