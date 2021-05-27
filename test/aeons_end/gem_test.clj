(ns aeons-end.gem-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.gem :refer :all]
            [aeons-end.cards.base :refer [crystal spark]]))

(deftest alien-element-test
  (testing "Pain Stone"
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
