(ns aeons-end.gems-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.gems :refer :all]))

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
