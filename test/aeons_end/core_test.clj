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
                       :current   {:aether 1}}]}))))
