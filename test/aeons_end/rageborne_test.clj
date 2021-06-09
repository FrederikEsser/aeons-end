(ns aeons-end.rageborne-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.rageborne :as rageborne :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]))

(deftest rageborne-test
  (testing "Rageborne"
    (testing "Unleash"
      (is (= (-> {:nemesis {:fury    0
                            :unleash [[::rageborne/gain-fury]]}}
                 unleash)
             {:nemesis {:fury    1
                        :unleash [[::rageborne/gain-fury]]}})))))
