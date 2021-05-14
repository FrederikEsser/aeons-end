(ns aeons-end.mages-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.mages :refer :all]))

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