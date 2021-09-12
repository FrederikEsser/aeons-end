(ns aeons-end.knight-of-shackles-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.knight-of-shackles :as knight-of-shackles :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer []]
            [aeons-end.cards.relic :refer []]
            [aeons-end.cards.spell :refer []]
            [aeons-end.mages :refer []]
            [aeons-end.cards.attack :refer []]
            [aeons-end.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 124 (f)))

(use-fixtures :each fixture)

(defn- open [breach]
  (-> breach
      (assoc :status :opened)
      (dissoc :cost :stage)))

(deftest knight-of-shackles-test
  (testing "Knight of Shackles"
    (testing "Unleash"
      (ut/reset-ids! 0)
      (is (= (-> {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                              :breaches [{:status  :closed
                                          :cost    4
                                          :stage   0
                                          :effects [[:damage-gravehold 1]]}
                                         {:status :closed
                                          :cost   5
                                          :stage  2}]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                          :breaches [{:status  :closed
                                      :cost    4
                                      :stage   1
                                      :effects [[:damage-gravehold 1]]}
                                     {:status :closed
                                      :cost   5
                                      :stage  2}]}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                              :breaches [{:status  :closed
                                          :cost    4
                                          :stage   3
                                          :effects [[:damage-gravehold 1]]}]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                          :breaches [{:status  :opened
                                      :effects [[:damage-gravehold 1]]}]}
              :gravehold {:life 29}}))
      (is (= (-> {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                              :breaches [{:status  :opened
                                          :effects [[:damage-gravehold 1]]}
                                         {:status  :closed
                                          :cost    4
                                          :stage   1
                                          :effects [[:damage-gravehold 2]]}]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                          :breaches [{:status  :opened
                                      :effects [[:damage-gravehold 1]]}
                                     {:status  :closed
                                      :cost    4
                                      :stage   2
                                      :effects [[:damage-gravehold 2]]}]}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                              :breaches [{:status  :opened
                                          :effects [[:damage-gravehold 1]]}
                                         {:status  :closed
                                          :cost    4
                                          :stage   3
                                          :effects [[:damage-gravehold 2]]}]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                          :breaches [{:status  :opened
                                      :effects [[:damage-gravehold 1]]}
                                     {:status  :opened
                                      :effects [[:damage-gravehold 2]]}]}
              :gravehold {:life 28}})))
    (testing "Breaches"
      (is (= (-> {:nemesis {:unleash  [[::knight-of-shackles/unleash]]
                            :breaches [(assoc breach-1 :stage 3)]}
                  :players [{:life 10}
                            {:life 10}]}
                 unleash
                 (choose [{:player-no 0} {:player-no 1}]))
             {:nemesis {:unleash  [[::knight-of-shackles/unleash]]
                        :breaches [(open breach-1)]}
              :players [{:life 8}
                        {:life 8}]}))
      (is (= (-> {:nemesis {:unleash  [[::knight-of-shackles/unleash]]
                            :breaches [(assoc breach-1 :stage 3)]}
                  :players [{:life 10}]}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis {:unleash  [[::knight-of-shackles/unleash]]
                        :breaches [(open breach-1)]}
              :players [{:life 6}]})))))
