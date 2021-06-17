(ns aeons-end.rageborne-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.rageborne :as rageborne :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]))

(defn do-strike [game]
  (-> game
      (push-effect-stack {:effects [[::rageborne/strike]]})
      check-stack))

(defn do-after-effects [game]
  (-> game
      (push-effect-stack {:effects [[::rageborne/after-effects]]})
      check-stack))

(deftest rageborne-test
  (testing "Rageborne"
    (testing "Unleash"
      (is (= (-> {:nemesis {:fury    0
                            :unleash [[::rageborne/gain-fury]]}}
                 unleash)
             {:nemesis {:fury    1
                        :unleash [[::rageborne/gain-fury]]}})))
    (testing "Strike"
      (is (= (-> {:nemesis   {:fury        4
                              :strike-deck [devastate]}
                  :gravehold {:life 30}}
                 do-strike
                 (choose :devastate))
             {:nemesis   {:fury        1
                          :strike-deck [devastate]
                          :discard     [devastate]}
              :gravehold {:life 25}}))
      (is (= (-> {:nemesis   {:fury        3
                              :strike-deck [devastate]}
                  :gravehold {:life 30}}
                 do-after-effects)
             {:nemesis   {:fury        3
                          :strike-deck [devastate]}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:fury        4
                              :strike-deck [devastate]}
                  :gravehold {:life 30}}
                 do-after-effects
                 (choose :devastate))
             {:nemesis   {:fury        1
                          :strike-deck [devastate]
                          :discard     [devastate]}
              :gravehold {:life 25}}))
      (is (= (-> {:difficulty :expert
                  :nemesis    {:fury        4
                               :strike-deck [devastate]}
                  :gravehold  {:life 30}}
                 do-after-effects
                 (choose :devastate))
             {:difficulty :expert
              :nemesis    {:fury        3
                           :strike-deck [devastate]
                           :discard     [devastate]}
              :gravehold  {:life 25}})))
    (testing "Provoker"
      (is (= (-> {:nemesis   {:fury      0
                              :play-area [provoker]}
                  :gravehold {:life 30}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis   {:fury      0
                          :play-area [provoker]}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:fury      1
                              :play-area [provoker]}
                  :gravehold {:life 30}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis   {:fury      1
                          :play-area [provoker]}
              :gravehold {:life 29}}))
      (is (= (-> {:nemesis   {:fury      4
                              :play-area [provoker]}
                  :gravehold {:life 30}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis   {:fury      4
                          :play-area [provoker]}
              :gravehold {:life 26}})))
    (testing "Invoke Carnage"
      (is (= (-> {:nemesis {:fury      0
                            :play-area [(assoc-in invoke-carnage [:power :power] 1)]}
                  :players [{:life 10}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 0}))
             {:nemesis {:fury    0
                        :discard [(assoc-in invoke-carnage [:power :power] 0)]}
              :players [{:life 9}]}))
      (is (= (-> {:nemesis {:fury      1
                            :play-area [(assoc-in invoke-carnage [:power :power] 1)]}
                  :players [{:life 10}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 0}))
             {:nemesis {:fury    1
                        :discard [(assoc-in invoke-carnage [:power :power] 0)]}
              :players [{:life 8}]}))
      (is (= (-> {:nemesis {:fury      4
                            :play-area [(assoc-in invoke-carnage [:power :power] 1)]}
                  :players [{:life 10}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 0}))
             {:nemesis {:fury    4
                        :discard [(assoc-in invoke-carnage [:power :power] 0)]}
              :players [{:life 5}]})))))
