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
      (is (= (-> {:nemesis {:strike  {:fury 0}
                            :unleash [[::rageborne/gain-fury]]}}
                 unleash)
             {:nemesis {:strike  {:fury 1}
                        :unleash [[::rageborne/gain-fury]]}})))
    (testing "Strike"
      (is (= (-> {:nemesis   {:strike {:fury 4
                                       :deck [devastate]}}
                  :gravehold {:life 30}}
                 do-strike)
             {:nemesis   {:strike {:fury    1
                                   :deck    [devastate]
                                   :discard [devastate]}}
              :gravehold {:life 25}}))
      (is (= (-> {:nemesis   {:strike {:fury 3
                                       :deck [devastate]}}
                  :gravehold {:life 30}}
                 do-after-effects)
             {:nemesis   {:strike {:fury 3
                                   :deck [devastate]}}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:strike {:fury 4
                                       :deck [devastate]}}
                  :gravehold {:life 30}}
                 do-after-effects)
             {:nemesis   {:strike {:fury    1
                                   :deck    [devastate]
                                   :discard [devastate]}}
              :gravehold {:life 25}}))
      (is (= (-> {:difficulty :expert
                  :nemesis    {:strike {:fury 4
                                        :deck [devastate]}}
                  :gravehold  {:life 30}}
                 do-after-effects)
             {:difficulty :expert
              :nemesis    {:strike {:fury    3
                                    :deck    [devastate]
                                    :discard [devastate]}}
              :gravehold  {:life 25}})))
    (testing "Provoker"
      (is (= (-> {:nemesis   {:strike    {:fury 0}
                              :play-area [provoker]}
                  :gravehold {:life 30}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis   {:strike    {:fury 0}
                          :play-area [provoker]}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:strike    {:fury 1}
                              :play-area [provoker]}
                  :gravehold {:life 30}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis   {:strike    {:fury 1}
                          :play-area [provoker]}
              :gravehold {:life 29}}))
      (is (= (-> {:nemesis   {:strike    {:fury 4}
                              :play-area [provoker]}
                  :gravehold {:life 30}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis   {:strike    {:fury 4}
                          :play-area [provoker]}
              :gravehold {:life 26}})))
    (testing "Invoke Carnage"
      (is (= (-> {:nemesis {:strike    {:fury 0}
                            :play-area [(assoc-in invoke-carnage [:power :power] 1)]}
                  :players [{:life 10}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 0}))
             {:nemesis {:strike  {:fury 0}
                        :discard [(assoc-in invoke-carnage [:power :power] 0)]}
              :players [{:life 9}]}))
      (is (= (-> {:nemesis {:strike    {:fury 1}
                            :play-area [(assoc-in invoke-carnage [:power :power] 1)]}
                  :players [{:life 10}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 0}))
             {:nemesis {:strike  {:fury 1}
                        :discard [(assoc-in invoke-carnage [:power :power] 0)]}
              :players [{:life 8}]}))
      (is (= (-> {:nemesis {:strike    {:fury 4}
                            :play-area [(assoc-in invoke-carnage [:power :power] 1)]}
                  :players [{:life 10}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 0}))
             {:nemesis {:strike  {:fury 4}
                        :discard [(assoc-in invoke-carnage [:power :power] 0)]}
              :players [{:life 5}]})))))
