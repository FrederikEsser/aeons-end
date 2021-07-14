(ns aeons-end.magus-of-cloaks-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.magus-of-cloaks :as magus-of-cloaks :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer []]
            [aeons-end.cards.relic :refer []]
            [aeons-end.cards.spell :refer [ignite]]))

(deftest crooked-mask-test
  (testing "Magus of Cloaks"
    (testing "Unleash"
      (is (= (-> {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                              :tokens  4}
                  :gravehold {:life 30}}
                 unleash
                 (choose :tokens))
             {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                          :tokens  6}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                              :tokens  4}
                  :gravehold {:life 30}}
                 unleash
                 (choose :damage))
             {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                          :tokens  4}
              :gravehold {:life 28}}))
      (is (= (-> {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                              :tokens  6}
                  :gravehold {:life 30}}
                 unleash
                 (choose :tokens))
             {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                          :tokens  8}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                              :tokens  7}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                          :tokens  7}
              :gravehold {:life 28}})))
    (testing "When damaged"
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/when-hit]]
                            :tokens        4
                            :life          35}}
                 (deal-damage 4))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/when-hit]]
                        :tokens        3
                        :life          35}}))
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/when-hit]]
                            :tokens        4
                            :life          35}}
                 (deal-damage 5))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/when-hit]]
                        :tokens        3
                        :life          34}}))
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/when-hit]]
                            :tokens        4
                            :life          35}}
                 (deal-damage 6))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/when-hit]]
                        :tokens        3
                        :life          33}}))
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/when-hit]]
                            :tokens        3
                            :life          35}}
                 (deal-damage 1))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/when-hit]]
                        :tokens        2
                        :life          35}}))
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/when-hit]]
                            :tokens        2
                            :life          35}}
                 (deal-damage 1))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/when-hit]]
                        :tokens        2
                        :life          35}})))))

(deftest rising-dark-test
  (testing "Rising Dark"
    (testing "To Discard"
      (is (= (-> {:nemesis {:play-area     [rising-dark]
                            :modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/when-hit]]
                            :tokens        4
                            :life          30}}
                 (deal-damage 4))
             {:nemesis {:play-area     [rising-dark]
                        :modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/when-hit]]
                        :tokens        3
                        :life          30}}))
      (is (= (-> {:nemesis {:play-area     [rising-dark]
                            :modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/when-hit]]
                            :tokens        3
                            :life          30}}
                 (deal-damage 4))
             {:nemesis {:discard       [rising-dark]
                        :modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/when-hit]]
                        :tokens        2
                        :life          29}})))
    (testing "Power"
      (is (= (-> {:nemesis   {:play-area [(assoc-in rising-dark [:power :power] 1)]
                              :unleash   [[:damage-gravehold 1]]}
                  :gravehold {:life 30}
                  :players   [{:breaches [{:prepped-spells [spark]}]
                               :life     10}]}
                 resolve-nemesis-cards-in-play
                 (choose {:player-no 0})
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:nemesis   {:discard [(assoc-in rising-dark [:power :power] 0)]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 29}
              :players   [{:breaches [{}]
                           :life     7}]
              :trash     [spark]}))
      (is (= (-> {:nemesis   {:play-area [(assoc-in rising-dark [:power :power] 1)]
                              :unleash   [[:damage-gravehold 1]]}
                  :gravehold {:life 30}
                  :players   [{:breaches [{:prepped-spells [ignite]}]
                               :life     10}
                              {:breaches [{}]
                               :life     10}]}
                 resolve-nemesis-cards-in-play
                 (choose {:player-no 1}))
             {:nemesis   {:discard [(assoc-in rising-dark [:power :power] 0)]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 29}
              :players   [{:breaches [{:prepped-spells [ignite]}]
                           :life     10}
                          {:breaches [{}]
                           :life     7}]})))))

(deftest twilight-empire-test
  (testing "Twilight Empire"
    (is (= (-> {:nemesis   {:play-area [(assoc-in twilight-empire [:power :power] 1)]
                            :tokens    4}
                :gravehold {:life 30}}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in twilight-empire [:power :power] 0)]
                        :tokens  5}
            :gravehold {:life 26}}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in twilight-empire [:power :power] 1)]
                            :tokens    8}
                :gravehold {:life 30}}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in twilight-empire [:power :power] 0)]
                        :tokens  8}
            :gravehold {:life 22}}))))