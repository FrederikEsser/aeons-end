(ns aeons-end.magus-of-cloaks-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.magus-of-cloaks :as magus-of-cloaks :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer []]
            [aeons-end.cards.relic :refer []]
            [aeons-end.cards.spell :refer []]))

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
                            :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                            :tokens        4
                            :life          35}}
                 (deal-damage 4))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                        :tokens        3
                        :life          35}}))
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                            :tokens        4
                            :life          35}}
                 (deal-damage 5))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                        :tokens        3
                        :life          34}}))
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                            :tokens        4
                            :life          35}}
                 (deal-damage 6))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                        :tokens        3
                        :life          33}}))
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                            :tokens        3
                            :life          35}}
                 (deal-damage 1))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                        :tokens        2
                        :life          35}}))
      (is (= (-> {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                            :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                            :tokens        2
                            :life          35}}
                 (deal-damage 1))
             {:nemesis {:modify-damage ::magus-of-cloaks/modify-damage
                        :when-hit      [[::magus-of-cloaks/lose-nemesis-token]]
                        :tokens        2
                        :life          35}})))))
