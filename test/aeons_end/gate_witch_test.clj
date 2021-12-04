(ns aeons-end.gate-witch-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.gate-witch :as gate-witch :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.turn-order :as turn-order]))

(defn do-accelerate-time [game]
  (-> game
      (push-effect-stack {:effects [[::gate-witch/accelerate-time]]})
      check-stack))

(defn do-after-effects [game]
  (-> game
      (push-effect-stack {:effects [[::gate-witch/at-end-turn]]})
      check-stack))

(deftest gate-witch-test
  (testing "gate-witch"
    (testing "Unleash"
      (is (= (-> {:nemesis {:time-gates 0
                            :unleash    [[::gate-witch/open-gate]]}}
                 unleash)
             {:nemesis {:time-gates 1
                        :unleash    [[::gate-witch/open-gate]]}})))
    (testing "Accelerate time"
      (is (= (-> {:nemesis    {:time-gates 5}
                  :turn-order {:discard [turn-order/nemesis]}}
                 do-accelerate-time
                 (choose :nemesis))
             {:nemesis    {:time-gates 1}
              :turn-order {:deck [turn-order/nemesis]}}))
      (is (= (-> {:nemesis    {:time-gates 4}
                  :turn-order {:discard [turn-order/nemesis]}}
                 do-after-effects)
             {:nemesis    {:time-gates 4}
              :turn-order {:discard [turn-order/nemesis]}}))
      (is (= (-> {:nemesis    {:time-gates 5}
                  :turn-order {:deck    [turn-order/nemesis]
                               :discard [turn-order/nemesis]}}
                 do-after-effects
                 (choose :nemesis))
             {:nemesis    {:time-gates 1}
              :turn-order {:deck [turn-order/nemesis
                                  turn-order/nemesis]}}))
      (is (= (-> {:nemesis    {:time-gates 5}
                  :turn-order {:discard [turn-order/nemesis
                                         turn-order/nemesis]}}
                 do-after-effects)
             {:nemesis    {:time-gates 5}
              :turn-order {:discard [turn-order/nemesis
                                     turn-order/nemesis]}}))
      (is (= (-> {:difficulty :expert
                  :nemesis    {:time-gates 6}
                  :turn-order {:deck    [turn-order/nemesis]
                               :discard [turn-order/nemesis]}}
                 do-after-effects
                 (choose :nemesis))
             {:difficulty :expert
              :nemesis    {:time-gates 3}
              :turn-order {:deck [turn-order/nemesis
                                  turn-order/nemesis]}})))))
