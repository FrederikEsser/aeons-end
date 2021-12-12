(ns aeons-end.gate-witch-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.gate-witch :as gate-witch :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.turn-order :as turn-order]
            [aeons-end.mages :refer [terminus-barrier]]))

(defn fixture [f]
  (with-rand-seed 42 (f)))

(use-fixtures :each fixture)

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

(deftest deep-abomination-test
  (testing "Deep Abomination"
    (is (= (-> {:nemesis    {:play-area [deep-abomination]}
                :players    [{:life 10}]
                :turn-order {:discard [turn-order/player-1
                                       turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play))
           {:nemesis    {:play-area [deep-abomination]}
            :players    [{:life 9}]
            :turn-order {:discard [turn-order/player-1
                                   turn-order/nemesis]}}))
    (is (= (-> {:nemesis    {:play-area [deep-abomination]}
                :players    [{:life 10}]
                :turn-order {:deck    [turn-order/player-1]
                             :discard [turn-order/player-1
                                       turn-order/player-1
                                       turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play))
           {:nemesis    {:play-area [deep-abomination]}
            :players    [{:life 8}]
            :turn-order {:deck    [turn-order/player-1]
                         :discard [turn-order/player-1
                                   turn-order/player-1
                                   turn-order/nemesis]}}))
    (is (= (-> {:nemesis    {:play-area [deep-abomination]}
                :players    [{:life 10}
                             {:life 10}]
                :turn-order {:discard [turn-order/player-1
                                       turn-order/nemesis
                                       turn-order/player-2
                                       turn-order/player-1
                                       turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play))
           {:nemesis    {:play-area [deep-abomination]}
            :players    [{:life 8}
                         {:life 9}]
            :turn-order {:discard [turn-order/player-1
                                   turn-order/nemesis
                                   turn-order/player-2
                                   turn-order/player-1
                                   turn-order/nemesis]}}))
    (is (= (-> {:nemesis    {:play-area [deep-abomination]}
                :players    [{:life 2}
                             {:life 1}
                             {:life 3}]
                :turn-order {:deck    [turn-order/player-2]
                             :discard [turn-order/player-1
                                       turn-order/nemesis
                                       turn-order/wild
                                       turn-order/player-3
                                       turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 2}))
           {:nemesis    {:play-area [deep-abomination]}
            :players    [{:life 1}
                         {:life 1}
                         {:life 1}]
            :turn-order {:deck    [turn-order/player-2]
                         :discard [turn-order/player-1
                                   turn-order/nemesis
                                   turn-order/wild
                                   turn-order/player-3
                                   turn-order/nemesis]}}))
    (is (= (-> {:nemesis    {:play-area [deep-abomination]}
                :players    [{:life 2}
                             {:life 1}
                             {:life 3}
                             {:life 10}]
                :turn-order {:discard [turn-order/player-1-2
                                       turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis    {:play-area [deep-abomination]}
            :players    [{:life 1}
                         {:life 1}
                         {:life 3}
                         {:life 10}]
            :turn-order {:discard [turn-order/player-1-2
                                   turn-order/nemesis]}}))
    (is (= (-> {:nemesis    {:play-area [deep-abomination]}
                :players    [{:life 2}
                             {:life 1}
                             {:life 3}
                             {:life 10}]
                :turn-order {:discard [turn-order/player-1-2
                                       turn-order/player-3-4
                                       turn-order/player-3-4
                                       turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis    {:play-area [deep-abomination]}
            :players    [{:life 1}
                         {:life 1}
                         {:life 2}
                         {:life 9}]
            :turn-order {:discard [turn-order/player-1-2
                                   turn-order/player-3-4
                                   turn-order/player-3-4
                                   turn-order/nemesis]}}))
    (is (= (-> {:nemesis    {:play-area [deep-abomination]}
                :players    [{:life 2}
                             {:life 1}
                             {:life 3}
                             {:life 10}]
                :turn-order {:discard [turn-order/player-3-4
                                       turn-order/player-1-2
                                       turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 3})
               (choose {:player-no 0}))
           {:nemesis    {:play-area [deep-abomination]}
            :players    [{:life 1}
                         {:life 1}
                         {:life 3}
                         {:life 9}]
            :turn-order {:discard [turn-order/player-3-4
                                   turn-order/player-1-2
                                   turn-order/nemesis]}}))))

(deftest distort-test
  (testing "distort"
    (is (= (-> {:nemesis    {:deck       [distort deep-abomination]
                             :unleash    [[::gate-witch/open-gate]]
                             :time-gates 1}
                :turn-order {:deck [turn-order/nemesis]}
                :gravehold  {:life 30}}
               draw-nemesis-card)
           {:nemesis    {:deck       [deep-abomination distort]
                         :unleash    [[::gate-witch/open-gate]]
                         :time-gates 2}
            :turn-order {:deck           [turn-order/nemesis]
                         :revealed-cards 1}
            :gravehold  {:life 25}}))
    (is (= (-> {:nemesis    {:deck       [distort deep-abomination]
                             :unleash    [[::gate-witch/open-gate]]
                             :time-gates 1}
                :turn-order {:deck [turn-order/player-1 turn-order/nemesis]}
                :gravehold  {:life 30}}
               draw-nemesis-card)
           {:nemesis    {:deck       [deep-abomination distort]
                         :unleash    [[::gate-witch/open-gate]]
                         :time-gates 2}
            :turn-order {:deck    [turn-order/nemesis]
                         :discard [turn-order/player-1]}
            :gravehold  {:life 30}}))
    (is (= (-> {:nemesis    {:deck       [distort deep-abomination]
                             :unleash    [[::gate-witch/open-gate]]
                             :time-gates 1}
                :turn-order {:discard [turn-order/player-1
                                       turn-order/player-1
                                       turn-order/player-2
                                       turn-order/player-2
                                       turn-order/nemesis
                                       turn-order/nemesis]}
                :gravehold  {:life 30}}
               draw-nemesis-card)
           {:nemesis    {:deck       [deep-abomination distort]
                         :unleash    [[::gate-witch/open-gate]]
                         :time-gates 2}
            :turn-order {:deck    [turn-order/player-1
                                   turn-order/nemesis
                                   turn-order/nemesis
                                   turn-order/player-2
                                   turn-order/player-2]
                         :discard [turn-order/player-1]}
            :gravehold  {:life 30}}))))

(deftest hasten-test
  (testing "Hasten"
    (is (= (-> {:nemesis {:deck       [hasten deep-abomination]
                          :unleash    [[::gate-witch/open-gate]]
                          :time-gates 1}
                :players [{:life 10}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis {:deck       [deep-abomination hasten]
                      :unleash    [[::gate-witch/open-gate]]
                      :time-gates 2}
            :players [{:life 9}]}))
    (is (= (-> {:nemesis {:deck       [hasten]
                          :unleash    [[::gate-witch/open-gate]]
                          :time-gates 3}
                :players [{:life 10}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis {:deck       [hasten]
                      :unleash    [[::gate-witch/open-gate]]
                      :time-gates 4}
            :players [{:life 7}]}))))

(deftest infinite-enmity-test
  (testing "Infinite Enmity"
    (let [infinite-enmity (assoc-in infinite-enmity [:power :start-power] 2)]
      (is (= (-> {:nemesis {:deck       [hasten]
                            :play-area  [(assoc-in infinite-enmity [:power :power] 1)]
                            :time-gates 1}}
                 resolve-nemesis-cards-in-play)
             {:nemesis {:deck       [hasten infinite-enmity]
                        :time-gates 6}}))
      (is (= (-> {:nemesis {:deck       [hasten]
                            :play-area  [(assoc-in infinite-enmity [:power :power] 1)]
                            :time-gates 6}}
                 resolve-nemesis-cards-in-play)
             {:nemesis {:deck       [hasten infinite-enmity]
                        :time-gates 6}}))
      (is (= (-> {:nemesis {:deck       [hasten]
                            :play-area  [(assoc-in infinite-enmity [:power :power] 1)]
                            :time-gates 7}}
                 resolve-nemesis-cards-in-play)
             {:nemesis {:deck       [hasten infinite-enmity]
                        :time-gates 7}})))))

(deftest nether-spiral-test
  (testing "Nether Spiral"
    (is (= (-> {:nemesis    {:play-area [(assoc-in nether-spiral [:power :power] 1)]}
                :turn-order {:deck    [turn-order/player-1
                                       turn-order/player-2
                                       turn-order/player-2
                                       turn-order/nemesis]
                             :discard [turn-order/player-1
                                       turn-order/nemesis]}}
               resolve-nemesis-cards-in-play
               (choose :player-1))
           {:nemesis    {:discard [(assoc-in nether-spiral [:power :power] 0)]}
            :turn-order {:deck    [turn-order/player-2
                                   turn-order/player-2
                                   turn-order/nemesis
                                   turn-order/nemesis
                                   turn-order/player-1]
                         :discard [turn-order/player-1]}}))
    (is (= (-> {:nemesis    {:play-area [(assoc-in nether-spiral [:power :power] 1)]}
                :turn-order {:discard [turn-order/player-1
                                       turn-order/player-1
                                       turn-order/player-2
                                       turn-order/player-2
                                       turn-order/nemesis
                                       turn-order/nemesis]}}
               resolve-nemesis-cards-in-play
               (choose :player-1))
           {:nemesis    {:discard [(assoc-in nether-spiral [:power :power] 0)]}
            :turn-order {:deck    [turn-order/nemesis
                                   turn-order/player-2
                                   turn-order/player-2
                                   turn-order/nemesis
                                   turn-order/player-1]
                         :discard [turn-order/player-1]}}))
    (is (= (-> {:nemesis    {:play-area [(assoc-in nether-spiral [:power :power] 1)]}
                :turn-order {:deck    [turn-order/player-1
                                       turn-order/player-2
                                       turn-order/player-3
                                       turn-order/player-4
                                       turn-order/nemesis]
                             :discard [turn-order/nemesis]}}
               resolve-nemesis-cards-in-play)
           {:nemesis    {:discard [(assoc-in nether-spiral [:power :power] 0)]}
            :turn-order {:deck [turn-order/nemesis
                                turn-order/player-1
                                turn-order/player-3
                                turn-order/nemesis
                                turn-order/player-4
                                turn-order/player-2]}}))
    (is (= (-> {:nemesis    {:play-area [(assoc-in nether-spiral [:power :power] 1)]}
                :turn-order {:deck    [turn-order/player-3
                                       turn-order/player-1
                                       turn-order/nemesis]
                             :discard [turn-order/player-2
                                       turn-order/wild
                                       turn-order/nemesis]}}
               resolve-nemesis-cards-in-play
               (choose :wild))
           {:nemesis    {:discard [(assoc-in nether-spiral [:power :power] 0)]}
            :turn-order {:deck    [turn-order/player-2
                                   turn-order/player-3
                                   turn-order/nemesis
                                   turn-order/player-1
                                   turn-order/nemesis]
                         :discard [turn-order/wild]}}))
    (is (= (-> {:nemesis    {:play-area [(assoc-in nether-spiral [:power :power] 1)]}
                :turn-order {:deck    [turn-order/nemesis]
                             :discard [turn-order/player-1-2
                                       turn-order/player-3-4
                                       turn-order/player-3-4
                                       turn-order/nemesis
                                       turn-order/player-1-2]}}
               resolve-nemesis-cards-in-play
               (choose :player-3-4))
           {:nemesis    {:discard [(assoc-in nether-spiral [:power :power] 0)]}
            :turn-order {:deck    [turn-order/player-1-2
                                   turn-order/player-1-2
                                   turn-order/player-3-4
                                   turn-order/nemesis
                                   turn-order/nemesis]
                         :discard [turn-order/player-3-4]}}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis    {:play-area [(assoc-in nether-spiral [:power :power] 1)]}
                               :turn-order {:deck    [turn-order/player-1
                                                      turn-order/player-2
                                                      turn-order/player-2
                                                      turn-order/nemesis]
                                            :discard [turn-order/player-1
                                                      turn-order/nemesis]}}
                              resolve-nemesis-cards-in-play
                              (choose nil))))))

(deftest rift-scourge-test
  (testing "Rift Scourge"
    (is (= (-> {:nemesis {:deck      [nether-spiral]
                          :play-area [(assoc rift-scourge :life 1
                                                          :max-life 13)]}}
               (deal-damage 1)
               (choose {:area :minions :player-no 0 :card-name :rift-scourge}))
           {:nemesis {:deck [nether-spiral (assoc rift-scourge :max-life 13)]}}))
    (is (= (-> {:players [{:ability (assoc terminus-barrier :charges 5)}]
                :nemesis {:deck [rift-scourge nether-spiral]}}
               (activate-ability 0))
           {:players [{:ability (assoc terminus-barrier :charges 0)}]
            :nemesis {:deck [nether-spiral rift-scourge]}}))
    (is (= (-> {:players [{:ability (assoc terminus-barrier :charges 5)}]
                :nemesis {:deck [distort rift-scourge]}}
               (activate-ability 0))
           {:players [{:ability (assoc terminus-barrier :charges 0)}]
            :nemesis {:deck    [rift-scourge]
                      :discard [distort]}}))))

(deftest temporal-nimbus-test
  (testing "Temporal Nimbus"
    (is (= (-> {:nemesis {:play-area  [(assoc-in temporal-nimbus [:power :power] 1)]
                          :unleash    [[::gate-witch/open-gate]]
                          :time-gates 1}}
               resolve-nemesis-cards-in-play)
           {:nemesis {:discard     [(assoc-in temporal-nimbus [:power :power] 0)]
                      :unleash     [[::gate-witch/open-gate]]
                      :time-gates  2
                      :draw-extra? true}}))
    (is (= (-> {:nemesis {:deck        [deep-abomination]
                          :time-gates  2
                          :draw-extra? true}}
               do-after-effects
               (choose {:area      :play-area
                        :card-name :deep-abomination}))
           {:nemesis {:play-area  [(assoc deep-abomination :max-life 6)]
                      :time-gates 2}}))))
