(ns aeons-end.nemeses-test
  (:require [clojure.test :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.common]
            [aeons-end.nemeses :as nemeses]
            [aeons-end.cards.base :refer :all]
            [aeons-end.cards.gems :refer [jade]]
            [aeons-end.cards.nemesis :refer :all]
            [aeons-end.turn-order :as turn-order]))

(defn unleash [game]
  (-> game
      (push-effect-stack {:effects [[:unleash]]})
      check-stack))

(defn draw-nemesis-card [game]
  (-> game
      (push-effect-stack {:effects [[:draw-nemesis-card]]})
      check-stack))

(deftest afflict-test
  (testing "Afflict"
    (is (= (-> {:nemesis   {:deck    [afflict]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:discard [crystal]
                             :life    10}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose :crystal))
           {:nemesis   {:discard [afflict]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:hand [crystal]
                         :life 7}]}))
    (is (= (-> {:nemesis   {:deck    [afflict]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:discard [crystal]
                             :life    10}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose nil))
           {:nemesis   {:discard [afflict]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:discard [crystal]
                         :life    7}]}))
    (is (= (-> {:nemesis   {:deck    [afflict]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:life 10}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [afflict]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:life 7}]}))))

(deftest nix-test
  (testing "Nix"
    (is (= (-> {:nemesis   {:deck    [nix]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal]
                             :life 10}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose :crystal))
           {:nemesis   {:discard [nix]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:discard [crystal]
                         :life    9}]}))
    (is (= (-> {:nemesis   {:deck    [nix]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal jade]
                             :life 10}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose :jade))
           {:nemesis   {:discard [nix]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:hand    [crystal]
                         :discard [jade]
                         :life    9}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [nix]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:hand [crystal jade]
                                            :life 10}]}
                              draw-nemesis-card
                              (choose {:player-no 0})
                              (choose :crystal))))))

(deftest umbra-titan-test
  (testing "Umbra Titan"
    (testing "Unleash"
      (is (= (-> {:nemesis    {:tokens  8
                               :unleash [[::nemeses/umbra-titan-unleash]]}
                  :turn-order {:discard [turn-order/nemesis]}
                  :players    [{:life 10}]}
                 unleash
                 (choose nil))
             {:nemesis    {:tokens  7
                           :unleash [[::nemeses/umbra-titan-unleash]]}
              :turn-order {:discard [turn-order/nemesis]}
              :players    [{:life 10}]}))
      (is (= (-> {:nemesis    {:tokens  8
                               :unleash [[::nemeses/umbra-titan-unleash]]}
                  :turn-order {:discard [turn-order/player-0 turn-order/nemesis]}
                  :players    [{:life 10}]}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis    {:tokens  8
                           :unleash [[::nemeses/umbra-titan-unleash]]}
              :turn-order {:discard [turn-order/player-0 turn-order/nemesis]}
              :players    [{:life 8}]}))
      (is (= (-> {:nemesis    {:tokens  8
                               :unleash [[::nemeses/umbra-titan-unleash]]}
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
                  :gravehold  {:life 30}}
                 unleash
                 (choose :token))
             {:nemesis    {:tokens  7
                           :unleash [[::nemeses/umbra-titan-unleash]]}
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
              :gravehold  {:life 30}}))
      (is (= (-> {:nemesis    {:tokens  8
                               :unleash [[::nemeses/umbra-titan-unleash]]}
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
                  :gravehold  {:life 30}}
                 unleash
                 (choose :damage))
             {:nemesis    {:tokens  8
                           :unleash [[::nemeses/umbra-titan-unleash]]}
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
              :gravehold  {:life 28}})))))
