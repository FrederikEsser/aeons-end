(ns aeons-end.nemeses-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.common]
            [aeons-end.nemeses :as nemeses :refer [cryptid grubber]]
            [aeons-end.cards.base :refer :all]
            [aeons-end.cards.gems :refer [jade]]
            [aeons-end.cards.spells :refer [ignite]]
            [aeons-end.cards.nemesis :refer :all]
            [aeons-end.turn-order :as turn-order]))

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

(deftest howling-spinners-test
  (testing "Howling Spinners"
    (is (= (-> {:nemesis {:play-area [howling-spinners]}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [howling-spinners]}
            :players [{:life 8}]}))))

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

(deftest night-unending-test
  (testing "Night Unending"
    (is (= (-> {:nemesis   {:play-area [(assoc-in night-unending [:power :power] 1)]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in night-unending [:power :power] 0)]}
            :gravehold {:life 28}
            :players   [{:breaches [{:prepped-spells [spark]}]}]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in night-unending [:power :power] 1)]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]}
                            {:breaches [{:prepped-spells [spark spark]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells []}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in night-unending [:power :power] 0)]}
            :gravehold {:life 24}
            :players   [{:breaches [{:prepped-spells [spark]}]}
                        {:breaches [{:prepped-spells [spark spark]}
                                    {:prepped-spells [spark]}
                                    {:prepped-spells []}]}]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in night-unending [:power :power] 1)]}
                :gravehold {:life 30}
                :players   [{:breaches [{}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in night-unending [:power :power] 0)]}
            :gravehold {:life 30}
            :players   [{:breaches [{}]}]}))))

(deftest planar-collision-test
  (testing "Planar Collision"
    (is (= (-> {:nemesis {:play-area [(assoc-in planar-collision [:power :power] 1)]}
                :players [{:breaches [{:prepped-spells [spark]}
                                      {:prepped-spells [spark]}]}]}
               (discard-power-card 0 :planar-collision)
               (choose [{:player-no 0
                         :breach-no 0
                         :card-name :spark}
                        {:player-no 0
                         :breach-no 1
                         :card-name :spark}]))
           {:nemesis {:discard [(assoc-in planar-collision [:power :power] 1)]}
            :players [{:breaches [{}
                                  {}]
                       :discard  [spark spark]}]}))
    (is (thrown-with-msg? AssertionError #"Resolve TO DISCARD error:"
                          (-> {:nemesis {:play-area [(assoc-in planar-collision [:power :power] 1)]}
                               :players [{:breaches [{:prepped-spells [spark]}]}]}
                              (discard-power-card 0 :planar-collision))))
    (is (thrown-with-msg? AssertionError #"Resolve TO DISCARD error:"
                          (-> {:nemesis {:play-area [(assoc-in planar-collision [:power :power] 1)]}
                               :players [{:breaches [{:prepped-spells [spark]}
                                                     {:prepped-spells [spark]}]}
                                         {}]}
                              (discard-power-card 1 :planar-collision))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [(assoc-in planar-collision [:power :power] 1)]}
                               :players [{:breaches [{:prepped-spells [spark]}
                                                     {:prepped-spells [spark]}]}]}
                              (discard-power-card 0 :planar-collision)
                              (choose [{:player-no 1
                                        :breach-no 0
                                        :card-name :spark}]))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [(assoc-in planar-collision [:power :power] 1)]}
                               :players [{:breaches [{:prepped-spells [spark]}
                                                     {:prepped-spells [spark]}]}
                                         {:breaches [{:prepped-spells [spark]}]}]}
                              (discard-power-card 0 :planar-collision)
                              (choose [{:player-no 0
                                        :breach-no 0
                                        :card-name :spark}
                                       {:player-no 1
                                        :breach-no 0
                                        :card-name :spark}]))))
    (is (= (-> {:nemesis   {:play-area [(assoc-in planar-collision [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in planar-collision [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}}))))

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
              :gravehold  {:life 28}})))
    (testing "Cryptid"
      (is (= (-> {:nemesis {:play-area [cryptid]
                            :tokens    8}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis {:play-area [cryptid]
                        :tokens    7}}))
      (is (= (-> {:nemesis {:play-area [cryptid]
                            :tokens    8}
                  :players [{:breaches [{:prepped-spells [spark]}]}]}
                 (resolve-nemesis-cards-in-play)
                 (choose nil))
             {:nemesis {:play-area [cryptid]
                        :tokens    7}
              :players [{:breaches [{:prepped-spells [spark]}]}]}))
      (is (= (-> {:nemesis {:play-area [cryptid]
                            :tokens    8}
                  :players [{:breaches [{:prepped-spells [spark]}]}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:nemesis {:play-area [cryptid]
                        :tokens    8}
              :players [{:breaches [{}]
                         :discard  [spark]}]}))
      (is (= (-> {:nemesis {:play-area [cryptid]
                            :tokens    8}
                  :players [{:breaches [{:prepped-spells [spark]}]}
                            {:breaches [{:prepped-spells [ignite]}]}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 1 :breach-no 0 :card-name :ignite}))
             {:nemesis {:play-area [cryptid]
                        :tokens    8}
              :players [{:breaches [{:prepped-spells [spark]}]}
                        {:breaches [{}]
                         :discard  [ignite]}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis {:play-area [cryptid]
                                           :tokens    8}
                                 :players [{:breaches [{:prepped-spells [spark]}]}
                                           {:breaches [{:prepped-spells [ignite]}]}]}
                                (resolve-nemesis-cards-in-play)
                                (choose {:player-no 0 :breach-no 0 :card-name :spark}))))
      (is (= (-> {:nemesis {:play-area [cryptid]
                            :tokens    8}
                  :players [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}
                            {:breaches [{:prepped-spells [ignite]}]}]}
                 (resolve-nemesis-cards-in-play)
                 (choose {:player-no 0 :breach-no 1 :card-name :ignite}))
             {:nemesis {:play-area [cryptid]
                        :tokens    8}
              :players [{:breaches [{:prepped-spells [spark]}
                                    {}]
                         :discard  [ignite]}
                        {:breaches [{:prepped-spells [ignite]}]}]})))
    (testing "Grubber"
      (is (= (-> {:nemesis    {:play-area [grubber]
                               :tokens    8}
                  :gravehold  {:life 30}
                  :turn-order {:discard [turn-order/nemesis]}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis    {:play-area [grubber]
                           :tokens    8}
              :gravehold  {:life 28}
              :turn-order {:discard [turn-order/nemesis]}}))
      (is (= (-> {:nemesis    {:play-area [grubber]
                               :tokens    8}
                  :gravehold  {:life 30}
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}}
                 (resolve-nemesis-cards-in-play))
             {:nemesis    {:play-area [grubber]
                           :tokens    7}
              :gravehold  {:life 30}
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}})))))
