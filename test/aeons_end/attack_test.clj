(ns aeons-end.attack-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.attack :refer :all]
            [aeons-end.cards.minion :refer [mangleroot]]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [amplify-vision dark-fire ignite]]
            [aeons-end.turn-order :as turn-order]))

(defn fixture [f]
  (with-rand-seed 42 (f)))

(use-fixtures :each fixture)

(deftest afflict-test
  (testing "Afflict"
    (let [crystal (assoc crystal :id 1)]
      (is (= (-> {:nemesis   {:deck    [afflict]
                              :unleash [[:damage-gravehold 1]]}
                  :gravehold {:life 30}
                  :players   [{:discard [crystal]
                               :life    10}]}
                 draw-nemesis-card
                 (choose {:player-no 0})
                 (choose {:player-no 0 :card-id 1}))
             {:nemesis   {:discard [afflict]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 29}
              :players   [{:hand [crystal]
                           :life 7}]})))
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

(deftest assail-test
  (testing "Assail"
    (is (= (-> {:nemesis   {:deck    [assail]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]
                             :deck     [crystal]}]}
               draw-nemesis-card
               (choose {:player-no 0 :breach-no 0 :card-name :spark}))
           {:nemesis   {:discard [assail]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches       [{}]
                         :deck           [spark crystal]
                         :revealed-cards 1}]}))
    (is (= (-> {:nemesis   {:deck    [assail]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]
                             :deck     [crystal]}
                            {:breaches [{:prepped-spells [amplify-vision]}]
                             :deck     [crystal]}]}
               draw-nemesis-card
               (choose {:player-no 0 :breach-no 1 :card-name :ignite}))
           {:nemesis   {:discard [assail]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches       [{:prepped-spells [spark]}
                                          {}]
                         :deck           [ignite crystal]
                         :revealed-cards 1}
                        {:breaches [{:prepped-spells [amplify-vision]}]
                         :deck     [crystal]}]}))
    (is (= (-> {:nemesis   {:deck    [assail]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]
                             :deck     [crystal]}
                            {:breaches [{:prepped-spells [amplify-vision]}]
                             :deck     [crystal]}]}
               draw-nemesis-card
               (choose {:player-no 1 :breach-no 0 :card-name :amplify-vision}))
           {:nemesis   {:discard [assail]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{:prepped-spells [spark]}
                                    {:prepped-spells [ignite]}]
                         :deck     [crystal]}
                        {:breaches       [{}]
                         :deck           [amplify-vision crystal]
                         :revealed-cards 1}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [assail]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:breaches [{:prepped-spells [spark]}
                                                       {:prepped-spells [ignite]}]
                                            :deck     [crystal]}
                                           {:breaches [{:prepped-spells [amplify-vision]}]
                                            :deck     [crystal]}]}
                              draw-nemesis-card
                              (choose {:player-no 0 :breach-no 0 :card-name :spark}))))))

(deftest banish-test
  (testing "Banish"
    (is (= (-> {:nemesis   {:deck    [banish]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{}]
                             :life     10}
                            {:breaches [{}]
                             :life     10}]}
               draw-nemesis-card)
           {:nemesis   {:discard [banish]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{}]
                         :life     10}
                        {:breaches [{}]
                         :life     10}]}))
    (is (= (-> {:nemesis   {:deck    [banish]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]
                             :life     10}
                            {:breaches [{}]
                             :life     10}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [banish]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{:prepped-spells [spark]}]
                         :life     9}
                        {:breaches [{}]
                         :life     10}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [banish]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:breaches [{:prepped-spells [spark]}]
                                            :life     10}
                                           {:breaches [{}]
                                            :life     10}]}
                              draw-nemesis-card
                              (choose {:player-no 1}))))
    (is (= (-> {:nemesis   {:deck    [banish]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark spark]}]
                             :life     10}
                            {:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]
                             :life     10}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [banish]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{:prepped-spells [spark spark]}]
                         :life     8}
                        {:breaches [{:prepped-spells [spark]}
                                    {:prepped-spells [spark]}]
                         :life     10}]}))
    (is (= (-> {:nemesis   {:deck    [banish]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark spark]}]
                             :life     10}
                            {:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]
                             :life     10}]}
               draw-nemesis-card
               (choose {:player-no 1}))
           {:nemesis   {:discard [banish]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{:prepped-spells [spark spark]}]
                         :life     10}
                        {:breaches [{:prepped-spells [spark]}
                                    {:prepped-spells [spark]}]
                         :life     8}]}))))

(deftest dispel-test
  (testing "Dispel"
    (is (= (-> {:nemesis   {:deck    [dispel]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{}]}
               draw-nemesis-card)
           {:nemesis   {:discard [dispel]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{}]}))
    (is (= (-> {:nemesis   {:deck    [dispel]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]}]}
               draw-nemesis-card
               (choose {:player-no 0 :breach-no 0 :card-name :spark}))
           {:nemesis   {:discard [dispel]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{}]
                         :discard  [spark]}]}))
    (is (= (-> {:nemesis   {:deck    [dispel]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]}
                            {:breaches [{:prepped-spells [ignite]}
                                        {:prepped-spells [spark]}]}]}
               draw-nemesis-card
               (choose {:player-no 1 :breach-no 0 :card-name :ignite}))
           {:nemesis   {:discard [dispel]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{:prepped-spells [spark]}]}
                        {:breaches [{}
                                    {:prepped-spells [spark]}]
                         :discard  [ignite]}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [dispel]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:breaches [{:prepped-spells [ignite]}]}
                                           {:breaches [{:prepped-spells [spark]}
                                                       {:prepped-spells [spark]}]}]}
                              draw-nemesis-card
                              (choose {:player-no 0 :breach-no 0 :card-name :ignite}))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [dispel]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:breaches [{:prepped-spells [spark]}]}
                                           {:breaches [{:prepped-spells [ignite]}
                                                       {:prepped-spells [spark]}]}]}
                              draw-nemesis-card
                              (choose {:player-no 1 :breach-no 1 :card-name :spark}))))
    (is (= (-> {:nemesis   {:deck    [dispel]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]}
                            {:breaches [{:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose {:player-no 0 :breach-no 0 :card-name :spark}))
           {:nemesis   {:discard [dispel]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{}]
                         :discard  [spark]}
                        {:breaches [{:prepped-spells [ignite]}]}]}))))

(deftest encroach-test
  (testing "Encroach"
    (is (= (-> {:nemesis    {:deck    [encroach]
                             :unleash [[:damage-gravehold 1]]}
                :gravehold  {:life 30}
                :turn-order {:deck [turn-order/player-1]}
                :players    [{:life 10}
                             {:life 10}]}
               draw-nemesis-card)
           {:nemesis    {:discard [encroach]
                         :unleash [[:damage-gravehold 1]]}
            :gravehold  {:life 29}
            :turn-order {:deck           [turn-order/player-1]
                         :revealed-cards 1}
            :players    [{:life 8}
                         {:life 10}]}))
    (is (= (-> {:nemesis    {:deck    [encroach]
                             :unleash [[:damage-gravehold 1]]}
                :gravehold  {:life 30}
                :turn-order {:discard [turn-order/player-2]}
                :players    [{:life 10}
                             {:life 10}]}
               draw-nemesis-card)
           {:nemesis    {:discard [encroach]
                         :unleash [[:damage-gravehold 1]]}
            :gravehold  {:life 29}
            :turn-order {:deck           [turn-order/player-2]
                         :revealed-cards 1}
            :players    [{:life 10}
                         {:life 8}]}))
    (is (= (-> {:nemesis    {:deck    [encroach]
                             :unleash [[:damage-gravehold 1]]}
                :gravehold  {:life 30}
                :turn-order {:deck [turn-order/nemesis]}
                :players    [{:life 10}
                             {:life 10}]}
               draw-nemesis-card)
           {:nemesis    {:discard [encroach]
                         :unleash [[:damage-gravehold 1]]}
            :gravehold  {:life 26}
            :turn-order {:deck           [turn-order/nemesis]
                         :revealed-cards 1}
            :players    [{:life 10}
                         {:life 10}]}))
    (is (= (-> {:nemesis    {:deck    [encroach]
                             :unleash [[:damage-gravehold 1]]}
                :gravehold  {:life 30}
                :turn-order {:deck [turn-order/wild]}
                :players    [{:life 10}
                             {:life 10}
                             {:life 10}]}
               draw-nemesis-card
               (choose {:player-no 2}))
           {:nemesis    {:discard [encroach]
                         :unleash [[:damage-gravehold 1]]}
            :gravehold  {:life 29}
            :turn-order {:deck           [turn-order/wild]
                         :revealed-cards 1}
            :players    [{:life 10}
                         {:life 10}
                         {:life 8}]}))))

(deftest engulf-test
  (let [smite (assoc smite :id 1)]
    (testing "Engulf"
      (is (= (-> {:nemesis   {:deck    [engulf]
                              :discard [smite]
                              :unleash [[:damage-gravehold 1]]}
                  :gravehold {:life 30}}
                 draw-nemesis-card)
             {:nemesis   {:discard [smite engulf]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 26}}))
      (is (= (-> {:nemesis   {:deck    [engulf]
                              :discard [banish smite]
                              :unleash [[:damage-gravehold 1]]}
                  :gravehold {:life 30}}
                 draw-nemesis-card)
             {:nemesis   {:discard [banish smite engulf]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 26}}))
      (is (= (-> {:nemesis   {:deck    [engulf]
                              :discard [smite mangleroot]
                              :unleash [[:damage-gravehold 1]]}
                  :gravehold {:life 30}}
                 draw-nemesis-card)
             {:nemesis   {:discard [mangleroot smite engulf]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 26}}))
      (is (= (-> {:nemesis   {:deck    [engulf]
                              :discard [mangleroot]
                              :unleash [[:damage-gravehold 1]]}
                  :gravehold {:life 30}}
                 draw-nemesis-card)
             {:nemesis   {:discard [mangleroot engulf]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 30}})))))

(deftest gathering-darkness-test
  (testing "Gathering Darkness"
    (is (= (-> {:nemesis   {:deck    [gathering-darkness]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:deck    [crystal crystal crystal]
                             :discard [spark spark]}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose [:spark :crystal :crystal :crystal]))
           {:nemesis   {:discard [gathering-darkness]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:deck [spark]}]
            :trash     [spark crystal crystal crystal]}))
    (is (= (-> {:nemesis   {:deck    [gathering-darkness]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:deck [crystal crystal crystal crystal]}
                            {:deck [crystal crystal crystal]}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose [:crystal :crystal :crystal :crystal]))
           {:nemesis   {:discard [gathering-darkness]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{}
                        {:deck [crystal crystal crystal]}]
            :trash     [crystal crystal crystal crystal]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [gathering-darkness]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:deck [crystal crystal crystal crystal]}
                                           {:deck [crystal crystal crystal]}]}
                              draw-nemesis-card
                              (choose {:player-no 1}))))
    (is (= (-> {:nemesis   {:deck    [gathering-darkness]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:deck [crystal crystal crystal]}
                            {:deck [crystal crystal crystal]}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose [:crystal :crystal :crystal]))
           {:nemesis   {:discard [gathering-darkness]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{}
                        {:deck [crystal crystal crystal]}]
            :trash     [crystal crystal crystal]}))))

(deftest mutilate-test
  (testing "Mutilate"
    (is (= (-> {:nemesis   {:deck    [mutilate]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]
                             :life     10}
                            {:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]
                             :life     10}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                        {:player-no 1 :breach-no 1 :card-name :spark}])
               (choose {:player-no 1}))
           {:nemesis   {:discard [mutilate]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:breaches [{}]
                         :discard  [spark]
                         :life     10}
                        {:breaches [{:prepped-spells [spark]}
                                    {}]
                         :discard  [spark]
                         :life     8}]}))
    (is (= (-> {:nemesis   {:deck    [mutilate]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]
                             :life     10}
                            {:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]
                             :life     10}]}
               draw-nemesis-card
               (choose [{:player-no 1 :breach-no 0 :card-name :spark}
                        {:player-no 1 :breach-no 1 :card-name :spark}])
               (choose {:player-no 1}))
           {:nemesis   {:discard [mutilate]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:breaches [{:prepped-spells [spark]}]
                         :life     10}
                        {:breaches [{}
                                    {}]
                         :discard  [spark spark]
                         :life     8}]}))
    (is (= (-> {:nemesis   {:deck    [mutilate]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]
                             :life     10}
                            {:breaches [{}
                                        {}]
                             :life     10}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}])
               (choose {:player-no 1}))
           {:nemesis   {:discard [mutilate]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:breaches [{}]
                         :discard  [spark]
                         :life     10}
                        {:breaches [{}
                                    {}]
                         :life     8}]}))))

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
                              (choose :crystal))))
    (is (= (-> {:nemesis   {:deck    [nix]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal jade]
                             :life 10}
                            {:life 10}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose :jade))
           {:nemesis   {:discard [nix]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:hand    [crystal]
                         :discard [jade]
                         :life    9}
                        {:life 10}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [nix]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:hand [crystal jade]
                                            :life 10}
                                           {:life 10}]}
                              draw-nemesis-card
                              (choose {:player-no 1}))))
    (is (= (-> {:nemesis   {:deck    [nix]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:life 10}
                            {:life 10}]}
               draw-nemesis-card
               (choose {:player-no 1}))
           {:nemesis   {:discard [nix]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:life 10}
                        {:life 9}]}))
    (is (= (-> {:nemesis   {:deck    [nix]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [{:name :no-cost}]
                             :life 10}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose :no-cost))
           {:nemesis   {:discard [nix]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:discard [{:name :no-cost}]
                         :life    9}]}))))

(deftest obliterate-test
  (testing "Obliterate"
    (is (= (-> {:nemesis   {:deck    [obliterate]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand    [crystal crystal crystal spark spark]
                             :ability {:charges 1}
                             :life    10}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose [:crystal :crystal :spark :spark]))
           {:nemesis   {:discard [obliterate]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand    [crystal]
                         :ability {:charges 1}
                         :life    10}]
            :trash     [crystal crystal spark spark]}))
    (is (= (-> {:nemesis   {:deck    [obliterate]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand    [crystal crystal crystal spark spark]
                             :ability {:charges 1}
                             :life    10}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose nil))
           {:nemesis   {:discard [obliterate]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand    [crystal crystal crystal spark spark]
                         :ability {:charges 1}
                         :life    6}]}))
    (is (= (-> {:nemesis   {:deck    [obliterate]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand    [crystal crystal crystal]
                             :ability {:charges 1}
                             :life    10}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [obliterate]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand    [crystal crystal crystal]
                         :ability {:charges 1}
                         :life    6}]}))))

(deftest throttle-test
  (testing "Throttle"
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal crystal crystal spark spark]}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose [:crystal :spark :spark]))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand [crystal crystal]}]
            :trash     [crystal spark spark]}))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [jade crystal crystal spark spark]}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose [:spark :spark]))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand [crystal crystal]}]
            :trash     [jade spark spark]}))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [jade crystal ignite spark spark]}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose :spark))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand [crystal spark]}]
            :trash     [ignite jade spark]}))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [jade crystal ignite jade spark]}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand [crystal spark]}]
            :trash     [ignite jade jade]}))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [jade crystal ignite]}
                            {:hand [ignite ignite spark jade]}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{}
                        {:hand [ignite ignite spark jade]}]
            :trash     [ignite jade crystal]}))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [spark crystal]}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{}]
            :trash     [spark crystal]}))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [jade ignite ignite jade spark]}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose :jade))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand [jade spark]}]
            :trash     [ignite ignite jade]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [throttle]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:hand [jade ignite ignite jade spark]}]}
                              draw-nemesis-card
                              (choose {:player-no 0})
                              (choose :spark))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [throttle]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:hand [crystal crystal crystal spark spark]}
                                           {:hand [crystal crystal]}]}
                              draw-nemesis-card
                              (choose {:player-no 1}))))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal spark]}
                            {:hand [crystal crystal]}]}
               draw-nemesis-card
               (choose {:player-no 1}))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand [crystal spark]}
                        {}]
            :trash     [crystal crystal]}))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal spark {:name :no-cost} {:name :no-cost}]}]}
               draw-nemesis-card
               (choose {:player-no 0})
               (choose :no-cost))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand [{:name :no-cost}]}]
            :trash     [crystal spark {:name :no-cost}]}))
    (is (= (-> {:nemesis   {:deck    [throttle]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal spark crystal {:name :no-cost}]}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand [{:name :no-cost}]}]
            :trash     [crystal spark crystal]}))))
