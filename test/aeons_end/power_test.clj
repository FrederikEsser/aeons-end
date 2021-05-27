(ns aeons-end.power-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.power :refer :all]
            [aeons-end.cards.base :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [amplify-vision dark-fire ignite]]))

(deftest aphotic-sun-test
  (testing "Aphotic Sun"
    (is (= (-> {:nemesis {:play-area [aphotic-sun]}
                :players [{:aether 7}]}
               (discard-power-card 0 :aphotic-sun))
           {:nemesis {:discard [aphotic-sun]}
            :players [{:aether 0}]}))
    (is (thrown-with-msg? AssertionError #"Resolve TO DISCARD error:"
                          (-> {:nemesis {:play-area [aphotic-sun]}
                               :players [{:aether 6}]}
                              (discard-power-card 0 :aphotic-sun))))
    (is (= (-> {:nemesis   {:play-area [(assoc-in aphotic-sun [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:ability {:charges 5}
                             :life    10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0}))
           {:nemesis   {:discard [(assoc-in aphotic-sun [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:ability {:charges 0}
                         :life    7}]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in aphotic-sun [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:ability {:charges 0}
                             :life    10}
                            {:ability {:charges 0}
                             :life    10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 1}))
           {:nemesis   {:discard [(assoc-in aphotic-sun [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}
            :players   [{:ability {:charges 0}
                         :life    10}
                        {:ability {:charges 0}
                         :life    7}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:play-area [(assoc-in aphotic-sun [:power :power] 1)]
                                           :unleash   [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:ability {:charges 1}
                                            :life    10}
                                           {:ability {:charges 0}
                                            :life    10}]}
                              resolve-nemesis-cards-in-play
                              (choose {:player-no 1}))))))

(deftest morbid-gyre-test
  (testing "Morbid Gyre"
    (is (= (-> {:nemesis   {:play-area [morbid-gyre]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal crystal crystal spark spark]}]}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :card-name :crystal}
                        {:player-no 0 :card-name :spark}
                        {:player-no 0 :card-name :spark}]))
           {:nemesis   {:discard [(assoc-in morbid-gyre [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand    [crystal crystal]
                         :discard [crystal spark spark]}]}))
    (is (= (-> {:nemesis   {:play-area [morbid-gyre]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal crystal crystal spark spark]}
                            {:hand [crystal crystal crystal spark spark]}]}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :card-name :crystal}
                        {:player-no 0 :card-name :spark}
                        {:player-no 1 :card-name :spark}]))
           {:nemesis   {:discard [(assoc-in morbid-gyre [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand    [crystal crystal spark]
                         :discard [crystal spark]}
                        {:hand    [crystal crystal crystal spark]
                         :discard [spark]}]}))
    (is (= (-> {:nemesis   {:play-area [morbid-gyre]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal crystal crystal spark spark]}
                            {:hand [crystal crystal crystal spark spark]}]}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :card-name :crystal}
                        {:player-no 0 :card-name :spark}
                        {:player-no 1 :card-name :spark}]))
           {:nemesis   {:discard [(assoc-in morbid-gyre [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:hand    [crystal crystal spark]
                         :discard [crystal spark]}
                        {:hand    [crystal crystal crystal spark]
                         :discard [spark]}]}))
    (is (= (-> {:nemesis   {:play-area [morbid-gyre]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal]}
                            {:hand [spark]}]}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :card-name :crystal}
                        {:player-no 1 :card-name :spark}]))
           {:nemesis   {:discard [(assoc-in morbid-gyre [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:discard [crystal]}
                        {:discard [spark]}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:play-area [morbid-gyre]
                                           :unleash   [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:hand [crystal crystal crystal spark spark]}]}
                              resolve-nemesis-cards-in-play
                              (choose [{:player-no 0 :card-name :crystal}
                                       {:player-no 0 :card-name :spark}]))))
    (is (= (-> {:nemesis   {:play-area [morbid-gyre]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:hand [crystal]}]}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :card-name :crystal}]))
           {:nemesis   {:discard [(assoc-in morbid-gyre [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:discard [crystal]}]}))
    (is (= (-> {:nemesis   {:play-area [morbid-gyre]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in morbid-gyre [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{}]}))))

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
    (is (= (-> {:nemesis {:play-area [planar-collision]}
                :players [{:breaches [{:prepped-spells [spark]}
                                      {:prepped-spells [spark]}]}]}
               (discard-power-card 0 :planar-collision)
               (choose [{:player-no 0
                         :breach-no 0
                         :card-name :spark}
                        {:player-no 0
                         :breach-no 1
                         :card-name :spark}]))
           {:nemesis {:discard [planar-collision]}
            :players [{:breaches [{}
                                  {}]
                       :discard  [spark spark]}]}))
    (is (thrown-with-msg? AssertionError #"Resolve TO DISCARD error:"
                          (-> {:nemesis {:play-area [planar-collision]}
                               :players [{:breaches [{:prepped-spells [spark]}]}]}
                              (discard-power-card 0 :planar-collision))))
    (is (thrown-with-msg? AssertionError #"Resolve TO DISCARD error:"
                          (-> {:nemesis {:play-area [planar-collision]}
                               :players [{:breaches [{:prepped-spells [spark]}
                                                     {:prepped-spells [spark]}]}
                                         {}]}
                              (discard-power-card 1 :planar-collision))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [planar-collision]}
                               :players [{:breaches [{:prepped-spells [spark]}
                                                     {:prepped-spells [spark]}]}]}
                              (discard-power-card 0 :planar-collision)
                              (choose [{:player-no 0
                                        :breach-no 0
                                        :card-name :spark}]))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [planar-collision]}
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
