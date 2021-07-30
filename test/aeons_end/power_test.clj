(ns aeons-end.power-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.power :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [amplify-vision dark-fire ignite]]))

(defn fixture [f]
  (with-rand-seed 42 (f)))

(use-fixtures :each fixture)

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

(deftest cataclysmic-fate-test
  (testing "Cataclysmic Fate"
    (is (= (-> {:nemesis {:play-area [cataclysmic-fate]}
                :players [{:breaches [{:status         :opened
                                       :prepped-spells [dark-fire]}]}]}
               (discard-power-card 0 :cataclysmic-fate)
               (choose {:player-no 0 :breach-no 0 :card-name :dark-fire}))
           {:nemesis {:discard [cataclysmic-fate]}
            :players [{:breaches [{:status :destroyed}]}]
            :trash   [dark-fire]}))
    (is (thrown-with-msg? AssertionError #"Resolve TO DISCARD error:"
                          (-> {:nemesis {:play-area [cataclysmic-fate]}
                               :players [{:breaches [{:prepped-spells [ignite]}]}]}
                              (discard-power-card 0 :cataclysmic-fate))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [cataclysmic-fate]}
                               :players [{:breaches [{:status         :opened
                                                      :prepped-spells [ignite]}
                                                     {:status         :opened
                                                      :prepped-spells [dark-fire]}]}]}
                              (discard-power-card 0 :cataclysmic-fate)
                              (choose {:player-no 0 :breach-no 0 :card-name :ignite}))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [cataclysmic-fate]}
                               :players [{:breaches [{:status         :opened
                                                      :prepped-spells [ignite]}
                                                     {:status         :opened
                                                      :prepped-spells [dark-fire]}]}
                                         {:breaches [{:status         :opened
                                                      :prepped-spells [dark-fire]}]}]}
                              (discard-power-card 0 :cataclysmic-fate)
                              (choose {:player-no 1 :breach-no 0 :card-name :dark-fire}))))
    (is (= (-> {:nemesis {:play-area [(assoc-in cataclysmic-fate [:power :power] 1)]}
                :players [{:life 10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0}))
           {:nemesis {:discard [(assoc-in cataclysmic-fate [:power :power] 0)]}
            :players [{:life 6}]}))
    (is (= (-> {:nemesis {:play-area [(assoc-in cataclysmic-fate [:power :power] 1)]}
                :players [{:life 8}
                          {:life 10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0}))
           {:nemesis {:discard [(assoc-in cataclysmic-fate [:power :power] 0)]}
            :players [{:life 4}
                      {:life 10}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [(assoc-in cataclysmic-fate [:power :power] 1)]}
                               :players [{:life 8}
                                         {:life 10}]}
                              resolve-nemesis-cards-in-play
                              (choose {:player-no 1}))))
    (is (= (-> {:nemesis {:play-area [(assoc-in cataclysmic-fate [:power :power] 1)]}
                :players [{:life 0}
                          {:life 10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 1}))
           {:nemesis {:discard [(assoc-in cataclysmic-fate [:power :power] 0)]}
            :players [{:life 0}
                      {:life 6}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [(assoc-in cataclysmic-fate [:power :power] 1)]}
                               :players [{:life 0}
                                         {:life 10}]}
                              resolve-nemesis-cards-in-play
                              (choose {:player-no 0}))))))

(deftest chaos-flail-test
  (testing "Chaos Flail"
    (is (= (-> {:nemesis   {:play-area [(assoc-in chaos-flail [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:deck    [crystal crystal]
                             :discard [spark spark]}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0})
               (choose :spark))
           {:nemesis   {:discard [(assoc-in chaos-flail [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:deck           [crystal spark crystal]
                         :revealed-cards 1}]
            :trash     [spark]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in chaos-flail [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:deck    [jade crystal]
                             :discard [spark spark]}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0})
               (choose :jade))
           {:nemesis   {:discard [(assoc-in chaos-flail [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:deck           [spark crystal spark]
                         :revealed-cards 1}]
            :trash     [jade]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:play-area [(assoc-in chaos-flail [:power :power] 1)]
                                           :unleash   [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:deck [crystal jade]}]}
                              resolve-nemesis-cards-in-play
                              (choose {:player-no 0})
                              (choose :crystal))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:play-area [(assoc-in chaos-flail [:power :power] 1)]
                                           :unleash   [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:deck [crystal jade]}
                                           {}]}
                              resolve-nemesis-cards-in-play
                              (choose {:player-no 1}))))
    (is (= (-> {:nemesis   {:play-area [(assoc-in chaos-flail [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{}
                            {:discard [crystal]}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 1})
               (choose :crystal))
           {:nemesis   {:discard [(assoc-in chaos-flail [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{}
                        {}]
            :trash     [crystal]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in chaos-flail [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:deck [crystal {:name :no-cost}]}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0})
               (choose :crystal))
           {:nemesis   {:discard [(assoc-in chaos-flail [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:deck           [{:name :no-cost}]
                         :revealed-cards 1}]
            :trash     [crystal]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in chaos-flail [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:deck [{:name :no-cost} {:name :no-cost}]}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0})
               (choose :no-cost))
           {:nemesis   {:discard [(assoc-in chaos-flail [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:deck           [{:name :no-cost}]
                         :revealed-cards 1}]
            :trash     [{:name :no-cost}]}))))

(deftest dire-abbatoir-test
  (testing "Dire Abbatoir"
    (is (= (-> {:nemesis {:play-area [(assoc-in dire-abbatoir [:power :power] 1)]}
                :players [{:life 10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0}))
           {:nemesis {:discard [(assoc-in dire-abbatoir [:power :power] 0)]}
            :players [{:life 0}]}))
    (is (= (-> {:nemesis {:play-area [(assoc-in dire-abbatoir [:power :power] 1)]}
                :players [{:life 2}
                          {:life 8}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 1}))
           {:nemesis {:discard [(assoc-in dire-abbatoir [:power :power] 0)]}
            :players [{:life 2}
                      {:life 0}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [(assoc-in dire-abbatoir [:power :power] 1)]}
                               :players [{:life 2}
                                         {:life 8}]}
                              resolve-nemesis-cards-in-play
                              (choose {:player-no 0}))))))

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
                :players [{:breaches [{:status         :opened
                                       :prepped-spells [spark]}
                                      {:status         :opened
                                       :prepped-spells [spark]}]}]}
               (discard-power-card 0 :planar-collision)
               (choose [{:player-no 0
                         :breach-no 0
                         :card-name :spark}
                        {:player-no 0
                         :breach-no 1
                         :card-name :spark}]))
           {:nemesis {:discard [planar-collision]}
            :players [{:breaches [{:status :opened}
                                  {:status :opened}]
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
                               :players [{:breaches [{:status         :opened
                                                      :prepped-spells [spark]}
                                                     {:status         :opened
                                                      :prepped-spells [spark]}]}]}
                              (discard-power-card 0 :planar-collision)
                              (choose [{:player-no 0
                                        :breach-no 0
                                        :card-name :spark}]))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [planar-collision]}
                               :players [{:breaches [{:status         :opened
                                                      :prepped-spells [spark]}
                                                     {:status         :opened
                                                      :prepped-spells [spark]}]}
                                         {:breaches [{:status         :opened
                                                      :prepped-spells [spark]}]}]}
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

(deftest pulverizing-ray-test
  (testing "Pulverizing Ray"
    (is (= (-> {:nemesis   {:play-area [(assoc-in pulverizing-ray [:power :power] 1)]}
                :players   [{:hand [spark spark]}]
                :gravehold {:life 30}}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in pulverizing-ray [:power :power] 0)]}
            :players   [{:hand [spark spark]}]
            :gravehold {:life 12}}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in pulverizing-ray [:power :power] 1)]}
                :players   [{:hand [crystal crystal crystal spark spark]}]
                :gravehold {:life 30}}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :card-name :crystal}
                        {:player-no 0 :card-name :crystal}]))
           {:nemesis   {:discard [(assoc-in pulverizing-ray [:power :power] 0)]}
            :players   [{:hand    [crystal spark spark]
                         :discard [crystal crystal]}]
            :gravehold {:life 18}}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in pulverizing-ray [:power :power] 1)]}
                :players   [{:hand [crystal crystal crystal spark spark]}
                            {:hand [crystal crystal crystal spark spark]}]
                :gravehold {:life 30}}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :card-name :crystal}
                        {:player-no 0 :card-name :crystal}
                        {:player-no 0 :card-name :crystal}
                        {:player-no 1 :card-name :crystal}
                        {:player-no 1 :card-name :crystal}
                        {:player-no 1 :card-name :crystal}]))
           {:nemesis   {:discard [(assoc-in pulverizing-ray [:power :power] 0)]}
            :players   [{:hand    [spark spark]
                         :discard [crystal crystal crystal]}
                        {:hand    [spark spark]
                         :discard [crystal crystal crystal]}]
            :gravehold {:life 30}}))))

(deftest reality-rupture-test
  (testing "Reality Rapture"
    (is (= (-> {:nemesis {:play-area [reality-rupture]}
                :players [{:breaches [{:status         :opened
                                       :prepped-spells [ignite]}
                                      {:status         :opened
                                       :prepped-spells [ignite]}]}]}
               (discard-power-card 0 :reality-rupture)
               (choose [{:player-no 0 :breach-no 0 :card-name :ignite}
                        {:player-no 0 :breach-no 1 :card-name :ignite}]))
           {:nemesis {:discard [reality-rupture]}
            :players [{:breaches [{:status :opened}
                                  {:status :opened}]}]
            :trash   [ignite ignite]}))
    (is (thrown-with-msg? AssertionError #"Resolve TO DISCARD error:"
                          (-> {:nemesis {:play-area [reality-rupture]}
                               :players [{:breaches [{:status         :opened
                                                      :prepped-spells [spark]}
                                                     {:status         :opened
                                                      :prepped-spells [ignite]}]}]}
                              (discard-power-card 0 :reality-rupture))))))

(deftest withering-beam-test
  (testing "Withering Beam"
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{}]}]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{}]}]
            :trash     [spark]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{} {}]}]
            :trash     [spark spark]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{} {}]}]
            :trash     [ignite spark]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]}]}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                        {:player-no 0 :breach-no 1 :card-name :spark}]))
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{} {}
                                    {:prepped-spells [spark]}]}]
            :trash     [spark spark]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [ignite]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{}
                                    {:prepped-spells [spark]}
                                    {}]}]
            :trash     [ignite ignite]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [dark-fire]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}]}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{}
                                    {:prepped-spells [spark]}
                                    {}]}]
            :trash     [dark-fire ignite]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}]}
               resolve-nemesis-cards-in-play
               (choose [{:player-no 0 :breach-no 1 :card-name :spark}]))
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{:prepped-spells [spark]}
                                    {}
                                    {}]}]
            :trash     [ignite spark]}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                            :unleash   [[:damage-gravehold 1]]}
                :gravehold {:life 30}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}
                                        {:prepped-spells [ignite]}
                                        {:prepped-spells [dark-fire]}]}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0 :breach-no 1 :card-name :ignite}))
           {:nemesis   {:discard [(assoc-in withering-beam [:power :power] 0)]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{:breaches [{:prepped-spells [spark]}
                                    {}
                                    {:prepped-spells [ignite]}
                                    {}]}]
            :trash     [dark-fire ignite]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:play-area [(assoc-in withering-beam [:power :power] 1)]
                                           :unleash   [[:damage-gravehold 1]]}
                               :gravehold {:life 30}
                               :players   [{:breaches [{:prepped-spells [spark]}
                                                       {:prepped-spells [ignite]}
                                                       {:prepped-spells [ignite]}
                                                       {:prepped-spells [dark-fire]}]}]}
                              resolve-nemesis-cards-in-play
                              (choose {:player-no 0 :breach-no 0 :card-name :spark}))))))
