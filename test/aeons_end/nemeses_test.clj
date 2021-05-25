(ns aeons-end.nemeses-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.common]
            [aeons-end.nemeses :as nemeses :refer [cryptid grubber seismic-roar]]
            [aeons-end.cards.base :refer :all]
            [aeons-end.cards.gems :refer [jade]]
            [aeons-end.cards.spells :refer [amplify-vision dark-fire ignite]]
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

(deftest howling-spinners-test
  (testing "Howling Spinners"
    (is (= (-> {:nemesis {:play-area [howling-spinners]}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [howling-spinners]}
            :players [{:life 8}]}))))

(deftest mangleroot-test
  (testing "Mangleroot"
    (is (= (-> {:nemesis   {:play-area [mangleroot]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:play-area [(assoc mangleroot :life 10)]}
            :gravehold {:life 27}}))
    (is (= (-> {:nemesis   {:play-area [(assoc mangleroot :life 3)]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:play-area [(assoc mangleroot :life 1)]}
            :gravehold {:life 27}}))
    (is (= (-> {:nemesis   {:play-area [(assoc mangleroot :life 2)]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:discard [(assoc mangleroot :life 0)]}
            :gravehold {:life 27}}))
    (is (= (-> {:nemesis   {:play-area [(assoc mangleroot :life 1)]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:discard [(assoc mangleroot :life 0)]}
            :gravehold {:life 27}}))))


(deftest monstrosity-of-omens-test
  (testing "Monstrosity of Omens"
    (is (= (-> {:nemesis   {:play-area [monstrosity-of-omens]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:play-area [monstrosity-of-omens]}
            :gravehold {:life 25}}))
    (is (= (-> {:nemesis   {:play-area [(assoc monstrosity-of-omens :life 4)]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:play-area [(assoc monstrosity-of-omens :life 4)]}
            :gravehold {:life 26}}))
    (is (= (-> {:nemesis   {:play-area [(assoc monstrosity-of-omens :life 1)]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:play-area [(assoc monstrosity-of-omens :life 1)]}
            :gravehold {:life 29}}))
    (testing "Taking damage"
      (is (= (-> {:nemesis {:play-area [(assoc monstrosity-of-omens :life 4)]}
                  :players [{:breaches [{:prepped-spells [spark]}]}]}
                 (cast-spell 0 0 :spark)
                 (choose {:area :minions :card-name :monstrosity-of-omens}))
             {:nemesis {:play-area [(assoc monstrosity-of-omens :life 3)]}
              :players [{:breaches [{}]
                         :discard  [spark]}]}))
      (is (= (-> {:nemesis {:play-area [(assoc monstrosity-of-omens :life 4)]}
                  :players [{:breaches [{:status         :opened
                                         :prepped-spells [amplify-vision]}]}]}
                 (cast-spell 0 0 :amplify-vision)
                 (choose {:area :minions :card-name :monstrosity-of-omens}))
             {:nemesis {:play-area [(assoc monstrosity-of-omens :life 3)]}
              :players [{:breaches [{:status :opened}]
                         :discard  [amplify-vision]}]}))
      (is (= (-> {:nemesis {:play-area [(assoc monstrosity-of-omens :life 4)]}
                  :players [{:breaches [{:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [spark]}]}]}
                 (cast-spell 0 0 :spark)
                 (choose {:area :minions :card-name :monstrosity-of-omens}))
             {:nemesis {:play-area [(assoc monstrosity-of-omens :life 3)]}
              :players [{:breaches [{:status       :opened
                                     :bonus-damage 1}]
                         :discard  [spark]}]}))
      (is (= (-> {:nemesis {:play-area [(assoc monstrosity-of-omens :life 4)]}
                  :players [{:breaches [{:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [dark-fire]}]}]}
                 (cast-spell 0 0 :dark-fire)
                 (choose {:area :minions :card-name :monstrosity-of-omens}))
             {:nemesis {:play-area [(assoc monstrosity-of-omens :life 3)]}
              :players [{:breaches [{:status       :opened
                                     :bonus-damage 1}]
                         :discard  [dark-fire]}]})))))

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
                :players   [{:hand [jade crystal ignite]}]}
               draw-nemesis-card
               (choose {:player-no 0}))
           {:nemesis   {:discard [throttle]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}
            :players   [{}]
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
            :trash     [spark crystal]}))))

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
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}})))
    (testing "Seismic Roar"
      (is (= (-> {:nemesis {:play-area [seismic-roar]}
                  :players [{:aether 6}]}
                 (discard-power-card 0 :seismic-roar))
             {:nemesis {:discard [seismic-roar]}
              :players [{:aether 0}]}))
      (is (thrown-with-msg? AssertionError #"Resolve TO DISCARD error:"
                            (-> {:nemesis {:play-area [seismic-roar]}
                                 :players [{:aether 5}]}
                                (discard-power-card 0 :seismic-roar))))
      (is (= (-> {:nemesis {:play-area [(assoc-in seismic-roar [:power :power] 1)]
                            :tokens    8}}
                 resolve-nemesis-cards-in-play)
             {:nemesis {:discard [(assoc-in seismic-roar [:power :power] 0)]
                        :tokens  6}})))))
