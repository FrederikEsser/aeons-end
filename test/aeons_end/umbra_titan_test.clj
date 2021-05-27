(ns aeons-end.umbra-titan-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.umbra-titan :as umbra-titan :refer :all]
            [aeons-end.cards.base :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [amplify-vision dark-fire ignite]]
            [aeons-end.turn-order :as turn-order]))

(deftest crumble-test
  (testing "Crumble"
    (is (= (-> {:nemesis   {:deck    [crumble]
                            :unleash [[:damage-gravehold 1]]
                            :tokens  8}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:discard [crumble]
                        :unleash [[:damage-gravehold 1]]
                        :tokens  5}
            :gravehold {:life 30}}))
    (is (= (-> {:nemesis   {:deck    [crumble]
                            :discard [cryptid]
                            :unleash [[:damage-gravehold 1]]
                            :tokens  8}
                :gravehold {:life 30}}
               draw-nemesis-card
               (choose nil))
           {:nemesis   {:discard [cryptid crumble]
                        :unleash [[:damage-gravehold 1]]
                        :tokens  5}
            :gravehold {:life 30}}))
    (is (= (-> {:nemesis   {:deck    [crumble]
                            :discard [cryptid]
                            :unleash [[:damage-gravehold 1]]
                            :tokens  8}
                :gravehold {:life 30}}
               draw-nemesis-card
               (choose :cryptid))
           {:nemesis   {:play-area [cryptid]
                        :discard   [crumble]
                        :unleash   [[:damage-gravehold 1]]
                        :tokens    8}
            :gravehold {:life 29}}))
    (is (= (-> {:nemesis   {:deck    [crumble]
                            :discard [cryptid grubber]
                            :unleash [[:damage-gravehold 1]]
                            :tokens  8}
                :gravehold {:life 30}}
               draw-nemesis-card
               (choose :grubber))
           {:nemesis   {:play-area [grubber]
                        :discard   [cryptid crumble]
                        :unleash   [[:damage-gravehold 1]]
                        :tokens    8}
            :gravehold {:life 29}}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [crumble]
                                           :discard [cryptid grubber]
                                           :unleash [[:damage-gravehold 1]]
                                           :tokens  8}
                               :gravehold {:life 30}}
                              draw-nemesis-card
                              (choose :cryptid))))))

(deftest cryptid-test
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
                      {:breaches [{:prepped-spells [ignite]}]}]}))))

(deftest grubber-test
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
            :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}}))))

(deftest maul-test
  (testing "Maul"
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{}]}]}
               draw-nemesis-card)
           {:nemesis {:discard [maul]
                      :tokens  6}
            :players [{:breaches [{}]}]}))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [spark]}]}]}
               draw-nemesis-card)
           {:nemesis {:discard [maul]
                      :tokens  6}
            :players [{:breaches [{:prepped-spells [spark]}]}]}))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [spark]}
                                      {:prepped-spells [spark]}]}]}
               draw-nemesis-card
               (choose nil))
           {:nemesis {:discard [maul]
                      :tokens  6}
            :players [{:breaches [{:prepped-spells [spark]}
                                  {:prepped-spells [spark]}]}]}))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [spark]}
                                      {:prepped-spells [spark]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                        {:player-no 0 :breach-no 1 :card-name :spark}]))
           {:nemesis {:discard [maul]
                      :tokens  8}
            :players [{:breaches [{}
                                  {}]}]
            :trash   [spark spark]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:deck   [maul]
                                         :tokens 8}
                               :players [{:breaches [{:prepped-spells [spark]}
                                                     {:prepped-spells [spark]}]}]}
                              draw-nemesis-card
                              (choose [{:player-no 0 :breach-no 0 :card-name :spark}]))))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [spark]}]}
                          {:breaches [{:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                        {:player-no 1 :breach-no 0 :card-name :ignite}]))
           {:nemesis {:discard [maul]
                      :tokens  8}
            :players [{:breaches [{}]}
                      {:breaches [{}]}]
            :trash   [spark ignite]}))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [spark]}
                                      {:prepped-spells [spark]}
                                      {:prepped-spells [spark]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                        {:player-no 0 :breach-no 1 :card-name :spark}]))
           {:nemesis {:discard [maul]
                      :tokens  8}
            :players [{:breaches [{}
                                  {}
                                  {:prepped-spells [spark]}]}]
            :trash   [spark spark]}))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [ignite]}
                                      {:prepped-spells [spark]}
                                      {:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :ignite}
                        {:player-no 0 :breach-no 2 :card-name :ignite}]))
           {:nemesis {:discard [maul]
                      :tokens  8}
            :players [{:breaches [{}
                                  {:prepped-spells [spark]}
                                  {}]}]
            :trash   [ignite ignite]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:deck   [maul]
                                         :tokens 8}
                               :players [{:breaches [{:prepped-spells [ignite]}
                                                     {:prepped-spells [spark]}
                                                     {:prepped-spells [ignite]}]}]}
                              draw-nemesis-card
                              (choose [{:player-no 0 :breach-no 0 :card-name :ignite}
                                       {:player-no 0 :breach-no 1 :card-name :spark}]))))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [dark-fire]}
                                      {:prepped-spells [spark]}
                                      {:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :dark-fire}
                        {:player-no 0 :breach-no 2 :card-name :ignite}]))
           {:nemesis {:discard [maul]
                      :tokens  8}
            :players [{:breaches [{}
                                  {:prepped-spells [spark]}
                                  {}]}]
            :trash   [dark-fire ignite]}))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [spark]}
                                      {:prepped-spells [spark]}
                                      {:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose :lose-tokens))
           {:nemesis {:discard [maul]
                      :tokens  6}
            :players [{:breaches [{:prepped-spells [spark]}
                                  {:prepped-spells [spark]}
                                  {:prepped-spells [ignite]}]}]}))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [spark]}
                                      {:prepped-spells [spark]}
                                      {:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose :destroy)
               (choose {:player-no 0 :breach-no 1 :card-name :spark}))
           {:nemesis {:discard [maul]
                      :tokens  8}
            :players [{:breaches [{:prepped-spells [spark]}
                                  {}
                                  {}]}]
            :trash   [ignite spark]}))
    (is (= (-> {:nemesis {:deck   [maul]
                          :tokens 8}
                :players [{:breaches [{:prepped-spells [spark]}
                                      {:prepped-spells [ignite]}
                                      {:prepped-spells [ignite]}
                                      {:prepped-spells [dark-fire]}]}]}
               draw-nemesis-card
               (choose :destroy)
               (choose {:player-no 0 :breach-no 1 :card-name :ignite}))
           {:nemesis {:discard [maul]
                      :tokens  8}
            :players [{:breaches [{:prepped-spells [spark]}
                                  {}
                                  {:prepped-spells [ignite]}
                                  {}]}]
            :trash   [dark-fire ignite]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:deck   [maul]
                                         :tokens 8}
                               :players [{:breaches [{:prepped-spells [spark]}
                                                     {:prepped-spells [ignite]}
                                                     {:prepped-spells [ignite]}
                                                     {:prepped-spells [dark-fire]}]}]}
                              draw-nemesis-card
                              (choose :destroy)
                              (choose {:player-no 0 :breach-no 0 :card-name :spark}))))))

(deftest seismic-roar-test
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
                      :tokens  6}}))))

(deftest umbra-titan-test
  (testing "Umbra Titan"
    (testing "Unleash"
      (is (= (-> {:nemesis    {:tokens  8
                               :unleash [[::umbra-titan/umbra-titan-unleash]]}
                  :turn-order {:discard [turn-order/nemesis]}
                  :players    [{:life 10}]}
                 unleash
                 (choose nil))
             {:nemesis    {:tokens  7
                           :unleash [[::umbra-titan/umbra-titan-unleash]]}
              :turn-order {:discard [turn-order/nemesis]}
              :players    [{:life 10}]}))
      (is (= (-> {:nemesis    {:tokens  8
                               :unleash [[::umbra-titan/umbra-titan-unleash]]}
                  :turn-order {:discard [turn-order/player-0 turn-order/nemesis]}
                  :players    [{:life 10}]}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis    {:tokens  8
                           :unleash [[::umbra-titan/umbra-titan-unleash]]}
              :turn-order {:discard [turn-order/player-0 turn-order/nemesis]}
              :players    [{:life 8}]}))
      (is (= (-> {:nemesis    {:tokens  8
                               :unleash [[::umbra-titan/umbra-titan-unleash]]}
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
                  :gravehold  {:life 30}}
                 unleash
                 (choose :token))
             {:nemesis    {:tokens  7
                           :unleash [[::umbra-titan/umbra-titan-unleash]]}
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
              :gravehold  {:life 30}}))
      (is (= (-> {:nemesis    {:tokens  8
                               :unleash [[::umbra-titan/umbra-titan-unleash]]}
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
                  :gravehold  {:life 30}}
                 unleash
                 (choose :damage))
             {:nemesis    {:tokens  8
                           :unleash [[::umbra-titan/umbra-titan-unleash]]}
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
              :gravehold  {:life 28}})))))
