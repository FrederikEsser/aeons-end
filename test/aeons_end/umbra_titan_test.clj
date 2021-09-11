(ns aeons-end.umbra-titan-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.umbra-titan :as umbra-titan :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [amplify-vision dark-fire ignite]]
            [aeons-end.turn-order :as turn-order]))

(deftest crumble-test
  (testing "Crumble"
    (is (= (-> {:nemesis   {:deck    [crumble]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:pillars 8
                            :life    30}}
               draw-nemesis-card)
           {:nemesis   {:discard [crumble]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:pillars 5
                        :life    30}}))
    (is (= (-> {:nemesis   {:deck    [crumble]
                            :discard [cryptid]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:pillars 8
                            :life    30}}
               draw-nemesis-card
               (choose nil))
           {:nemesis   {:discard [cryptid crumble]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:pillars 5
                        :life    30}}))
    (is (= (-> {:nemesis   {:deck    [crumble]
                            :discard [cryptid]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:pillars 8
                            :life    30}}
               draw-nemesis-card
               (choose :cryptid))
           {:nemesis   {:play-area [cryptid]
                        :discard   [crumble]
                        :unleash   [[:damage-gravehold 1]]}
            :gravehold {:pillars 8
                        :life    29}}))
    (is (= (-> {:nemesis   {:deck    [crumble]
                            :discard [cryptid grubber]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:pillars 8
                            :life    30}}
               draw-nemesis-card
               (choose :grubber))
           {:nemesis   {:play-area [grubber]
                        :discard   [cryptid crumble]
                        :unleash   [[:damage-gravehold 1]]}
            :gravehold {:pillars 8
                        :life    29}}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck    [crumble]
                                           :discard [cryptid grubber]
                                           :unleash [[:damage-gravehold 1]]}
                               :gravehold {:pillars 8
                                           :life    30}}
                              draw-nemesis-card
                              (choose :cryptid))))))

(deftest cryptid-test
  (testing "Cryptid"
    (is (= (-> {:nemesis   {:play-area [cryptid]}
                :gravehold {:pillars 8}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:play-area [cryptid]}
            :gravehold {:pillars 7}}))
    (is (= (-> {:nemesis   {:play-area [cryptid]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}]}]}
               (resolve-nemesis-cards-in-play)
               (choose nil))
           {:nemesis   {:play-area [cryptid]}
            :gravehold {:pillars 7}
            :players   [{:breaches [{:prepped-spells [spark]}]}]}))
    (is (= (-> {:nemesis   {:play-area [cryptid]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}]}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0 :breach-no 0 :card-name :spark}))
           {:nemesis   {:play-area [cryptid]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{}]
                         :discard  [spark]}]}))
    (is (= (-> {:nemesis   {:play-area [cryptid]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}]}
                            {:breaches [{:prepped-spells [ignite]}]}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 1 :breach-no 0 :card-name :ignite}))
           {:nemesis   {:play-area [cryptid]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{:prepped-spells [spark]}]}
                        {:breaches [{}]
                         :discard  [ignite]}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:play-area [cryptid]}
                               :gravehold {:pillars 8}
                               :players   [{:breaches [{:prepped-spells [spark]}]}
                                           {:breaches [{:prepped-spells [ignite]}]}]}
                              (resolve-nemesis-cards-in-play)
                              (choose {:player-no 0 :breach-no 0 :card-name :spark}))))
    (is (= (-> {:nemesis   {:play-area [cryptid]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}
                            {:breaches [{:prepped-spells [ignite]}]}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0 :breach-no 1 :card-name :ignite}))
           {:nemesis   {:play-area [cryptid]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{:prepped-spells [spark]}
                                    {}]
                         :discard  [ignite]}
                        {:breaches [{:prepped-spells [ignite]}]}]}))))

(deftest grubber-test
  (testing "Grubber"
    (is (= (-> {:nemesis    {:play-area [grubber]}
                :gravehold  {:pillars 8
                             :life    30}
                :turn-order {:discard [turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play))
           {:nemesis    {:play-area [grubber]}
            :gravehold  {:pillars 8
                         :life    28}
            :turn-order {:discard [turn-order/nemesis]}}))
    (is (= (-> {:nemesis    {:play-area [grubber]}
                :gravehold  {:pillars 8
                             :life    30}
                :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}}
               (resolve-nemesis-cards-in-play))
           {:nemesis    {:play-area [grubber]}
            :gravehold  {:pillars 7
                         :life    30}
            :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}}))))

(deftest maul-test
  (testing "Maul"
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{}]}]}
               draw-nemesis-card)
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 6}
            :players   [{:breaches [{}]}]}))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}]}]}
               draw-nemesis-card)
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 6}
            :players   [{:breaches [{:prepped-spells [spark]}]}]}))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]}]}
               draw-nemesis-card
               (choose nil))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 6}
            :players   [{:breaches [{:prepped-spells [spark]}
                                    {:prepped-spells [spark]}]}]}))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                        {:player-no 0 :breach-no 1 :card-name :spark}]))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{}
                                    {}]}]
            :trash     [spark spark]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck [maul]}
                               :gravehold {:pillars 8}
                               :players   [{:breaches [{:prepped-spells [spark]}
                                                       {:prepped-spells [spark]}]}]}
                              draw-nemesis-card
                              (choose [{:player-no 0 :breach-no 0 :card-name :spark}]))))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}]}
                            {:breaches [{:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                        {:player-no 1 :breach-no 0 :card-name :ignite}]))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{}]}
                        {:breaches [{}]}]
            :trash     [spark ignite]}))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [spark]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                        {:player-no 0 :breach-no 1 :card-name :spark}]))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{}
                                    {}
                                    {:prepped-spells [spark]}]}]
            :trash     [spark spark]}))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [ignite]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :ignite}
                        {:player-no 0 :breach-no 2 :card-name :ignite}]))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{}
                                    {:prepped-spells [spark]}
                                    {}]}]
            :trash     [ignite ignite]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck [maul]}
                               :gravehold {:pillars 8}
                               :players   [{:breaches [{:prepped-spells [ignite]}
                                                       {:prepped-spells [spark]}
                                                       {:prepped-spells [ignite]}]}]}
                              draw-nemesis-card
                              (choose [{:player-no 0 :breach-no 0 :card-name :ignite}
                                       {:player-no 0 :breach-no 1 :card-name :spark}]))))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [dark-fire]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose [{:player-no 0 :breach-no 0 :card-name :dark-fire}
                        {:player-no 0 :breach-no 2 :card-name :ignite}]))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{}
                                    {:prepped-spells [spark]}
                                    {}]}]
            :trash     [dark-fire ignite]}))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose :pillars))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 6}
            :players   [{:breaches [{:prepped-spells [spark]}
                                    {:prepped-spells [spark]}
                                    {:prepped-spells [ignite]}]}]}))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}]}]}
               draw-nemesis-card
               (choose :spells)
               (choose {:player-no 0 :breach-no 1 :card-name :spark}))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{:prepped-spells [spark]}
                                    {}
                                    {}]}]
            :trash     [ignite spark]}))
    (is (= (-> {:nemesis   {:deck [maul]}
                :gravehold {:pillars 8}
                :players   [{:breaches [{:prepped-spells [spark]}
                                        {:prepped-spells [ignite]}
                                        {:prepped-spells [ignite]}
                                        {:prepped-spells [dark-fire]}]}]}
               draw-nemesis-card
               (choose :spells)
               (choose {:player-no 0 :breach-no 1 :card-name :ignite}))
           {:nemesis   {:discard [maul]}
            :gravehold {:pillars 8}
            :players   [{:breaches [{:prepped-spells [spark]}
                                    {}
                                    {:prepped-spells [ignite]}
                                    {}]}]
            :trash     [dark-fire ignite]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis   {:deck [maul]}
                               :gravehold {:pillars 8}
                               :players   [{:breaches [{:prepped-spells [spark]}
                                                       {:prepped-spells [ignite]}
                                                       {:prepped-spells [ignite]}
                                                       {:prepped-spells [dark-fire]}]}]}
                              draw-nemesis-card
                              (choose :spells)
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
    (is (= (-> {:nemesis   {:play-area [(assoc-in seismic-roar [:power :power] 1)]}
                :gravehold {:pillars 8}}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in seismic-roar [:power :power] 0)]}
            :gravehold {:pillars 6}}))))

(deftest umbra-titan-test
  (testing "Umbra Titan"
    (testing "Unleash"
      (is (= (-> {:nemesis    {:unleash [[::umbra-titan/unleash]]}
                  :turn-order {:discard [turn-order/nemesis]}
                  :gravehold  {:pillars 8}
                  :players    [{:life 10}]}
                 unleash
                 (choose nil))
             {:nemesis    {:unleash [[::umbra-titan/unleash]]}
              :turn-order {:discard [turn-order/nemesis]}
              :gravehold  {:pillars 7}
              :players    [{:life 10}]}))
      (is (= (-> {:nemesis    {:unleash [[::umbra-titan/unleash]]}
                  :turn-order {:discard [turn-order/player-1 turn-order/nemesis]}
                  :gravehold  {:pillars 8}
                  :players    [{:life 10}]}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis    {:unleash [[::umbra-titan/unleash]]}
              :turn-order {:discard [turn-order/player-1 turn-order/nemesis]}
              :gravehold  {:pillars 8}
              :players    [{:life 8}]}))
      (is (= (-> {:nemesis    {:unleash [[::umbra-titan/unleash]]}
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
                  :gravehold  {:pillars 8
                               :life    30}}
                 unleash
                 (choose :pillar))
             {:nemesis    {:unleash [[::umbra-titan/unleash]]}
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
              :gravehold  {:pillars 7
                           :life    30}}))
      (is (= (-> {:nemesis    {:unleash [[::umbra-titan/unleash]]}
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
                  :gravehold  {:pillars 8
                               :life    30}}
                 unleash
                 (choose :damage))
             {:nemesis    {:unleash [[::umbra-titan/unleash]]}
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
              :gravehold  {:pillars 8
                           :life    28}})))
    (testing "Victory condition"
      (is (= (-> {:real-game? true
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
                  :nemesis    (merge umbra-titan
                                     {:deck [{}]})
                  :gravehold  {:pillars 2}}
                 unleash
                 (choose :pillar))
             {:real-game? true
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
              :nemesis    (merge umbra-titan
                                 {:deck [{}]})
              :gravehold  {:pillars 1}}))
      (is (= (-> {:real-game? true
                  :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
                  :nemesis    (merge umbra-titan
                                     {:deck [{}]})
                  :gravehold  {:pillars 1}}
                 unleash
                 (choose :pillar)
                 (update :game-over dissoc :text))
             {:real-game? true
              :turn-order {:discard [turn-order/nemesis turn-order/nemesis]}
              :nemesis    (merge umbra-titan
                                 {:deck [{}]})
              :gravehold  {:pillars 0}
              :game-over  {:conclusion :defeat}})))))
