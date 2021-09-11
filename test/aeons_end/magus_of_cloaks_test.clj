(ns aeons-end.magus-of-cloaks-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.magus-of-cloaks :as magus-of-cloaks :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer []]
            [aeons-end.cards.relic :refer []]
            [aeons-end.cards.spell :refer [ignite]]))

(deftest crooked-mask-test
  (testing "Magus of Cloaks"
    (testing "Unleash"
      (is (= (-> {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                              :cloaks  4}
                  :gravehold {:life 30}}
                 unleash
                 (choose :cloaks))
             {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                          :cloaks  6}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                              :cloaks  4}
                  :gravehold {:life 30}}
                 unleash
                 (choose :damage))
             {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                          :cloaks  4}
              :gravehold {:life 28}}))
      (is (= (-> {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                              :cloaks  6}
                  :gravehold {:life 30}}
                 unleash
                 (choose :cloaks))
             {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                          :cloaks  8}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                              :cloaks  7}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash [[::magus-of-cloaks/unleash]]
                          :cloaks  7}
              :gravehold {:life 28}})))
    (testing "When damaged"
      (is (= (-> {:nemesis {:shield   ::magus-of-cloaks/shield
                            :when-hit [[::magus-of-cloaks/when-hit]]
                            :cloaks   4
                            :life     35}}
                 (deal-damage 4))
             {:nemesis {:shield   ::magus-of-cloaks/shield
                        :when-hit [[::magus-of-cloaks/when-hit]]
                        :cloaks   3
                        :life     35}}))
      (is (= (-> {:nemesis {:shield   ::magus-of-cloaks/shield
                            :when-hit [[::magus-of-cloaks/when-hit]]
                            :cloaks   4
                            :life     35}}
                 (deal-damage 5))
             {:nemesis {:shield   ::magus-of-cloaks/shield
                        :when-hit [[::magus-of-cloaks/when-hit]]
                        :cloaks   3
                        :life     34}}))
      (is (= (-> {:nemesis {:shield   ::magus-of-cloaks/shield
                            :when-hit [[::magus-of-cloaks/when-hit]]
                            :cloaks   4
                            :life     35}}
                 (deal-damage 6))
             {:nemesis {:shield   ::magus-of-cloaks/shield
                        :when-hit [[::magus-of-cloaks/when-hit]]
                        :cloaks   3
                        :life     33}}))
      (is (= (-> {:nemesis {:shield   ::magus-of-cloaks/shield
                            :when-hit [[::magus-of-cloaks/when-hit]]
                            :cloaks   3
                            :life     35}}
                 (deal-damage 1))
             {:nemesis {:shield   ::magus-of-cloaks/shield
                        :when-hit [[::magus-of-cloaks/when-hit]]
                        :cloaks   2
                        :life     35}}))
      (is (= (-> {:nemesis {:shield   ::magus-of-cloaks/shield
                            :when-hit [[::magus-of-cloaks/when-hit]]
                            :cloaks   2
                            :life     35}}
                 (deal-damage 1))
             {:nemesis {:shield   ::magus-of-cloaks/shield
                        :when-hit [[::magus-of-cloaks/when-hit]]
                        :cloaks   2
                        :life     35}})))))

(deftest ashen-haruspex-test
  (testing "Ashen Haruspex"
    (is (= (-> {:nemesis {:play-area [ashen-haruspex]
                          :cloaks    2}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [ashen-haruspex]
                      :cloaks    2}
            :players [{:life 9}]}))
    (is (= (-> {:nemesis {:play-area [ashen-haruspex]
                          :cloaks    7}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [ashen-haruspex]
                      :cloaks    7}
            :players [{:life 6}]}))
    (testing "Taking damage"
      (is (= (-> {:nemesis {:play-area [(assoc ashen-haruspex :life 4)]}}
                 (deal-damage 2)
                 (choose {:area :minions :player-no 0 :card-name :ashen-haruspex}))
             {:nemesis {:play-area [(assoc ashen-haruspex :life 4)]}}))
      (is (= (-> {:nemesis {:play-area [(assoc ashen-haruspex :life 4)]}}
                 (deal-damage 3)
                 (choose {:area :minions :player-no 0 :card-name :ashen-haruspex}))
             {:nemesis {:play-area [(assoc ashen-haruspex :life 3)]}}))
      (is (= (-> {:nemesis {:play-area [(assoc ashen-haruspex :life 4)]}}
                 (deal-damage 5)
                 (choose {:area :minions :player-no 0 :card-name :ashen-haruspex}))
             {:nemesis {:play-area [(assoc ashen-haruspex :life 1)]}})))))

(deftest black-solstice-test
  (testing "Black Solstice"
    (testing "To Discard"
      (is (= (-> {:nemesis {:play-area [black-solstice]
                            :shield    ::magus-of-cloaks/shield
                            :when-hit  [[::magus-of-cloaks/when-hit]]
                            :cloaks    4
                            :life      30}}
                 (deal-damage 4))
             {:nemesis {:play-area [black-solstice]
                        :shield    ::magus-of-cloaks/shield
                        :when-hit  [[::magus-of-cloaks/when-hit]]
                        :cloaks    3
                        :life      30}}))
      (is (= (-> {:nemesis {:play-area [black-solstice]
                            :shield    ::magus-of-cloaks/shield
                            :when-hit  [[::magus-of-cloaks/when-hit]]
                            :cloaks    3
                            :life      30}}
                 (deal-damage 4))
             {:nemesis {:discard  [black-solstice]
                        :shield   ::magus-of-cloaks/shield
                        :when-hit [[::magus-of-cloaks/when-hit]]
                        :cloaks   2
                        :life     29}})))
    (testing "Power"
      (is (= (-> {:nemesis   {:play-area [(assoc-in black-solstice [:power :power] 1)]
                              :unleash   [[:damage-gravehold 1]]}
                  :gravehold {:life 30}
                  :players   [{:breaches [{:prepped-spells [spark]}]
                               :life     10}]}
                 resolve-nemesis-cards-in-play
                 (choose {:player-no 0})
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:nemesis   {:discard [(assoc-in black-solstice [:power :power] 0)]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 28}
              :players   [{:breaches [{}]
                           :discard  [spark]
                           :life     7}]}))
      (is (= (-> {:nemesis   {:play-area [(assoc-in black-solstice [:power :power] 1)]
                              :unleash   [[:damage-gravehold 1]]}
                  :gravehold {:life 30}
                  :players   [{:breaches [{:prepped-spells [ignite]}]
                               :life     10}
                              {:breaches [{}]
                               :life     10}]}
                 resolve-nemesis-cards-in-play
                 (choose {:player-no 0})
                 (choose {:player-no 0 :breach-no 0 :card-name :ignite}))
             {:nemesis   {:discard [(assoc-in black-solstice [:power :power] 0)]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 28}
              :players   [{:breaches [{}]
                           :discard  [ignite]
                           :life     7}
                          {:breaches [{}]
                           :life     10}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis   {:play-area [(assoc-in black-solstice [:power :power] 1)]
                                             :unleash   [[:damage-gravehold 1]]}
                                 :gravehold {:life 30}
                                 :players   [{:breaches [{:prepped-spells [ignite]}]
                                              :life     10}
                                             {:breaches [{:prepped-spells [spark]}]
                                              :life     3}]}
                                resolve-nemesis-cards-in-play
                                (choose {:player-no 0})
                                (choose {:player-no 1 :breach-no 0 :card-name :spark}))))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis   {:play-area [(assoc-in black-solstice [:power :power] 1)]
                                             :unleash   [[:damage-gravehold 1]]}
                                 :gravehold {:life 30}
                                 :players   [{:breaches [{:prepped-spells [ignite]}]
                                              :life     10}
                                             {:breaches [{}]
                                              :life     10}]}
                                resolve-nemesis-cards-in-play
                                (choose {:player-no 1}))))
      (is (= (-> {:nemesis   {:play-area [(assoc-in black-solstice [:power :power] 1)]
                              :unleash   [[:damage-gravehold 1]]}
                  :gravehold {:life 30}
                  :players   [{:breaches [{}]
                               :life     10}
                              {:breaches [{}]
                               :life     10}]}
                 resolve-nemesis-cards-in-play
                 (choose {:player-no 1}))
             {:nemesis   {:discard [(assoc-in black-solstice [:power :power] 0)]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 28}
              :players   [{:breaches [{}]
                           :life     10}
                          {:breaches [{}]
                           :life     7}]})))))

(deftest dusk-spawn-test
  (testing "Dusk Spawn"
    (is (= (-> {:nemesis {:play-area [dusk-spawn]}
                :players [{:ability {:charges 1}
                           :life    10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:area :players :player-no 0}))
           {:nemesis {:play-area [dusk-spawn]}
            :players [{:ability {:charges 1}
                       :life    8}]}))
    (is (= (-> {:nemesis {:play-area [dusk-spawn]}
                :players [{:ability {:charges 1}
                           :life    10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:area :ability :player-no 0}))
           {:nemesis {:play-area [dusk-spawn]}
            :players [{:ability {:charges 0}
                       :life    10}]}))
    (testing "Taking damage"
      (is (= (-> {:nemesis {:play-area [(assoc dusk-spawn :life 4)]
                            :cloaks    4}}
                 (deal-damage 2)
                 (choose {:area :minions :player-no 0 :card-name :dusk-spawn}))
             {:nemesis {:play-area [(assoc dusk-spawn :life 2)]
                        :cloaks    5}}))
      (is (= (-> {:nemesis {:play-area [(assoc dusk-spawn :life 1)]
                            :cloaks    5}}
                 (deal-damage 1)
                 (choose {:area :minions :player-no 0 :card-name :dusk-spawn}))
             {:nemesis {:discard [(assoc dusk-spawn :life 0)]
                        :cloaks  6}})))))

(deftest eclipse-test
  (testing "Eclipse"
    (let [spark (assoc spark :id 1)]
      (is (= (-> {:nemesis {:deck   [eclipse]
                            :cloaks 2}
                  :players [{:breaches [{:prepped-spells [spark]}]
                             :life     10}
                            {:breaches [{:prepped-spells [spark]}]
                             :life     10}]}
                 draw-nemesis-card
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:nemesis {:discard [eclipse]
                        :cloaks  7}
              :players [{:breaches [{}]
                         :life     8}
                        {:breaches [{:prepped-spells [spark]}]
                         :life     10}]
              :trash   [spark]}))
      (is (= (-> {:nemesis {:deck   [eclipse]
                            :cloaks 3}
                  :players [{:breaches [{:prepped-spells [spark]}]
                             :life     10}
                            {:breaches [{:prepped-spells [ignite]}]
                             :life     10}]}
                 draw-nemesis-card
                 (choose {:player-no 1 :breach-no 0 :card-name :ignite}))
             {:nemesis {:discard [eclipse]
                        :cloaks  8}
              :players [{:breaches [{:prepped-spells [spark]}]
                         :life     10}
                        {:breaches [{}]
                         :life     8}]
              :trash   [ignite]}))
      (is (= (-> {:nemesis {:deck   [eclipse]
                            :cloaks 4}
                  :players [{:breaches [{}]
                             :life     10}
                            {:breaches [{}]
                             :life     10}]}
                 draw-nemesis-card
                 (choose {:player-no 1}))
             {:nemesis {:discard [eclipse]
                        :cloaks  8}
              :players [{:breaches [{}]
                         :life     10}
                        {:breaches [{}]
                         :life     8}]})))))

(deftest enshroud-test
  (testing "Enshroud"
    (let [spark (assoc spark :id 1)]
      (is (= (-> {:nemesis {:deck   [enshroud]
                            :cloaks 3}
                  :players [{:breaches [{:prepped-spells [spark]}]}
                            {:breaches [{:prepped-spells [spark]}]}]}
                 draw-nemesis-card
                 (choose [{:player-no 0 :breach-no 0 :card-name :spark}
                          {:player-no 1 :breach-no 0 :card-name :spark}]))
             {:nemesis {:discard [enshroud]
                        :cloaks  7}
              :players [{:breaches [{}]}
                        {:breaches [{}]}]
              :trash   [spark spark]}))
      (is (= (-> {:nemesis {:deck   [enshroud]
                            :cloaks 4}
                  :players [{:breaches [{:prepped-spells [spark]}]}
                            {:breaches [{:prepped-spells [ignite]}]}]}
                 draw-nemesis-card
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:nemesis {:discard [enshroud]
                        :cloaks  8}
              :players [{:breaches [{}]}
                        {:breaches [{:prepped-spells [ignite]}]}]
              :trash   [spark]}))
      (is (= (-> {:nemesis {:deck   [enshroud]
                            :cloaks 5}
                  :players [{:breaches [{:prepped-spells [ignite]}]}
                            {:breaches [{:prepped-spells [ignite]}]}]}
                 draw-nemesis-card)
             {:nemesis {:discard [enshroud]
                        :cloaks  8}
              :players [{:breaches [{:prepped-spells [ignite]}]}
                        {:breaches [{:prepped-spells [ignite]}]}]})))))

(deftest rising-dark-test
  (testing "Rising Dark"
    (testing "To Discard"
      (is (= (-> {:nemesis {:play-area [rising-dark]
                            :shield    ::magus-of-cloaks/shield
                            :when-hit  [[::magus-of-cloaks/when-hit]]
                            :cloaks    4
                            :life      30}}
                 (deal-damage 4))
             {:nemesis {:play-area [rising-dark]
                        :shield    ::magus-of-cloaks/shield
                        :when-hit  [[::magus-of-cloaks/when-hit]]
                        :cloaks    3
                        :life      30}}))
      (is (= (-> {:nemesis {:play-area [rising-dark]
                            :shield    ::magus-of-cloaks/shield
                            :when-hit  [[::magus-of-cloaks/when-hit]]
                            :cloaks    3
                            :life      30}}
                 (deal-damage 4))
             {:nemesis {:discard  [rising-dark]
                        :shield   ::magus-of-cloaks/shield
                        :when-hit [[::magus-of-cloaks/when-hit]]
                        :cloaks   2
                        :life     29}})))
    (testing "Power"
      (is (= (-> {:nemesis   {:play-area [(assoc-in rising-dark [:power :power] 1)]
                              :unleash   [[:damage-gravehold 1]]}
                  :gravehold {:life 30}
                  :players   [{:breaches [{:prepped-spells [spark]}]
                               :life     10}]}
                 resolve-nemesis-cards-in-play
                 (choose {:player-no 0})
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:nemesis   {:discard [(assoc-in rising-dark [:power :power] 0)]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 29}
              :players   [{:breaches [{}]
                           :life     7}]
              :trash     [spark]}))
      (is (= (-> {:nemesis   {:play-area [(assoc-in rising-dark [:power :power] 1)]
                              :unleash   [[:damage-gravehold 1]]}
                  :gravehold {:life 30}
                  :players   [{:breaches [{:prepped-spells [ignite]}]
                               :life     10}
                              {:breaches [{}]
                               :life     10}]}
                 resolve-nemesis-cards-in-play
                 (choose {:player-no 1}))
             {:nemesis   {:discard [(assoc-in rising-dark [:power :power] 0)]
                          :unleash [[:damage-gravehold 1]]}
              :gravehold {:life 29}
              :players   [{:breaches [{:prepped-spells [ignite]}]
                           :life     10}
                          {:breaches [{}]
                           :life     7}]})))))

(deftest twilight-empire-test
  (testing "Twilight Empire"
    (is (= (-> {:nemesis   {:play-area [(assoc-in twilight-empire [:power :power] 1)]
                            :cloaks    4}
                :gravehold {:life 30}}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in twilight-empire [:power :power] 0)]
                        :cloaks  5}
            :gravehold {:life 26}}))
    (is (= (-> {:nemesis   {:play-area [(assoc-in twilight-empire [:power :power] 1)]
                            :cloaks    8}
                :gravehold {:life 30}}
               resolve-nemesis-cards-in-play)
           {:nemesis   {:discard [(assoc-in twilight-empire [:power :power] 0)]
                        :cloaks  8}
            :gravehold {:life 22}}))))

(deftest veil-daughter-test
  (testing "Veil Daughter"
    (is (= (-> {:nemesis {:play-area [veil-daughter]}
                :players [{:ability {:charges 0}
                           :life    10}
                          {:ability {:charges 0}
                           :life    10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [veil-daughter]}
            :players [{:ability {:charges 0}
                       :life    6}
                      {:ability {:charges 0}
                       :life    10}]}))
    (is (= (-> {:nemesis {:play-area [veil-daughter]}
                :players [{:ability {:charges 0}
                           :life    10}
                          {:ability {:charges 1}
                           :life    10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 1}))
           {:nemesis {:play-area [veil-daughter]}
            :players [{:ability {:charges 0}
                       :life    10}
                      {:ability {:charges 1}
                       :life    6}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [veil-daughter]}
                               :players [{:ability {:charges 0}
                                          :life    10}
                                         {:ability {:charges 1}
                                          :life    10}]}
                              (resolve-nemesis-cards-in-play)
                              (choose {:player-no 0}))))
    (testing "Taking damage"
      (is (= (-> {:nemesis {:play-area [(assoc veil-daughter :life 4)]
                            :cloaks    2}}
                 (deal-damage 2)
                 (choose {:area :minions :player-no 0 :card-name :veil-daughter}))
             {:nemesis {:play-area [(assoc veil-daughter :life 4)]
                        :cloaks    2}}))
      (is (= (-> {:nemesis {:play-area [(assoc veil-daughter :life 4)]
                            :cloaks    2}}
                 (deal-damage 3)
                 (choose {:area :minions :player-no 0 :card-name :veil-daughter}))
             {:nemesis {:play-area [(assoc veil-daughter :life 3)]
                        :cloaks    2}}))
      (is (= (-> {:nemesis {:play-area [(assoc veil-daughter :life 4)]
                            :cloaks    3}}
                 (deal-damage 3)
                 (choose {:area :minions :player-no 0 :card-name :veil-daughter}))
             {:nemesis {:play-area [(assoc veil-daughter :life 4)]
                        :cloaks    3}}))
      (is (= (-> {:nemesis {:play-area [(assoc veil-daughter :life 4)]
                            :cloaks    4}}
                 (deal-damage 7)
                 (choose {:area :minions :player-no 0 :card-name :veil-daughter}))
             {:nemesis {:play-area [(assoc veil-daughter :life 1)]
                        :cloaks    4}})))))
