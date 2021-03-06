(ns aeons-end.minion-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.minion :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [amplify-vision dark-fire ignite]]))

(deftest howling-spinners-test
  (testing "Howling Spinners"
    (is (= (-> {:nemesis {:play-area [howling-spinners]}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [howling-spinners]}
            :players [{:life 8}]}))))

(deftest labyrinth-wisp-test
  (testing "Labyrinth Wisp"
    (is (= (-> {:nemesis {:play-area [labyrinth-wisp]}
                :players [{:breaches [{:prepped-spells [spark]}]
                           :ability  {:charges 2}}
                          {:breaches [{:prepped-spells [ignite]}]
                           :ability  {:charges 0}}]}
               (resolve-nemesis-cards-in-play)
               (choose {:area :prepped-spells :player-no 0 :breach-no 0 :card-name :spark}))
           {:nemesis {:play-area [labyrinth-wisp]}
            :players [{:breaches [{}]
                       :discard  [spark]
                       :ability  {:charges 2}}
                      {:breaches [{:prepped-spells [ignite]}]
                       :ability  {:charges 0}}]}))
    (is (= (-> {:nemesis {:play-area [labyrinth-wisp]}
                :players [{:breaches [{:prepped-spells [spark]}]
                           :ability  {:charges 2}}
                          {:breaches [{:prepped-spells [ignite]}]
                           :ability  {:charges 0}}]}
               (resolve-nemesis-cards-in-play)
               (choose {:area :ability :player-no 0}))
           {:nemesis {:play-area [labyrinth-wisp]}
            :players [{:breaches [{:prepped-spells [spark]}]
                       :ability  {:charges 1}}
                      {:breaches [{:prepped-spells [ignite]}]
                       :ability  {:charges 0}}]}))
    (is (= (-> {:nemesis {:play-area [labyrinth-wisp]}
                :players [{:breaches [{:prepped-spells [spark]}]
                           :ability  {:charges 2}}
                          {:breaches [{:prepped-spells [ignite]}]
                           :ability  {:charges 0}}]}
               (resolve-nemesis-cards-in-play)
               (choose {:area :prepped-spells :player-no 1 :breach-no 0 :card-name :ignite}))
           {:nemesis {:play-area [labyrinth-wisp]}
            :players [{:breaches [{:prepped-spells [spark]}]
                       :ability  {:charges 2}}
                      {:breaches [{}]
                       :discard  [ignite]
                       :ability  {:charges 0}}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [labyrinth-wisp]}
                               :players [{:breaches [{:prepped-spells [spark]}]
                                          :ability  {:charges 2}}
                                         {:breaches [{:prepped-spells [ignite]}]
                                          :ability  {:charges 0}}]}
                              (resolve-nemesis-cards-in-play)
                              (choose {:area :ability :player-no 1}))))))

(deftest mage-ender-test
  (testing "Mage Ender"
    (is (= (-> {:nemesis {:play-area [mage-ender]}
                :players [{:breaches [{:status :opened}]
                           :life     10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [mage-ender]}
            :players [{:breaches [{:status :opened}]
                       :life     8}]}))
    (is (= (-> {:nemesis {:play-area [mage-ender]}
                :players [{:breaches [{:status :opened}]
                           :life     10}
                          {:breaches [{:status :opened}]
                           :life     10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [mage-ender]}
            :players [{:breaches [{:status :opened}]
                       :life     8}
                      {:breaches [{:status :opened}]
                       :life     10}]}))
    (is (= (-> {:nemesis {:play-area [mage-ender]}
                :players [{:breaches [{:status :opened}]
                           :life     10}
                          {:breaches [{:status :opened}]
                           :life     10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 1}))
           {:nemesis {:play-area [mage-ender]}
            :players [{:breaches [{:status :opened}]
                       :life     10}
                      {:breaches [{:status :opened}]
                       :life     8}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area [mage-ender]}
                               :players [{:breaches [{:status :opened}
                                                     {:status :opened}]
                                          :life     10}
                                         {:breaches [{:status :opened}
                                                     {:status :closed}]
                                          :life     10}]}
                              (resolve-nemesis-cards-in-play)
                              (choose {:player-no 1}))))))

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
                 (choose {:area :minions :player-no 0 :card-name :monstrosity-of-omens}))
             {:nemesis {:play-area [(assoc monstrosity-of-omens :life 3)]}
              :players [{:breaches [{}]
                         :discard  [spark]}]}))
      (is (= (-> {:nemesis {:play-area [(assoc monstrosity-of-omens :life 4)]}
                  :players [{:breaches [{:status         :opened
                                         :prepped-spells [amplify-vision]}]}]}
                 (cast-spell 0 0 :amplify-vision)
                 (choose {:area :minions :player-no 0 :card-name :monstrosity-of-omens}))
             {:nemesis {:play-area [(assoc monstrosity-of-omens :life 3)]}
              :players [{:breaches [{:status :opened}]
                         :discard  [amplify-vision]}]}))
      (is (= (-> {:nemesis {:play-area [(assoc monstrosity-of-omens :life 4)]}
                  :players [{:breaches [{:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [spark]}]}]}
                 (cast-spell 0 0 :spark)
                 (choose {:area :minions :player-no 0 :card-name :monstrosity-of-omens}))
             {:nemesis {:play-area [(assoc monstrosity-of-omens :life 3)]}
              :players [{:breaches [{:status       :opened
                                     :bonus-damage 1}]
                         :discard  [spark]}]}))
      (is (= (-> {:nemesis {:play-area [(assoc monstrosity-of-omens :life 4)]}
                  :players [{:breaches [{:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [dark-fire]}]}]}
                 (cast-spell 0 0 :dark-fire)
                 (choose {:area :minions :player-no 0 :card-name :monstrosity-of-omens}))
             {:nemesis {:play-area [(assoc monstrosity-of-omens :life 3)]}
              :players [{:breaches [{:status       :opened
                                     :bonus-damage 1}]
                         :discard  [dark-fire]}]})))))
