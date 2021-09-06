(ns aeons-end.carapace-queen-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.carapace-queen :as carapace-queen :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.spell :refer [amplify-vision dark-fire ignite]]))

(defn do-swarm [game]
  (-> game
      (push-effect-stack {:effects [[::carapace-queen/swarm]]})
      check-stack))

(deftest carapace-queen-test
  (testing "Carapace Queen"
    (testing "Unleash"
      (is (= (-> {:nemesis {:husks   {:number-of-husks 0}
                            :unleash [[::carapace-queen/unleash]]}}
                 unleash)
             {:nemesis {:husks   {:number-of-husks 2}
                        :unleash [[::carapace-queen/unleash]]}}))
      (is (= (-> {:difficulty :expert
                  :nemesis    {:husks   {:number-of-husks 0}
                               :unleash [[::carapace-queen/unleash]]}}
                 unleash)
             {:difficulty :expert
              :nemesis    {:husks   {:number-of-husks 3}
                           :unleash [[::carapace-queen/unleash]]}}))
      (is (= (-> {:nemesis   {:husks   {:number-of-husks 13}
                              :unleash [[::carapace-queen/unleash]]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:husks   {:number-of-husks 15}
                          :unleash [[::carapace-queen/unleash]]}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:husks   {:number-of-husks 14}
                              :unleash [[::carapace-queen/unleash]]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:husks   {:number-of-husks 15}
                          :unleash [[::carapace-queen/unleash]]}
              :gravehold {:life 29}}))
      (is (= (-> {:nemesis   {:husks   {:number-of-husks 15}
                              :unleash [[::carapace-queen/unleash]]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:husks   {:number-of-husks 15}
                          :unleash [[::carapace-queen/unleash]]}
              :gravehold {:life 28}})))
    (testing "Damage Husks"
      (is (= (-> {:nemesis {:husks {:number-of-husks 3}}}
                 (deal-damage 1)
                 (choose {:area :minions :player-no 0 :card-name :husks}))
             {:nemesis {:husks {:number-of-husks 2}}}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 1}}}
                 (deal-damage 1)
                 (choose {:area :minions :player-no 0 :card-name :husks}))
             {:nemesis {:husks {:number-of-husks 0}}}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 0}
                            :life  60}}
                 (deal-damage 1))
             {:nemesis {:husks {:number-of-husks 0}
                        :life  59}}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 3}}
                  :players [{:life 10}]}
                 (deal-damage 2)
                 (choose {:area :minions :player-no 0 :card-name :husks})
                 (choose :husks))
             {:nemesis {:husks {:number-of-husks 1}}
              :players [{:life 9}]}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 3}}
                  :players [{:life 10}]}
                 (deal-damage 2)
                 (choose {:area :minions :player-no 0 :card-name :husks})
                 (choose nil))
             {:nemesis {:husks {:number-of-husks 2}}
              :players [{:life 10}]}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 3}}
                  :players [{:life 10}]}
                 (deal-damage 4)
                 (choose {:area :minions :player-no 0 :card-name :husks})
                 (choose :husks))
             {:nemesis {:husks {:number-of-husks 0}}
              :players [{:life 9}]}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 1}}
                  :players [{:life 10}]}
                 (deal-damage 2)
                 (choose {:area :minions :player-no 0 :card-name :husks}))
             {:nemesis {:husks {:number-of-husks 0}}
              :players [{:life 10}]})))
    (testing "Swarm"
      (is (= (-> {:nemesis {:husks {:number-of-husks 0
                                    :swarm-effects   swarm-effects}}}
                 do-swarm)
             {:nemesis {:husks {:number-of-husks 4
                                :swarm-effects   swarm-effects}}}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 3
                                    :swarm-effects   swarm-effects}}
                  :players [{:breaches [{:prepped-spells [spark]}]}]}
                 do-swarm)
             {:nemesis {:husks {:number-of-husks 7
                                :swarm-effects   swarm-effects}}
              :players [{:breaches [{:prepped-spells [spark]}]}]}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 4
                                    :swarm-effects   swarm-effects}}
                  :players [{:breaches [{:prepped-spells [spark]}]}]}
                 do-swarm
                 (choose {:player-no 0 :breach-no 0 :card-name :spark}))
             {:nemesis {:husks {:number-of-husks 6
                                :swarm-effects   swarm-effects}}
              :players [{:breaches [{}]
                         :discard  [spark]}]}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 6
                                    :swarm-effects   swarm-effects}}
                  :players [{:breaches [{}]}]}
                 do-swarm)
             {:nemesis {:husks {:number-of-husks 8
                                :swarm-effects   swarm-effects}}
              :players [{:breaches [{}]}]}))
      (is (= (-> {:nemesis   {:husks {:number-of-husks 7
                                      :swarm-effects   swarm-effects}}
                  :gravehold {:life 30}}
                 do-swarm)
             {:nemesis   {:husks {:number-of-husks 7
                                  :swarm-effects   swarm-effects}}
              :gravehold {:life 25}}))
      (is (= (-> {:nemesis   {:husks {:number-of-husks 10
                                      :swarm-effects   swarm-effects}}
                  :gravehold {:life 30}}
                 do-swarm)
             {:nemesis   {:husks {:number-of-husks 10
                                  :swarm-effects   swarm-effects}}
              :gravehold {:life 25}}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 11
                                    :swarm-effects   swarm-effects}}
                  :players [{:life 10}]}
                 do-swarm
                 (choose {:player-no 0}))
             {:nemesis {:husks {:number-of-husks 10
                                :swarm-effects   swarm-effects}}
              :players [{:life 7}]}))
      (is (= (-> {:nemesis {:husks {:number-of-husks 12
                                    :swarm-effects   swarm-effects}}
                  :players [{:life 10}
                            {:life 8}]}
                 do-swarm
                 (choose {:player-no 1}))
             {:nemesis {:husks {:number-of-husks 11
                                :swarm-effects   swarm-effects}}
              :players [{:life 10}
                        {:life 5}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis {:husks {:number-of-husks 12
                                                   :swarm-effects   swarm-effects}}
                                 :players [{:life 10}
                                           {:life 8}]}
                                do-swarm
                                (choose {:player-no 0}))))
      (is (= (-> {:real-game? true
                  :nemesis    {:husks             {:number-of-husks 13
                                                   :swarm-effects   swarm-effects}
                               :victory-condition ::carapace-queen/victory-condition}}
                 do-swarm
                 (update :game-over dissoc :text))
             {:real-game? true
              :nemesis    {:husks             {:number-of-husks 13
                                               :swarm-effects   swarm-effects
                                               :overswarming?   true}
                           :victory-condition ::carapace-queen/victory-condition}
              :game-over  {:conclusion :defeat}})))
    (testing "Broodwomb"
      (testing "Taking damage"
        (is (= (-> {:nemesis {:play-area [(assoc broodwomb :life 4)]
                              :husks     {:number-of-husks 0}}}
                   (deal-damage 1)
                   (choose {:area :minions :player-no 0 :card-name :broodwomb}))
               {:nemesis {:play-area [(assoc broodwomb :life 3)]
                          :husks     {:number-of-husks 0}}}))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:nemesis {:play-area [(assoc broodwomb :life 4)]
                                             :husks     {:number-of-husks 5}}}
                                  (deal-damage 1)
                                  (choose {:area :minions :player-no 0 :card-name :broodwomb}))
                              {:nemesis {:play-area [(assoc broodwomb :life 4)]
                                         :husks     {:number-of-husks 5}}}))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:nemesis {:play-area [(assoc broodwomb :life 4)]
                                             :husks     {:number-of-husks 1}}}
                                  (deal-damage 4)
                                  (choose {:area :minions :player-no 0 :card-name :broodwomb}))))))
    (testing "Foul Multitudes"
      (testing "Immediately"
        (is (= (-> {:nemesis {:deck    [foul-multitudes]
                              :unleash [[::carapace-queen/unleash]]
                              :husks   {:number-of-husks 0}}}
                   draw-nemesis-card)
               {:nemesis {:play-area [foul-multitudes]
                          :unleash   [[::carapace-queen/unleash]]
                          :husks     {:number-of-husks 2}}})))
      (testing "Damage husks"
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 1
                                          :damaged-husk?   true}}}
                   (deal-damage 1)
                   (choose {:area :minions :player-no 0 :card-name :husks}))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 0}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 1}}}
                   (deal-damage 1)
                   (choose {:area :minions :player-no 0 :card-name :husks}))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 1
                                      :damaged-husk?   true}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 1}}}
                   (deal-damage 2)
                   (choose {:area :minions :player-no 0 :card-name :husks}))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 0}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 1}}}
                   (deal-damage 3)
                   (choose {:area :minions :player-no 0 :card-name :husks}))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 0}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2
                                          :damaged-husk?   true}}}
                   (deal-damage 1)
                   (choose {:area :minions :player-no 0 :card-name :husks}))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 1}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2
                                          :damaged-husk?   true}}}
                   (deal-damage 2)
                   (choose {:area :minions :player-no 0 :card-name :husks}))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 1
                                      :damaged-husk?   true}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2
                                          :damaged-husk?   true}}}
                   (deal-damage 3)
                   (choose {:area :minions :player-no 0 :card-name :husks})
                   (choose nil))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 1
                                      :damaged-husk?   true}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2
                                          :damaged-husk?   true}}
                    :players [{:life 10}]}
                   (deal-damage 3)
                   (choose {:area :minions :player-no 0 :card-name :husks})
                   (choose :husks))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 0}}
                :players [{:life 9}]}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2
                                          :damaged-husk?   true}}
                    :players [{:life 10}]}
                   (deal-damage 4)
                   (choose {:area :minions :player-no 0 :card-name :husks})
                   (choose :husks))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 0}}
                :players [{:life 9}]}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2
                                          :damaged-husk?   true}}
                    :players [{:life 10}]}
                   (deal-damage 5)
                   (choose {:area :minions :player-no 0 :card-name :husks})
                   (choose :husks))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 0}}
                :players [{:life 9}]}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2}}}
                   (deal-damage 2)
                   (choose {:area :minions :player-no 0 :card-name :husks}))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 1}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2}}}
                   (deal-damage 3)
                   (choose {:area :minions :player-no 0 :card-name :husks})
                   (choose nil))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 1}}}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2}}
                    :players [{:life 10}]}
                   (deal-damage 3)
                   (choose {:area :minions :player-no 0 :card-name :husks})
                   (choose :husks))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 1
                                      :damaged-husk?   true}}
                :players [{:life 9}]}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2}}
                    :players [{:life 10}]}
                   (deal-damage 4)
                   (choose {:area :minions :player-no 0 :card-name :husks})
                   (choose :husks))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 0}}
                :players [{:life 9}]}))
        (is (= (-> {:nemesis {:play-area [foul-multitudes]
                              :husks     {:number-of-husks 2
                                          :damaged-husk?   true}}
                    :players [{:phase :draw}]}
                   (end-turn 0))
               {:nemesis {:play-area [foul-multitudes]
                          :husks     {:number-of-husks 2}}
                :players [{:phase :out-of-turn}]})))
      (testing "Resolve power"
        (is (= (-> {:nemesis {:play-area [(assoc-in foul-multitudes [:power :power] 1)]
                              :husks     {:number-of-husks 2
                                          :swarm-effects   swarm-effects}}}
                   resolve-nemesis-cards-in-play)
               {:nemesis {:discard [(assoc-in foul-multitudes [:power :power] 0)]
                          :husks   {:number-of-husks 6
                                    :swarm-effects   swarm-effects}}}))))))
