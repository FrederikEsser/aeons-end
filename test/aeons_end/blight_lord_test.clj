(ns aeons-end.blight-lord-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.blight-lord :as blight-lord :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer []]
            [aeons-end.cards.relic :refer [mages-totem]]
            [aeons-end.cards.spell :refer []]))

(defn fixture [f]
  (with-rand-seed 124 (f)))

(use-fixtures :each fixture)

(defn do-start-turn [game]
  (-> game
      (push-effect-stack {:effects [[::blight-lord/at-start-turn]]})
      check-stack))

(defn do-advance-tainted-track-test [game]
  (-> game
      (do-advance-tainted-track {})
      check-stack))

(deftest tainted-jade-test
  (testing "Tainted Jade"
    (let [tainted-jade (assoc tainted-jade :id 1)]
      (is (= (-> {:players [{:hand   [tainted-jade]
                             :aether 0
                             :life   10}]}
                 (play 0 :tainted-jade))
             {:players [{:play-area [tainted-jade]
                         :aether    2
                         :life      9}]}))
      (is (= (-> {:players [{:hand   [tainted-jade]
                             :aether 2
                             :life   10}]}
                 (play 0 :tainted-jade)
                 (choose :aether))
             {:players [{:play-area [tainted-jade]
                         :aether    4
                         :life      9}]}))
      (is (= (-> {:players [{:hand   [tainted-jade]
                             :aether 2
                             :life   10}]
                  :nemesis {:tainted-jades []}}
                 (play 0 :tainted-jade)
                 (choose :destroy))
             {:players [{:aether 0
                         :life   10}]
              :nemesis {:tainted-jades [tainted-jade]}}))
      (is (= (-> {:players [{:hand      [mages-totem]
                             :play-area [tainted-jade]
                             :aether    2
                             :life      9}]
                  :nemesis {:tainted-jades []}}
                 (play 0 :mage's-totem)
                 (choose :tainted-jade))
             {:players [{:play-area [mages-totem]
                         :aether    2
                         :life      8}]
              :nemesis {:tainted-jades [tainted-jade]}})))))

(deftest blight-lord-test
  (testing "Blight Lord"
    (testing "Unleash"
      (let [tainted-jade (assoc tainted-jade :id 1)]
        (is (= (-> {:nemesis {:tainted-jades [tainted-jade]
                              :unleash       [[::blight-lord/unleash]]}
                    :players [{}]}
                   unleash
                   (choose {:player-no 0}))
               {:nemesis {:tainted-jades []
                          :unleash       [[::blight-lord/unleash]]}
                :players [{:discard [tainted-jade]}]}))
        (is (= (-> {:nemesis {:tainted-jades []
                              :unleash       [[::blight-lord/unleash]]}
                    :players [{:life 10}]}
                   unleash
                   (choose {:player-no 0}))
               {:nemesis {:tainted-jades []
                          :unleash       [[::blight-lord/unleash]]}
                :players [{:life 9}]}))))
    (testing "Start of turn"
      (is (= (-> {:nemesis {:tainted-jades [tainted-jade tainted-jade]
                            :tainted-track {:tainted-level 1}}}
                 do-start-turn)
             {:nemesis {:tainted-jades [tainted-jade tainted-jade]
                        :tainted-track {:tainted-level 1}}}))
      (is (= (-> {:nemesis {:tainted-jades [tainted-jade]
                            :tainted-track {:tainted-level 1}}}
                 do-start-turn
                 (choose :tainted-track))
             {:nemesis {:tainted-jades [tainted-jade]
                        :tainted-track {:tainted-level 2}}}))
      (is (= (-> {:nemesis {:tainted-jades []
                            :tainted-track {:tainted-level 1}}}
                 do-start-turn
                 (choose :tainted-track)
                 (choose :tainted-track))
             {:nemesis {:tainted-jades []
                        :tainted-track {:tainted-level 3}}})))
    (testing "Tainted Track"
      (is (= (-> {:nemesis {:tainted-track {:tainted-level   1
                                            :tainted-effects tainted-effects}}}
                 do-advance-tainted-track-test)
             {:nemesis {:tainted-track {:tainted-level   2
                                        :tainted-effects tainted-effects}}}))
      (is (= (-> {:nemesis   {:tainted-track {:tainted-level   2
                                              :tainted-effects tainted-effects}}
                  :gravehold {:life 30}}
                 do-advance-tainted-track-test)
             {:nemesis   {:tainted-track {:tainted-level   3
                                          :tainted-effects tainted-effects}}
              :gravehold {:life 23}}))
      (is (= (-> {:nemesis {:tainted-track {:tainted-level   3
                                            :tainted-effects tainted-effects}}}
                 do-advance-tainted-track-test)
             {:nemesis {:tainted-track {:tainted-level   4
                                        :tainted-effects tainted-effects}}}))
      (is (= (-> {:nemesis {:tainted-track {:tainted-level   4
                                            :tainted-effects tainted-effects}}
                  :players [{:life 9}
                            {:life 10}]}
                 do-advance-tainted-track-test
                 (choose {:player-no 0}))
             {:nemesis {:tainted-track {:tainted-level   5
                                        :tainted-effects tainted-effects}}
              :players [{:life 5}
                        {:life 10}]}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis {:tainted-track {:tainted-level   4
                                                           :tainted-effects tainted-effects}}
                                 :players [{:life 9}
                                           {:life 10}]}
                                do-advance-tainted-track-test
                                (choose {:player-no 1}))))
      (is (= (-> {:nemesis {:tainted-track {:tainted-level   5
                                            :tainted-effects tainted-effects}}}
                 do-advance-tainted-track-test)
             {:nemesis {:tainted-track {:tainted-level   6
                                        :tainted-effects tainted-effects}}}))
      (is (= (-> {:nemesis {:tainted-track {:tainted-level   6
                                            :tainted-effects tainted-effects}
                            :life          50
                            :max-life      70}}
                 do-advance-tainted-track-test)
             {:nemesis {:tainted-track {:tainted-level   7
                                        :tainted-effects tainted-effects}
                        :life          60
                        :max-life      70}}))
      (is (= (-> {:nemesis {:tainted-track {:tainted-level   6
                                            :tainted-effects tainted-effects}
                            :life          61
                            :max-life      70}}
                 do-advance-tainted-track-test)
             {:nemesis {:tainted-track {:tainted-level   7
                                        :tainted-effects tainted-effects}
                        :life          70
                        :max-life      70}}))
      (is (= (-> {:nemesis {:tainted-track {:tainted-level   7
                                            :tainted-effects tainted-effects}}}
                 do-advance-tainted-track-test)
             {:nemesis {:tainted-track {:tainted-level   8
                                        :tainted-effects tainted-effects}}}))
      (is (= (-> {:real-game? true
                  :nemesis    {:tainted-track     {:tainted-level   8
                                                   :tainted-effects tainted-effects}
                               :victory-condition ::blight-lord/victory-condition}}
                 do-advance-tainted-track-test
                 (update :game-over dissoc :text))
             {:real-game? true
              :resolving  :tainted-track
              :nemesis    {:tainted-track     {:tainted-level    9
                                               :totally-tainted? true
                                               :tainted-effects  tainted-effects}
                           :victory-condition ::blight-lord/victory-condition}
              :game-over  {:conclusion :defeat}})))))

(deftest creeping-viridian-test
  (testing "Creeping Viridian"
    (is (= (-> {:nemesis {:play-area     [(assoc-in creeping-viridian [:power :power] 1)]
                          :tainted-jades [tainted-jade]
                          :tainted-track {:tainted-level 1}}
                :players [{}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0})
               (choose :tainted-track))
           {:nemesis {:discard       [(assoc-in creeping-viridian [:power :power] 0)]
                      :tainted-jades []
                      :tainted-track {:tainted-level 2}}
            :players [{:hand [tainted-jade]}]}))))