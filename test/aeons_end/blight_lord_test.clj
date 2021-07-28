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

(deftest glittering-doom-test
  (testing "Glittering Doom"
    (is (= (-> {:nemesis {:play-area     [(assoc-in glittering-doom [:power :power] 1)]
                          :tainted-jades [tainted-jade tainted-jade tainted-jade]}
                :players [{:hand [crystal crystal tainted-jade spark spark]}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0})
               (choose [:crystal :spark :tainted-jade]))
           {:nemesis {:discard       [(assoc-in glittering-doom [:power :power] 0)]
                      :tainted-jades []}
            :players [{:hand    [crystal spark tainted-jade tainted-jade tainted-jade]
                       :discard [crystal spark tainted-jade]}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:play-area     [(assoc-in glittering-doom [:power :power] 1)]
                                         :tainted-jades [tainted-jade]}
                               :players [{:hand [crystal crystal]
                                          :life 10}
                                         {:hand [crystal crystal crystal]}]}
                              resolve-nemesis-cards-in-play
                              (choose {:player-no 0}))))
    (is (= (-> {:nemesis {:play-area     [(assoc-in glittering-doom [:power :power] 1)]
                          :tainted-jades [tainted-jade]}
                :players [{:hand [crystal crystal]
                           :life 10}
                          {:hand [crystal crystal]}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0})
               (choose [:crystal :crystal]))
           {:nemesis {:discard       [(assoc-in glittering-doom [:power :power] 0)]
                      :tainted-jades []}
            :players [{:hand    [tainted-jade]
                       :discard [crystal crystal]
                       :life    8}
                      {:hand [crystal crystal]}]}))))

(deftest ossify-test
  (testing "Ossify"
    (is (= (-> {:nemesis {:deck          [ossify]
                          :tainted-jades [tainted-jade tainted-jade tainted-jade]}
                :players [{}
                          {}]}
               draw-nemesis-card
               (choose [{:player-no 0}
                        {:player-no 0}
                        {:player-no 1}]))
           {:nemesis {:discard       [ossify]
                      :tainted-jades []}
            :players [{:deck [tainted-jade tainted-jade]}
                      {:deck [tainted-jade]}]}))
    (is (= (-> {:nemesis {:deck          [ossify]
                          :tainted-jades [tainted-jade]}
                :players [{:life 10}
                          {:life 10}]}
               draw-nemesis-card
               (choose [{:player-no 0}
                        {:player-no 0}
                        {:player-no 1}]))
           {:nemesis {:discard       [ossify]
                      :tainted-jades []}
            :players [{:deck [tainted-jade]
                       :life 9}
                      {:life 9}]}))))

(deftest petrify-test
  (testing "Petrify"
    (is (= (-> {:nemesis   {:deck          [petrify]
                            :tainted-track {:tainted-level 1}}
                :gravehold {:life 30}}
               draw-nemesis-card
               (choose :advance)
               (choose :tainted-track)
               (choose :tainted-track))
           {:nemesis   {:discard       [petrify]
                        :tainted-track {:tainted-level 3}}
            :gravehold {:life 30}}))
    (is (= (-> {:nemesis   {:deck          [petrify]
                            :tainted-track {:tainted-level 1}}
                :gravehold {:life 30}}
               draw-nemesis-card
               (choose :damage))
           {:nemesis   {:discard       [petrify]
                        :tainted-track {:tainted-level 1}}
            :gravehold {:life 22}}))))

(deftest shard-spitter-test
  (testing "Shard Spitter"
    (is (= (-> {:nemesis   {:play-area     [shard-spitter]
                            :tainted-jades [tainted-jade]}
                :gravehold {:life 30}
                :players   [{}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis   {:play-area     [shard-spitter]
                        :tainted-jades []}
            :gravehold {:life 28}
            :players   [{:deck [tainted-jade]}]}))))

(deftest slag-horror-test
  (testing "Slag Horror"
    (is (= (-> {:nemesis {:play-area     [slag-horror]
                          :tainted-jades [tainted-jade tainted-jade]
                          :tainted-track {:tainted-level 1}}
                :players [{}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0})
               (choose :tainted-track))
           {:nemesis {:play-area     [slag-horror]
                      :tainted-jades []
                      :tainted-track {:tainted-level 2}}
            :players [{:discard [tainted-jade tainted-jade]}]}))))

(deftest verdigra-test
  (testing "Verdigra"
    (is (= (-> {:nemesis {:play-area     [verdigra]
                          :tainted-track {:tainted-level 1}}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area     [verdigra]
                      :tainted-track {:tainted-level 1}}
            :players [{:life 7}]}))
    (is (= (-> {:nemesis {:play-area     [(assoc verdigra :life 8)]
                          :tainted-track {:tainted-level 1}}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0})
               (choose :tainted-track))
           {:nemesis {:play-area     [(assoc verdigra :life 8)]
                      :tainted-track {:tainted-level 2}}
            :players [{:life 7}]}))))

(deftest vitrify-test
  (testing "Vitrify"
    (is (= (-> {:nemesis {:deck          [vitrify]
                          :tainted-jades [tainted-jade tainted-jade]}
                :players [{}
                          {}]}
               draw-nemesis-card
               (choose [{:player-no 0}
                        {:player-no 0}]))
           {:nemesis {:discard       [vitrify]
                      :tainted-jades []}
            :players [{:hand [tainted-jade tainted-jade]}
                      {}]}))
    (is (= (-> {:nemesis {:deck          [vitrify]
                          :tainted-jades [tainted-jade tainted-jade]}
                :players [{}]}
               draw-nemesis-card
               (choose [{:player-no 0}
                        {:player-no 0}]))
           {:nemesis {:discard       [vitrify]
                      :tainted-jades []}
            :players [{:hand [tainted-jade tainted-jade]}]}))
    (is (= (-> {:nemesis {:deck          [vitrify]
                          :tainted-jades [tainted-jade]}
                :players [{}
                          {:life 10}]}
               draw-nemesis-card
               (choose [{:player-no 0}
                        {:player-no 1}]))
           {:nemesis {:discard       [vitrify]
                      :tainted-jades []}
            :players [{:hand [tainted-jade]}
                      {:life 9}]}))))
