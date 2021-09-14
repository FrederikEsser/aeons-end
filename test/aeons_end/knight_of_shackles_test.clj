(ns aeons-end.knight-of-shackles-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer [unfocus-nemesis-breach]]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.knight-of-shackles :as knight-of-shackles :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer []]
            [aeons-end.cards.relic :refer []]
            [aeons-end.cards.spell :refer []]
            [aeons-end.mages :refer []]
            [aeons-end.cards.attack :refer []]
            [aeons-end.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 124 (f)))

(use-fixtures :each fixture)

(defn- open [breach]
  (-> breach
      (assoc :status :opened)
      (dissoc :cost :stage)))

(deftest knight-of-shackles-test
  (testing "Knight of Shackles"
    (testing "Unleash"
      (ut/reset-ids! 0)
      (is (= (-> {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                              :breaches [{:status  :closed
                                          :cost    4
                                          :stage   0
                                          :effects [[:damage-gravehold 1]]}
                                         {:status :closed
                                          :cost   5
                                          :stage  2}]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                          :breaches [{:status  :closed
                                      :cost    4
                                      :stage   1
                                      :effects [[:damage-gravehold 1]]}
                                     {:status :closed
                                      :cost   5
                                      :stage  2}]}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                              :breaches [{:status  :closed
                                          :cost    4
                                          :stage   3
                                          :effects [[:damage-gravehold 1]]}]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                          :breaches [{:status  :opened
                                      :effects [[:damage-gravehold 1]]}]}
              :gravehold {:life 29}}))
      (is (= (-> {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                              :breaches [{:status  :opened
                                          :effects [[:damage-gravehold 1]]}
                                         {:status  :closed
                                          :cost    4
                                          :stage   1
                                          :effects [[:damage-gravehold 2]]}]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                          :breaches [{:status  :opened
                                      :effects [[:damage-gravehold 1]]}
                                     {:status  :closed
                                      :cost    4
                                      :stage   2
                                      :effects [[:damage-gravehold 2]]}]}
              :gravehold {:life 30}}))
      (is (= (-> {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                              :breaches [{:status  :opened
                                          :effects [[:damage-gravehold 1]]}
                                         {:status  :closed
                                          :cost    4
                                          :stage   3
                                          :effects [[:damage-gravehold 2]]}]}
                  :gravehold {:life 30}}
                 unleash)
             {:nemesis   {:unleash  [[::knight-of-shackles/unleash]]
                          :breaches [{:status  :opened
                                      :effects [[:damage-gravehold 1]]}
                                     {:status  :opened
                                      :effects [[:damage-gravehold 2]]}]}
              :gravehold {:life 28}})))
    (testing "Unfocus breaches"
      (is (= (-> {:nemesis {:breaches [{:status :closed
                                        :cost   4
                                        :stage  3}]}
                  :players [{:aether 4}]}
                 (unfocus-nemesis-breach 0 0))
             {:nemesis {:breaches [{:status :closed
                                    :cost   4
                                    :stage  2}]}
              :players [{:aether 0}]}))
      (is (= (-> {:nemesis {:breaches [{:status :closed
                                        :cost   4
                                        :stage  1}]}
                  :players [{:aether 4}]}
                 (unfocus-nemesis-breach 0 0))
             {:nemesis {:breaches [{:status :closed
                                    :cost   4
                                    :stage  0}]}
              :players [{:aether 0}]}))
      (is (thrown-with-msg? AssertionError #"Unfocus error:"
                            (-> {:nemesis {:breaches [{:status :closed
                                                       :cost   4
                                                       :stage  0}]}
                                 :players [{:aether 4}]}
                                (unfocus-nemesis-breach 0 0))))
      (is (thrown-with-msg? AssertionError #"Unfocus error:"
                            (-> {:nemesis {:breaches [{:status :opened
                                                       :cost   0}]}
                                 :players [{:aether 4}]}
                                (unfocus-nemesis-breach 0 0))))
      (is (thrown-with-msg? AssertionError #"Pay error:"
                            (-> {:nemesis {:breaches [{:status :closed
                                                       :cost   4
                                                       :stage  3}]}
                                 :players [{:aether 3}]}
                                (unfocus-nemesis-breach 0 0)))))
    (testing "Breach open effects"
      (is (= (-> {:nemesis {:unleash  [[::knight-of-shackles/unleash]]
                            :breaches [(assoc breach-1 :stage 3)]}
                  :players [{:life 10}
                            {:life 10}]}
                 unleash
                 (choose [{:player-no 0} {:player-no 1}]))
             {:nemesis {:unleash  [[::knight-of-shackles/unleash]]
                        :breaches [(open breach-1)]}
              :players [{:life 8}
                        {:life 8}]}))
      (is (= (-> {:nemesis {:unleash  [[::knight-of-shackles/unleash]]
                            :breaches [(assoc breach-1 :stage 3)]}
                  :players [{:life 10}]}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis {:unleash  [[::knight-of-shackles/unleash]]
                        :breaches [(open breach-1)]}
              :players [{:life 6}]})))))

(deftest chainsworn-test
  (testing "Chainsworn"
    (testing "Immediately"
      (is (= (-> {:nemesis {:deck     [chainsworn]
                            :breaches [{:status :opened}
                                       {:status :opened}
                                       {:status :closed :stage 2}]}}
                 draw-nemesis-card)
             {:nemesis {:play-area [(assoc chainsworn :max-life 10)]
                        :breaches  [{:status :opened}
                                    {:status :opened}
                                    {:status :closed :stage 3}]}}))
      (is (= (-> {:nemesis {:deck     [chainsworn]
                            :breaches [{:status :opened}
                                       {:status :opened}
                                       {:status :closed :stage 3}]}}
                 draw-nemesis-card)
             {:nemesis {:play-area [(assoc chainsworn :max-life 10)]
                        :breaches  [{:status :opened}
                                    {:status :opened}
                                    {:status :opened}]}}))
      (is (= (-> {:nemesis {:deck     [chainsworn]
                            :breaches [{:status :opened}
                                       {:status :opened}
                                       {:status :opened}]}}
                 draw-nemesis-card)
             {:nemesis {:play-area [(assoc chainsworn :max-life 10)]
                        :breaches  [{:status :opened}
                                    {:status :opened}
                                    {:status :opened}]}})))
    (is (= (-> {:nemesis {:play-area [chainsworn]
                          :breaches  [{:status :closed :stage 1}]}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [chainsworn]
                      :breaches  [{:status :closed :stage 1}]}
            :players [{:life 9}]}))
    (is (= (-> {:nemesis {:play-area [chainsworn]
                          :breaches  [{:status :opened}
                                      {:status :closed :stage 2}]}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [chainsworn]
                      :breaches  [{:status :opened}
                                  {:status :closed :stage 2}]}
            :players [{:life 8}]}))
    (is (= (-> {:nemesis {:play-area [chainsworn]
                          :breaches  [{:status :opened}
                                      {:status :closed :stage 3}
                                      {:status :opened}]}
                :players [{:life 10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:nemesis {:play-area [chainsworn]
                      :breaches  [{:status :opened}
                                  {:status :closed :stage 3}
                                  {:status :opened}]}
            :players [{:life 7}]}))))

(deftest engine-of-war-test
  (testing "Engine of War"
    (is (= (-> {:nemesis {:play-area [(assoc-in engine-of-war [:power :power] 1)]
                          :breaches  [{:status :closed :stage 1}]
                          :unleash   [[::knight-of-shackles/unleash]]}
                :players [{:life 10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0}))
           {:nemesis {:discard  [(assoc-in engine-of-war [:power :power] 0)]
                      :breaches [{:status :closed :stage 2}]
                      :unleash  [[::knight-of-shackles/unleash]]}
            :players [{:life 9}]}))
    (is (= (-> {:nemesis {:play-area [(assoc-in engine-of-war [:power :power] 1)]
                          :breaches  [{:status :opened}
                                      {:status :closed :stage 2}]
                          :unleash   [[::knight-of-shackles/unleash]]}
                :players [{:life 10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0}))
           {:nemesis {:discard  [(assoc-in engine-of-war [:power :power] 0)]
                      :breaches [{:status :opened}
                                 {:status :closed :stage 3}]
                      :unleash  [[::knight-of-shackles/unleash]]}
            :players [{:life 7}]}))
    (is (= (-> {:nemesis {:play-area [(assoc-in engine-of-war [:power :power] 1)]
                          :breaches  [{:status :opened}
                                      {:status :closed :stage 3}
                                      {:status :opened}]
                          :unleash   [[::knight-of-shackles/unleash]]}
                :players [{:life 10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0}))
           {:nemesis {:discard  [(assoc-in engine-of-war [:power :power] 0)]
                      :breaches [{:status :opened}
                                 {:status :opened}
                                 {:status :opened}]
                      :unleash  [[::knight-of-shackles/unleash]]}
            :players [{:life 5}]}))))

(deftest fellblade-test
  (testing "Fellblade"
    (is (= (-> {:nemesis   {:play-area [fellblade]
                            :breaches  [{:status :closed}
                                        {:status :closed :stage 0}]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play)
               (choose nil))
           {:nemesis   {:play-area [fellblade]
                        :breaches  [{:status :closed}
                                    {:status :closed :stage 0}]}
            :gravehold {:life 27}}))
    (is (= (-> {:nemesis   {:play-area [fellblade]
                            :breaches  [{:status :closed}
                                        {:status :closed :stage 0}]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play)
               (choose {:breach-no 1}))
           {:nemesis   {:play-area [fellblade]
                        :breaches  [{:status :closed}
                                    {:status :closed :stage 1}]}
            :gravehold {:life 30}}))
    (is (= (-> {:nemesis   {:play-area [fellblade]
                            :breaches  [{:status :closed}
                                        {:status  :closed
                                         :stage   3
                                         :effects [[:damage-gravehold 7]]}]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play)
               (choose {:breach-no 1}))
           {:nemesis   {:play-area [fellblade]
                        :breaches  [{:status :closed}
                                    {:status  :opened
                                     :effects [[:damage-gravehold 7]]}]}
            :gravehold {:life 23}}))
    (is (= (-> {:nemesis   {:play-area [fellblade]
                            :breaches  [{:status :closed}
                                        {:status :opened}]}
                :gravehold {:life 30}}
               (resolve-nemesis-cards-in-play))
           {:nemesis   {:play-area [fellblade]
                        :breaches  [{:status :closed}
                                    {:status :opened}]}
            :gravehold {:life 27}}))))

(deftest invade-test
  (testing "Invade"
    (is (= (-> {:nemesis {:deck     [invade]
                          :breaches [{:status :opened}
                                     {:status :closed}]}}
               draw-nemesis-card)
           {:nemesis {:discard  [invade]
                      :breaches [{:status :opened}
                                 {:status :opened}]}}))
    (is (= (-> {:nemesis {:deck     [invade]
                          :discard  [march-on-gravehold fellblade engine-of-war chainsworn]
                          :breaches [{:status :closed}
                                     {:status :opened}]}}
               draw-nemesis-card
               (choose :engine-of-war))
           {:nemesis {:play-area [engine-of-war]
                      :discard   [march-on-gravehold fellblade chainsworn invade]
                      :breaches  [{:status :closed}
                                  {:status :opened}]}}))
    (is (= (-> {:nemesis {:deck     [invade]
                          :discard  [march-on-gravehold fellblade engine-of-war chainsworn]
                          :breaches [{:status :opened}
                                     {:status :opened}]}}
               draw-nemesis-card
               (choose :engine-of-war)
               (choose :march-on-gravehold))
           {:nemesis {:play-area [engine-of-war march-on-gravehold]
                      :discard   [fellblade chainsworn invade]
                      :breaches  [{:status :opened}
                                  {:status :opened}]}}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:deck     [invade]
                                         :discard  [march-on-gravehold fellblade engine-of-war chainsworn]
                                         :breaches [{:status :closed}
                                                    {:status :opened}]}}
                              draw-nemesis-card
                              (choose :chainsworn))))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:deck     [invade]
                                         :discard  [march-on-gravehold fellblade engine-of-war chainsworn]
                                         :breaches [{:status :closed}
                                                    {:status :opened}]}}
                              draw-nemesis-card
                              (choose :march-on-gravehold))))))

(deftest march-on-gravehold-test
  (testing "March on Gravehold"
    (is (= (-> {:nemesis {:play-area [(assoc-in march-on-gravehold [:power :power] 1)]
                          :breaches  [{:status :opened}
                                      {:status :closed :stage 0}]
                          :unleash   [[::knight-of-shackles/unleash]]}
                :players [{:life 10}]}
               resolve-nemesis-cards-in-play
               (choose {:player-no 0}))
           {:nemesis {:discard  [(assoc-in march-on-gravehold [:power :power] 0)]
                      :breaches [{:status :opened}
                                 {:status :closed :stage 0}]
                      :unleash  [[::knight-of-shackles/unleash]]}
            :players [{:life 6}]}))
    (is (= (-> {:nemesis {:play-area [(assoc-in march-on-gravehold [:power :power] 1)]
                          :breaches  [{:status :closed :stage 0}]
                          :unleash   [[::knight-of-shackles/unleash]]}
                :players [{:life 10}]}
               resolve-nemesis-cards-in-play)
           {:nemesis {:discard  [(assoc-in march-on-gravehold [:power :power] 0)]
                      :breaches [{:status :closed :stage 2}]
                      :unleash  [[::knight-of-shackles/unleash]]}
            :players [{:life 10}]}))
    (is (= (-> {:nemesis {:play-area [(assoc-in march-on-gravehold [:power :power] 1)]
                          :breaches  [{:status :closed :stage 3}
                                      {:status :closed :stage 0}]
                          :unleash   [[::knight-of-shackles/unleash]]}
                :players [{:life 10}]}
               resolve-nemesis-cards-in-play)
           {:nemesis {:discard  [(assoc-in march-on-gravehold [:power :power] 0)]
                      :breaches [{:status :opened}
                                 {:status :closed :stage 1}]
                      :unleash  [[::knight-of-shackles/unleash]]}
            :players [{:life 10}]}))))

(deftest rout-test
  (testing "Rout"
    (is (= (-> {:nemesis   {:deck     [rout]
                            :breaches [{:status :opened}]}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:discard  [rout]
                        :breaches [{:status :opened}]}
            :gravehold {:life 24}}))
    (is (= (-> {:nemesis   {:deck     [rout]
                            :breaches [{:status :closed :stage 0}]}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:discard  [rout]
                        :breaches [{:status :opened}]}
            :gravehold {:life 30}}))))
