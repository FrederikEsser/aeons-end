(ns aeons-end.hollow-crown-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer [unfocus-nemesis-breach]]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.hollow-crown :as hollow-crown :refer :all]
            [aeons-end.cards.starter :refer :all]))

(defn fixture [f]
  (with-rand-seed 124 (f)))

(use-fixtures :each fixture)

(defn do-resolve-blood-magic [game acolyte & {:keys [avoid-self-damage]}]
  (-> game
      (push-effect-stack {:effects [[::hollow-crown/resolve-blood-magic {:card-name         acolyte
                                                                         :avoid-self-damage avoid-self-damage}]]})
      check-stack))

(deftest hollow-crown-test
  (testing "Hollow Crown"
    (testing "Unleash"
      (is (= (-> {:nemesis   {:unleash   [[::hollow-crown/unleash]]
                              :play-area [{:name        :bupu
                                           :type        :acolyte
                                           :life        11
                                           :blood-magic {:effects [[:damage-gravehold 1]]}}
                                          {:name        :son-goku
                                           :type        :acolyte
                                           :life        11
                                           :blood-magic {:effects [[:damage-gravehold 9000]]}}]}
                  :gravehold {:life 30}}
                 unleash
                 (choose :bupu))
             {:nemesis   {:unleash   [[::hollow-crown/unleash]]
                          :play-area [{:name        :bupu
                                       :type        :acolyte
                                       :life        11
                                       :blood-magic {:effects [[:damage-gravehold 1]]}}
                                      {:name        :son-goku
                                       :type        :acolyte
                                       :life        11
                                       :blood-magic {:effects [[:damage-gravehold 9000]]}}]}
              :gravehold {:life 29}}))
      (is (= (-> {:nemesis   {:unleash   [[::hollow-crown/unleash]]
                              :play-area [{:name        :bupu
                                           :type        :acolyte
                                           :life        11
                                           :blood-magic {:effects [[:damage-gravehold 1]]}}
                                          {:name        :son-goku
                                           :type        :acolyte
                                           :life        10
                                           :blood-magic {:effects [[:damage-gravehold 9000]]}}]}
                  :gravehold {:life 30}}
                 unleash
                 (choose :bupu))
             {:nemesis   {:unleash   [[::hollow-crown/unleash]]
                          :play-area [{:name        :bupu
                                       :type        :acolyte
                                       :life        11
                                       :blood-magic {:effects [[:damage-gravehold 1]]}}
                                      {:name        :son-goku
                                       :type        :acolyte
                                       :life        10
                                       :blood-magic {:effects [[:damage-gravehold 9000]]}}]}
              :gravehold {:life 29}}))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis   {:unleash   [[::hollow-crown/unleash]]
                                             :play-area [{:name        :bupu
                                                          :type        :acolyte
                                                          :life        11
                                                          :blood-magic {:effects [[:damage-gravehold 1]]}}
                                                         {:name        :son-goku
                                                          :type        :acolyte
                                                          :life        10
                                                          :blood-magic {:effects [[:damage-gravehold 9000]]}}]}
                                 :gravehold {:life 30}}
                                unleash
                                (choose :son-goku))))
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis   {:unleash   [[::hollow-crown/unleash]]
                                             :play-area [{:name        :bupu
                                                          :type        :minion
                                                          :life        11
                                                          :blood-magic {:effects [[:damage-gravehold 1]]}}
                                                         {:name        :son-goku
                                                          :type        :acolyte
                                                          :life        10
                                                          :blood-magic {:effects [[:damage-gravehold 9000]]}}]}
                                 :gravehold {:life 30}}
                                unleash
                                (choose :bupu))))
      (is (= (-> {:nemesis   {:unleash   [[::hollow-crown/unleash]]
                              :play-area [{:name        :bupu
                                           :type        :acolyte
                                           :life        10
                                           :blood-magic {:effects [[:damage-gravehold 1]]}}
                                          {:name        :son-goku
                                           :type        :acolyte
                                           :life        11
                                           :blood-magic {:effects [[:damage-gravehold 2]
                                                                   [:deal-damage-to-minion {:card-name :son-goku
                                                                                            :damage    2}]]}}]}
                  :gravehold {:life 30}}
                 unleash
                 (choose :son-goku))
             {:nemesis   {:unleash   [[::hollow-crown/unleash]]
                          :play-area [{:name        :bupu
                                       :type        :acolyte
                                       :life        10
                                       :blood-magic {:effects [[:damage-gravehold 1]]}}
                                      {:name        :son-goku
                                       :type        :acolyte
                                       :life        9
                                       :blood-magic {:effects [[:damage-gravehold 2]
                                                               [:deal-damage-to-minion {:card-name :son-goku
                                                                                        :damage    2}]]}}]}
              :gravehold {:life 28}}))
      (is (= (-> {:difficulty :expert
                  :nemesis    {:unleash   [[::hollow-crown/unleash]]
                               :play-area [{:name        :bupu
                                            :type        :acolyte
                                            :life        10
                                            :blood-magic {:effects [[:damage-gravehold 1]]}}
                                           {:name        :son-goku
                                            :type        :acolyte
                                            :life        11
                                            :blood-magic {:effects [[:damage-gravehold 2]
                                                                    [:deal-damage-to-minion {:card-name :son-goku
                                                                                             :damage    2}]]}}]}
                  :gravehold  {:life 30}}
                 unleash
                 (choose :bupu)
                 (choose :son-goku))
             {:difficulty :expert
              :nemesis    {:unleash   [[::hollow-crown/unleash]]
                           :play-area [{:name        :bupu
                                        :type        :acolyte
                                        :life        10
                                        :blood-magic {:effects [[:damage-gravehold 1]]}}
                                       {:name        :son-goku
                                        :type        :acolyte
                                        :life        11
                                        :blood-magic {:effects [[:damage-gravehold 2]
                                                                [:deal-damage-to-minion {:card-name :son-goku
                                                                                         :damage    2}]]}}]}
              :gravehold  {:life 27}})))
    (testing "Acolytes"
      (is (= (-> {:nemesis {:play-area [{:name :bupu
                                         :type :acolyte
                                         :life 11}]}}
                 (resolve-nemesis-cards-in-play :auto-resolve? false))
             {:nemesis {:play-area [{:name :bupu
                                     :type :acolyte
                                     :life 11}]}}))
      (is (= (-> {:nemesis {:life       1
                            :max-damage 0
                            :play-area  [{:name :bupu
                                          :type :acolyte
                                          :life 11}]}}
                 (deal-damage 1)
                 (choose {:area :minions :player-no 0 :card-name :bupu}))
             {:nemesis {:life       1
                        :max-damage 0
                        :play-area  [{:name :bupu
                                      :type :acolyte
                                      :life 10}]}}))
      (is (= (-> {:nemesis {:acolytes  [{:name :son-goku
                                         :type :acolyte
                                         :life 11}]
                            :play-area [{:name        :bupu
                                         :type        :acolyte
                                         :life        1
                                         :max-life    11
                                         :when-killed [[::hollow-crown/draw-acolyte]]}]}}
                 (deal-damage 1)
                 (choose {:area :minions :player-no 0 :card-name :bupu}))
             {:nemesis {:play-area [{:name     :son-goku
                                     :type     :acolyte
                                     :life     11
                                     :max-life 11}]
                        :discard   [{:name        :bupu
                                     :type        :acolyte
                                     :life        11
                                     :max-life    11
                                     :when-killed [[::hollow-crown/draw-acolyte]]}]}})))
    (testing "Additional rules"
      (is (thrown-with-msg? AssertionError #"Choose error:"
                            (-> {:nemesis {:life       1
                                           :max-damage 0
                                           :play-area  [{:name :mini
                                                         :type :minion
                                                         :life 11}]}}
                                (deal-damage 1)
                                (choose {:area :nemesis :player-no 0 :card-name :nemesis}))))
      (is (= (-> {:nemesis {:life       1
                            :max-damage 0}}
                 (deal-damage 1))
             {:nemesis {:life       1
                        :max-damage 0}}))
      (is (= (-> {:players   [{:breaches [{:status     :closed
                                           :focus-cost 2}]
                               :ability  {:charges 3}
                               :life     1}]
                  :nemesis   {:unleash             [[:damage-gravehold 1]]
                              :on-player-exhausted [[:damage-gravehold 4]]}
                  :gravehold {:life 30}}
                 (damage-player 0 1)
                 (choose {:player-no 0
                          :breach-no 0}))
             {:players   [{:breaches [{:status :destroyed}]
                           :ability  {:charges 0}
                           :life     0}]
              :nemesis   {:unleash             [[:damage-gravehold 1]]
                          :on-player-exhausted [[:damage-gravehold 4]]}
              :gravehold {:life 26}})))))

(deftest edryss-tragg-test
  (testing "Edryss Tragg"
    (is (= (-> {:nemesis {:unleash   [[::hollow-crown/unleash]]
                          :play-area [edryss-tragg]}
                :players [{:life 8}]}
               (do-resolve-blood-magic :edryss-tragg)
               (choose {:player-no 0}))
           {:nemesis {:unleash   [[::hollow-crown/unleash]]
                      :play-area [edryss-tragg]}
            :players [{:life 6}]}))
    (is (= (-> {:nemesis {:unleash   [[::hollow-crown/unleash]]
                          :play-area [edryss-tragg]}
                :players [{:life 7}]}
               (do-resolve-blood-magic :edryss-tragg)
               (choose {:player-no 0}))
           {:nemesis {:unleash   [[::hollow-crown/unleash]]
                      :play-area [(assoc edryss-tragg :life 9)]}
            :players [{:life 5}]}))
    (is (= (-> {:nemesis {:unleash   [[::hollow-crown/unleash]]
                          :play-area [edryss-tragg]}
                :players [{:life 7}]}
               (do-resolve-blood-magic :edryss-tragg :avoid-self-damage true)
               (choose {:player-no 0}))
           {:nemesis {:unleash   [[::hollow-crown/unleash]]
                      :play-area [edryss-tragg]}
            :players [{:life 5}]}))))
