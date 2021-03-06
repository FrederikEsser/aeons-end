(ns aeons-end.core-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.starter :refer [crystal spark]]
            [aeons-end.cards.gem :refer [jade]]
            [aeons-end.cards.relic :refer [cairn-compass]]
            [aeons-end.mages :refer [buried-light]]
            [aeons-end.cards.power :as power]
            [aeons-end.turn-order :as turn-order]))

(defn fixture [f]
  (with-rand-seed 123 (f)))

(use-fixtures :each fixture)

(deftest gem-test
  (testing "Gems"
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :main}]}
               (play 0 :crystal))
           {:players [{:play-area [crystal]
                       :aether    1
                       :phase     :main}]}))
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :casting}]}
               (play 0 :crystal))
           {:players [{:play-area [crystal]
                       :aether    1
                       :phase     :main}]}))
    (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                          (-> {:players [{:hand  [crystal]
                                          :phase :draw}]}
                              (play 0 :crystal))))
    (is (thrown-with-msg? AssertionError #"Play error: You can't play Spell cards"
                          (-> {:players [{:hand [spark]}]}
                              (play 0 :spark)))))
  (testing "Play all gems"
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :main}]}
               (play-all-gems 0))
           {:players [{:play-area [crystal]
                       :aether    1
                       :phase     :main}]}))
    (is (= (-> {:players [{:hand  [jade crystal]
                           :phase :casting}]}
               (play-all-gems 0))
           {:players [{:play-area [jade crystal]
                       :aether    3
                       :phase     :main}]}))
    (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                          (-> {:players [{:hand  [crystal]
                                          :phase :draw}]}
                              (play-all-gems 0))))
    (is (= (-> {:players [{:hand  [(assoc jade :auto-play-index 1) crystal]
                           :phase :casting}]}
               (play-all-gems 0))
           {:players [{:play-area [crystal (assoc jade :auto-play-index 1)]
                       :aether    3
                       :phase     :main}]}))
    (is (= (-> {:players [{:hand  [(assoc jade :auto-play-index -1) crystal]
                           :phase :casting}]}
               (play-all-gems 0))
           {:players [{:play-area [(assoc jade :auto-play-index -1) crystal]
                       :aether    3
                       :phase     :main}]}))))

(deftest spell-test
  (testing "Spell"
    (testing "Prepping"
      (is (= (-> {:players [{:hand     [spark]
                             :breaches [{:status :opened}]
                             :phase    :main}]}
                 (prep-spell 0 0 :spark))
             {:players [{:breaches [{:status         :opened
                                     :prepped-spells [spark]}]
                         :phase    :main}]}))
      (is (= (-> {:players [{:hand     [spark]
                             :breaches [{:status :opened}]
                             :phase    :casting}]}
                 (prep-spell 0 0 :spark))
             {:players [{:breaches [{:status         :opened
                                     :prepped-spells [spark]}]
                         :phase    :main}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status :opened}]
                                            :phase    :draw}]}
                                (prep-spell 0 0 :spark))))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Crystal, which has type Gem"
                            (-> {:players [{:hand     [crystal]
                                            :breaches [{:status :opened}]}]}
                                (prep-spell 0 0 :crystal))))
      (is (= (-> {:players [{:hand     [spark]
                             :breaches [{:status :focused}]
                             :phase    :main}]}
                 (prep-spell 0 0 :spark))
             {:players [{:breaches [{:status         :focused
                                     :prepped-spells [spark]}]
                         :phase    :main}]}))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Spark to breach 0"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status :closed}]}]}
                                (prep-spell 0 0 :spark))))
      (is (thrown-with-msg? AssertionError #"Prep error: You can't prep Spark to breach 0"
                            (-> {:players [{:hand     [spark]
                                            :breaches [{:status         :opened
                                                        :prepped-spells [spark]}]}]}
                                (prep-spell 0 0 :spark)))))
    (testing "Casting"
      (is (= (-> {:players [{:breaches [{:prepped-spells [spark]}]
                             :phase    :casting}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :spark))
             {:players [{:breaches [{}]
                         :discard  [spark]
                         :phase    :casting}]
              :nemesis {:life 49}}))
      (is (= (-> {:players [{:breaches [{:status         :opened
                                         :bonus-damage   1
                                         :prepped-spells [spark]}]
                             :phase    :casting}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :spark))
             {:players [{:breaches [{:status       :opened
                                     :bonus-damage 1}]
                         :discard  [spark]
                         :phase    :casting}]
              :nemesis {:life 48}}))
      (is (= (-> {:players [{:breaches [{:status         :closed
                                         :bonus-damage   1
                                         :prepped-spells [spark]}]
                             :phase    :casting}]
                  :nemesis {:life 50}}
                 (cast-spell 0 0 :spark))
             {:players [{:breaches [{:status       :closed
                                     :bonus-damage 1}]
                         :discard  [spark]
                         :phase    :casting}]
              :nemesis {:life 49}}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Main phase to the Casting phase"
                            (-> {:players [{:breaches [{:prepped-spells [spark]}]
                                            :phase    :main}]
                                 :nemesis {:life 50}}
                                (cast-spell 0 0 :spark))))
      (is (= (-> {:players [{:hand     [crystal]
                             :breaches [{:status         :opened
                                         :prepped-spells [spark]}]
                             :phase    :casting}]}
                 (play 0 :crystal))
             {:players [{:play-area [crystal]
                         :breaches  [{:status         :opened
                                      :prepped-spells [spark]}]
                         :aether    1
                         :phase     :main}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go to the Main phase while you have prepped spells in closed breaches: Spark"
                            (-> {:players [{:hand     [crystal]
                                            :breaches [{:status         :closed
                                                        :prepped-spells [spark]}]
                                            :phase    :casting}]}
                                (play 0 :crystal)))))))

(deftest dual-breach-spell-test
  (testing "Dual breach spells"
    (let [dual-breach-spell {:id          1
                             :name        :dual-breach-spell
                             :type        :spell
                             :dual-breach true
                             :effects     [[:deal-damage 1]]}]
      (testing "Prepping"
        (is (= (-> {:players [{:hand     [dual-breach-spell]
                               :breaches [{:status :opened}
                                          {:status :opened}]}]}
                   (prep-spell 0 0 :dual-breach-spell))
               {:players [{:breaches [{:status         :opened
                                       :prepped-spells [dual-breach-spell]}
                                      {:status :opened}]}]}))
        (is (= (-> {:players [{:hand     [dual-breach-spell]
                               :breaches [{:status :opened}
                                          {:status :focused}]}]}
                   (prep-spell 0 0 :dual-breach-spell))
               {:players [{:breaches [{:status         :opened
                                       :prepped-spells [dual-breach-spell]}
                                      {:status :focused}]}]}))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [dual-breach-spell]
                                              :breaches [{:status :opened}
                                                         {:status :closed}]}]}
                                  (prep-spell 0 0 :dual-breach-spell))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [dual-breach-spell]
                                              :breaches [{:status :opened}
                                                         {:status :opened}]}]}
                                  (prep-spell 0 1 :dual-breach-spell))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [dual-breach-spell]
                                              :breaches [{:status :opened}
                                                         {:status :closed}
                                                         {:status :opened}]}]}
                                  (prep-spell 0 0 :dual-breach-spell))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [dual-breach-spell]
                                              :breaches [{:status :opened}
                                                         {:status :destroyed}
                                                         {:status :opened}]}]}
                                  (prep-spell 0 0 :dual-breach-spell))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [dual-breach-spell]
                                              :breaches [{:status         :opened
                                                          :prepped-spells [spark]}
                                                         {:status :opened}]}]}
                                  (prep-spell 0 0 :dual-breach-spell))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [dual-breach-spell]
                                              :breaches [{:status :opened}
                                                         {:status         :opened
                                                          :prepped-spells [spark]}]}]}
                                  (prep-spell 0 0 :dual-breach-spell))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [spark]
                                              :breaches [{:status         :opened
                                                          :prepped-spells [dual-breach-spell]}
                                                         {:status :opened}]}]}
                                  (prep-spell 0 0 :spark))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [spark]
                                              :breaches [{:status         :opened
                                                          :prepped-spells [dual-breach-spell]}
                                                         {:status :opened}]}]}
                                  (prep-spell 0 1 :spark))))
        (is (= (-> {:players [{:hand            [dual-breach-spell]
                               :breaches        [{:status         :opened
                                                  :prepped-spells [spark]}
                                                 {:status         :opened
                                                  :prepped-spells [spark]}]
                               :breach-capacity 2}]}
                   (prep-spell 0 0 :dual-breach-spell))
               {:players [{:breaches        [{:status         :opened
                                              :prepped-spells [spark dual-breach-spell]}
                                             {:status         :opened
                                              :prepped-spells [spark]}]
                           :breach-capacity 2}]}))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand            [dual-breach-spell]
                                              :breaches        [{:status         :opened
                                                                 :prepped-spells [spark]}
                                                                {:status         :focused
                                                                 :prepped-spells [spark]}]
                                              :breach-capacity 2}]}
                                  (prep-spell 0 0 :dual-breach-spell))))
        (is (= (-> {:players [{:hand            [dual-breach-spell]
                               :breaches        [{:status         :opened
                                                  :prepped-spells [spark]}
                                                 {:status :opened}]
                               :breach-capacity 2}]}
                   (prep-spell 0 0 :dual-breach-spell))
               {:players [{:breaches        [{:status         :opened
                                              :prepped-spells [spark dual-breach-spell]}
                                             {:status :opened}]
                           :breach-capacity 2}]}))
        (let [dual-breach-spell-2 (assoc dual-breach-spell :id 2)]
          (is (= (-> {:players [{:hand            [dual-breach-spell-2]
                                 :breaches        [{:status         :opened
                                                    :prepped-spells [spark dual-breach-spell]}
                                                   {:status :opened}
                                                   {:status         :opened
                                                    :prepped-spells [spark]}]
                                 :breach-capacity 2}]}
                     (prep-spell 0 1 :dual-breach-spell))
                 {:players [{:breaches        [{:status         :opened
                                                :prepped-spells [spark dual-breach-spell]}
                                               {:status         :opened
                                                :prepped-spells [dual-breach-spell-2]}
                                               {:status         :opened
                                                :prepped-spells [spark]}]
                             :breach-capacity 2}]}))
          (is (thrown-with-msg? AssertionError #"Prep error:"
                                (-> {:players [{:hand            [spark]
                                                :breaches        [{:status         :opened
                                                                   :prepped-spells [spark dual-breach-spell]}
                                                                  {:status         :opened
                                                                   :prepped-spells [dual-breach-spell-2]}
                                                                  {:status         :opened
                                                                   :prepped-spells [spark]}]
                                                :breach-capacity 2}]}
                                    (prep-spell 0 1 :spark))))
          (is (thrown-with-msg? AssertionError #"Prep error:"
                                (-> {:players [{:hand            [spark]
                                                :breaches        [{:status         :opened
                                                                   :prepped-spells [spark dual-breach-spell]}
                                                                  {:status         :opened
                                                                   :prepped-spells [dual-breach-spell-2]}
                                                                  {:status         :opened
                                                                   :prepped-spells [spark]}]
                                                :breach-capacity 2}]}
                                    (prep-spell 0 2 :spark)))))
        (is (thrown-with-msg? AssertionError #"Phase error:"
                              (-> {:players [{:hand     [crystal]
                                              :breaches [{:status         :opened
                                                          :prepped-spells [dual-breach-spell]}
                                                         {:status :closed}]
                                              :phase    :casting}]}
                                  (play 0 :crystal)))))
      (testing "Casting"
        (is (= (-> {:players [{:breaches [{:prepped-spells [dual-breach-spell]}
                                          {}]}]
                    :nemesis {:life 50}}
                   (cast-spell 0 0 :dual-breach-spell))
               {:players [{:breaches [{}
                                      {}]
                           :discard  [dual-breach-spell]}]
                :nemesis {:life 49}}))
        (is (= (-> {:players [{:breaches [{:status         :opened
                                           :bonus-damage   1
                                           :prepped-spells [dual-breach-spell]}
                                          {:status       :closed
                                           :bonus-damage 1}]}]
                    :nemesis {:life 50}}
                   (cast-spell 0 0 :dual-breach-spell))
               {:players [{:breaches [{:status       :opened
                                       :bonus-damage 1}
                                      {:status       :closed
                                       :bonus-damage 1}]
                           :discard  [dual-breach-spell]}]
                :nemesis {:life 48}}))
        (is (= (-> {:players [{:breaches [{:status         :opened
                                           :bonus-damage   1
                                           :prepped-spells [dual-breach-spell]}
                                          {:status       :opened
                                           :bonus-damage 1}]}]
                    :nemesis {:life 50}}
                   (cast-spell 0 0 :dual-breach-spell))
               {:players [{:breaches [{:status       :opened
                                       :bonus-damage 1}
                                      {:status       :opened
                                       :bonus-damage 1}]
                           :discard  [dual-breach-spell]}]
                :nemesis {:life 47}}))
        (is (= (-> {:players [{:breaches [{:status         :opened
                                           :opened-effects [[:gain-aether 1]]
                                           :prepped-spells [dual-breach-spell]}
                                          {}]}]
                    :nemesis {:life 50}}
                   (cast-spell 0 0 :dual-breach-spell))
               {:players [{:breaches [{:status         :opened
                                       :opened-effects [[:gain-aether 1]]}
                                      {}]
                           :discard  [dual-breach-spell]
                           :aether   1}]
                :nemesis {:life 49}}))
        (is (= (-> {:players [{:breaches [{:prepped-spells [dual-breach-spell]}
                                          {:status         :opened
                                           :opened-effects [[:gain-aether 1]]}]}]
                    :nemesis {:life 50}}
                   (cast-spell 0 0 :dual-breach-spell))
               {:players [{:breaches [{}
                                      {:status         :opened
                                       :opened-effects [[:gain-aether 1]]}]
                           :discard  [dual-breach-spell]
                           :aether   1}]
                :nemesis {:life 49}})))
      (testing "Destroy breach"
        (is (= (-> {:players [{:breaches [{:status         :opened
                                           :prepped-spells [dual-breach-spell]}
                                          {:status :closed}]}]}
                   (destroy-breach 0 0))
               {:players [{:breaches [{:status :destroyed}
                                      {:status :closed}]
                           :discard  [dual-breach-spell]}]}))
        (is (= (-> {:players [{:breaches [{:status         :opened
                                           :prepped-spells [dual-breach-spell]}
                                          {:status :closed}]}]}
                   (destroy-breach 0 1))
               {:players [{:breaches [{:status :opened}
                                      {:status :destroyed}]
                           :discard  [dual-breach-spell]}]}))
        (is (= (-> {:players [{:breaches [{:status         :opened
                                           :prepped-spells [dual-breach-spell]}
                                          {:status :closed}
                                          {:status :closed}]}]}
                   (destroy-breach 0 2))
               {:players [{:breaches [{:status         :opened
                                       :prepped-spells [dual-breach-spell]}
                                      {:status :closed}
                                      {:status :destroyed}]}]}))))))

(deftest link-spell-test
  (testing "Link spells"
    (let [link-spell {:id      1
                      :name    :link-spell
                      :type    :spell
                      :link    true
                      :effects [[:deal-damage 1]]}]
      (testing "Prepping"
        (is (= (-> {:players [{:hand     [link-spell]
                               :breaches [{:status :opened}]}]}
                   (prep-spell 0 0 :link-spell))
               {:players [{:breaches [{:status         :opened
                                       :prepped-spells [link-spell]}]}]}))
        (is (= (-> {:players [{:hand     [link-spell]
                               :breaches [{:status         :opened
                                           :prepped-spells [link-spell]}]}]}
                   (prep-spell 0 0 :link-spell))
               {:players [{:breaches [{:status         :opened
                                       :prepped-spells [link-spell link-spell]}]}]}))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [link-spell]
                                              :breaches [{:status         :opened
                                                          :prepped-spells [spark]}]}]}
                                  (prep-spell 0 0 :link-spell))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [spark]
                                              :breaches [{:status         :opened
                                                          :prepped-spells [link-spell]}]}]}
                                  (prep-spell 0 0 :spark))))
        (is (thrown-with-msg? AssertionError #"Prep error:"
                              (-> {:players [{:hand     [link-spell]
                                              :breaches [{:status         :opened
                                                          :prepped-spells [link-spell link-spell]}]}]}
                                  (prep-spell 0 0 :link-spell))))))))

(deftest buy-card-test
  (testing "Buy card"
    (let [jade (assoc jade :id 1)]
      (is (= (-> {:supply  [{:card jade :pile-size 7}]
                  :players [{:aether 2
                             :phase  :main}]}
                 (buy-card 0 :jade))
             {:supply  [{:card jade :pile-size 6}]
              :players [{:discard [jade]
                         :aether  0
                         :phase   :main}]}))
      (is (= (-> {:supply  [{:card jade :pile-size 7}]
                  :players [{:aether 2
                             :phase  :casting}]}
                 (buy-card 0 :jade))
             {:supply  [{:card jade :pile-size 6}]
              :players [{:discard [jade]
                         :aether  0
                         :phase   :main}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                            (-> {:supply  [{:card jade :pile-size 7}]
                                 :players [{:aether 2
                                            :phase  :draw}]}
                                (buy-card 0 :jade))))
      (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 2 aether, when you only have 1"
                            (-> {:supply  [{:card jade :pile-size 7}]
                                 :players [{:aether 1
                                            :phase  :main}]}
                                (buy-card 0 :jade))))
      (is (thrown-with-msg? AssertionError #"Buy error: Jade supply is empty"
                            (-> {:supply  [{:card jade :pile-size 0}]
                                 :players [{:aether 2
                                            :phase  :main}]}
                                (buy-card 0 :jade))))
      (testing "Earmarked aether"
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether           0
                               :earmarked-aether {#{:gem} 2}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard [jade]
                           :aether  0}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether           1
                               :earmarked-aether {#{:gem} 1}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard [jade]
                           :aether  0}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether           2
                               :earmarked-aether {#{:gem} 3}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard          [jade]
                           :aether           2
                           :earmarked-aether {#{:gem} 1}}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether           2
                               :earmarked-aether {#{:spell} 3}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard          [jade]
                           :aether           0
                           :earmarked-aether {#{:spell} 3}}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether           1
                               :earmarked-aether [[#{:gem :relic :spell} 3]
                                                  [#{:gem} 1]]}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard          [jade]
                           :aether           1
                           :earmarked-aether {#{:gem :relic :spell} 2}}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether           1
                               :earmarked-aether {#{:spell} 1
                                                  #{:gem}   1}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard          [jade]
                           :aether           0
                           :earmarked-aether {#{:spell} 1}}]})))
      (testing "Restricted aether"
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether            0
                               :restricted-aether {#{:spell} 2}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard [jade]
                           :aether  0}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether            1
                               :restricted-aether {#{:relic} 1}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard [jade]
                           :aether  0}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether            2
                               :restricted-aether {#{:spell} 3}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard           [jade]
                           :aether            2
                           :restricted-aether {#{:spell} 1}}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether            2
                               :restricted-aether {#{:gem} 3}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard           [jade]
                           :aether            0
                           :restricted-aether {#{:gem} 3}}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether            1
                               :restricted-aether [[#{:relic :spell} 1]
                                                   [#{:spell} 3]]}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard           [jade]
                           :aether            1
                           :restricted-aether {#{:spell} 2}}]}))
        (is (= (-> {:supply  [{:card jade :pile-size 7}]
                    :players [{:aether            1
                               :restricted-aether {#{:spell} 1
                                                   #{:gem}   1}}]}
                   (buy-card 0 :jade))
               {:supply  [{:card jade :pile-size 6}]
                :players [{:discard           [jade]
                           :aether            0
                           :restricted-aether {#{:gem} 1}}]}))))))

(deftest ability-test
  (testing "Ability"
    (testing "Charge"
      (is (= (-> {:current-player 0
                  :players        [{:ability {:charges     0
                                              :charge-cost 4}
                                    :aether  2
                                    :phase   :main}]}
                 (charge-ability 0))
             {:current-player 0
              :players        [{:ability {:charges     1
                                          :charge-cost 4}
                                :aether  0
                                :phase   :main}]}))
      (is (= (-> {:current-player 0
                  :players        [{:ability {:charges     0
                                              :charge-cost 4}
                                    :aether  2
                                    :phase   :casting}]}
                 (charge-ability 0))
             {:current-player 0
              :players        [{:ability {:charges     1
                                          :charge-cost 4}
                                :aether  0
                                :phase   :main}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                            (-> {:current-player 0
                                 :players        [{:ability {:charges     0
                                                             :charge-cost 4}
                                                   :aether  2
                                                   :phase   :draw}]}
                                (charge-ability 0))))
      (is (= (-> {:players [{:ability {:charges     3
                                       :charge-cost 4}
                             :aether  2}]}
                 (charge-ability 0))
             {:players [{:ability {:charges     4
                                   :charge-cost 4}
                         :aether  0}]}))
      (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 2 aether, when you only have 1"
                            (-> {:players [{:ability {:charges     0
                                                      :charge-cost 4}
                                            :aether  1}]}
                                (charge-ability 0))))
      (is (thrown-with-msg? AssertionError #"Charge error: You already have 4 charges"
                            (-> {:players [{:ability {:charges     4
                                                      :charge-cost 4}
                                            :aether  2}]}
                                (charge-ability 0)))))
    (testing "Activate"
      (is (= (-> {:current-player 0
                  :players        [{:ability {:activation  :your-main-phase
                                              :charges     4
                                              :charge-cost 4
                                              :effects     [[:gain-aether 2]]}
                                    :phase   :casting}]}
                 (activate-ability 0))
             {:current-player 0
              :players        [{:ability {:activation  :your-main-phase
                                          :charges     0
                                          :charge-cost 4
                                          :effects     [[:gain-aether 2]]}
                                :phase   :main
                                :aether  2}]}))
      (is (= (-> {:current-player 0
                  :players        [{:ability {:activation  :your-main-phase
                                              :charges     4
                                              :charge-cost 4
                                              :effects     [[:gain-aether 2]]}
                                    :phase   :main}]}
                 (activate-ability 0))
             {:current-player 0
              :players        [{:ability {:activation  :your-main-phase
                                          :charges     0
                                          :charge-cost 4
                                          :effects     [[:gain-aether 2]]}
                                :phase   :main
                                :aether  2}]}))
      (is (thrown-with-msg? AssertionError #"Phase error: You can't go from the Draw phase to the Main phase"
                            (-> {:current-player 0
                                 :players        [{:ability {:activation  :your-main-phase
                                                             :charges     4
                                                             :charge-cost 4
                                                             :effects     [[:gain-aether 2]]}
                                                   :phase   :draw}]}
                                (activate-ability 0))))
      (is (thrown-with-msg? AssertionError #"Activate error: It's not your turn"
                            (-> {:current-player :nemesis
                                 :players        [{:ability {:name        :uberpower
                                                             :activation  :your-main-phase
                                                             :charges     4
                                                             :charge-cost 4
                                                             :effects     [[:gain-aether 2]]}
                                                   :phase   :out-of-turn}]}
                                (activate-ability 0))))
      (is (thrown-with-msg? AssertionError #"Activate error: Uberpower is not fully charged"
                            (-> {:current-player 0
                                 :players        [{:ability {:name        :uberpower
                                                             :activation  :your-main-phase
                                                             :charges     3
                                                             :charge-cost 4
                                                             :effects     [[:gain-aether 2]]}
                                                   :phase   :main}]}
                                (activate-ability 0))))
      (is (thrown-with-msg? AssertionError #"Activate error: Uberpower can't be activated in the Main phase"
                            (-> {:current-player 0
                                 :players        [{:ability {:name        :uberpower
                                                             :activation  :nemesis-draw
                                                             :charges     4
                                                             :charge-cost 4
                                                             :effects     [[:gain-aether 2]]}
                                                   :phase   :main}]}
                                (activate-ability 0))))
      (is (= (-> {:current-player :nemesis
                  :nemesis        {:deck    [{:name    :fugazi
                                              :type    :attack
                                              :effects [[:unleash]]}]
                                   :unleash [[:damage-gravehold 1]]
                                   :phase   :main}
                  :gravehold      {:life 30}
                  :players        [{:ability {:name        :uberpower
                                              :activation  :nemesis-draw
                                              :charges     4
                                              :charge-cost 4
                                              :effects     [[:discard-nemesis-card {:card-name :fugazi}]]}
                                    :phase   :out-of-turn}]}
                 (draw-nemesis-card :auto-resolve? false)
                 (choose {:area :ability :player-no 0}))
             {:current-player :nemesis
              :nemesis        {:discard [{:name    :fugazi
                                          :type    :attack
                                          :effects [[:unleash]]}]
                               :unleash [[:damage-gravehold 1]]
                               :phase   :draw}
              :gravehold      {:life 30}
              :players        [{:ability {:name        :uberpower
                                          :activation  :nemesis-draw
                                          :charges     0
                                          :charge-cost 4
                                          :effects     [[:discard-nemesis-card {:card-name :fugazi}]]}
                                :phase   :out-of-turn}]})))))

(deftest breach-test
  (testing "Breaches"
    (testing "Focus"
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      0}]
                             :aether   2}]}
                 (focus-breach 0 0))
             {:players [{:breaches [{:status     :focused
                                     :focus-cost 2
                                     :open-costs [5 4 3 2]
                                     :stage      1}]
                         :aether   0}]}))
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      0}]
                             :aether   4}]}
                 (focus-breach 0 0)
                 (focus-breach 0 0))
             {:players [{:breaches [{:status     :focused
                                     :focus-cost 2
                                     :open-costs [5 4 3 2]
                                     :stage      2}]
                         :aether   0}]}))
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      2}]
                             :aether   2}]}
                 (focus-breach 0 0))
             {:players [{:breaches [{:status     :focused
                                     :focus-cost 2
                                     :open-costs [5 4 3 2]
                                     :stage      3}]
                         :aether   0}]}))
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      3}]
                             :aether   2}]}
                 (focus-breach 0 0))
             {:players [{:breaches [{:status :opened}]
                         :aether   0}]}))
      (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 2 aether, when you only have 1"
                            (-> {:players [{:breaches [{:status     :closed
                                                        :focus-cost 2
                                                        :open-costs [5 4 3 2]
                                                        :stage      0}]
                                            :aether   1}]}
                                (focus-breach 0 0))))
      (is (= (-> {:players [{:breaches [{:status :opened}
                                        {:status     :closed
                                         :focus-cost 3
                                         :open-costs [9 7 5 3]
                                         :stage      0}]
                             :aether   3}]}
                 (focus-breach 0 1))
             {:players [{:breaches [{:status :opened}
                                    {:status     :focused
                                     :focus-cost 3
                                     :open-costs [9 7 5 3]
                                     :stage      1}]
                         :aether   0}]}))
      (testing "cost reduction"
        (is (= (-> {:players [{:breaches              [{:status     :closed
                                                        :focus-cost 2
                                                        :stage      0}]
                               :breach-cost-reduction 3
                               :aether                2}]}
                   (focus-breach 0 0))
               {:players [{:breaches [{:status     :focused
                                       :focus-cost 2
                                       :stage      1}]
                           :aether   2}]}))
        (is (= (-> {:players [{:breaches              [{:status     :closed
                                                        :focus-cost 4
                                                        :stage      0}]
                               :breach-cost-reduction 3
                               :aether                2}]}
                   (focus-breach 0 0))
               {:players [{:breaches [{:status     :focused
                                       :focus-cost 4
                                       :stage      1}]
                           :aether   1}]}))))
    (testing "Open"
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      0}]
                             :aether   5}]}
                 (open-breach 0 0))
             {:players [{:breaches [{:status :opened}]
                         :aether   0}]}))
      (is (= (-> {:players [{:breaches [{:status     :closed
                                         :focus-cost 2
                                         :open-costs [5 4 3 2]
                                         :stage      1}]
                             :aether   5}]}
                 (open-breach 0 0))
             {:players [{:breaches [{:status :opened}]
                         :aether   1}]}))
      (is (thrown-with-msg? AssertionError #"Pay error: You can't pay 3 aether, when you only have 2"
                            (-> {:players [{:breaches [{:status     :closed
                                                        :focus-cost 2
                                                        :open-costs [5 4 3 2]
                                                        :stage      2}]
                                            :aether   2}]}
                                (open-breach 0 0))))
      (is (= (-> {:players [{:breaches [{:status :opened}
                                        {:status     :closed
                                         :focus-cost 3
                                         :open-costs [9 7 5 3]
                                         :stage      2}]
                             :aether   5}]}
                 (open-breach 0 1))
             {:players [{:breaches [{:status :opened}
                                    {:status :opened}]
                         :aether   0}]}))
      (testing "cost reduction"
        (is (= (-> {:players [{:breaches              [{:status     :closed
                                                        :focus-cost 2
                                                        :open-costs [5 4 3 2]
                                                        :stage      0}]
                               :breach-cost-reduction 3
                               :aether                5}]}
                   (open-breach 0 0))
               {:players [{:breaches [{:status :opened}]
                           :aether   3}]}))
        (is (= (-> {:players [{:breaches              [{:status     :closed
                                                        :focus-cost 4
                                                        :open-costs [13 10 7 4]
                                                        :stage      2}]
                               :breach-cost-reduction 3
                               :aether                5}]}
                   (open-breach 0 0))
               {:players [{:breaches [{:status :opened}]
                           :aether   1}]}))))))

(deftest discard-test
  (testing "Discard"
    (is (= (-> {:players [{:play-area [crystal jade]
                           :phase     :main}]}
               (discard 0 :jade))
           {:players [{:play-area [crystal]
                       :discard   [jade]
                       :phase     :draw}]}))
    (is (= (-> {:players [{:play-area [crystal]
                           :discard   [jade]
                           :phase     :draw}]}
               (discard 0 :crystal))
           {:players [{:discard [jade crystal]
                       :phase   :draw}]})))
  (testing "Discard all"
    (is (= (-> {:players [{:play-area [crystal jade]
                           :phase     :main}]}
               (discard-all 0))
           {:players [{:discard [jade crystal]
                       :phase   :draw}]}))
    (is (= (-> {:players [{:play-area [crystal buried-light jade]
                           :phase     :main}]}
               (discard-all 0))
           {:players [{:discard [jade buried-light crystal]
                       :phase   :draw}]}))
    (is (= (-> {:players [{:play-area [jade]
                           :discard   [crystal]
                           :phase     :draw}]}
               (discard-all 0))
           {:players [{:discard [crystal jade]
                       :phase   :draw}]}))
    (is (= (-> {:players [{:hand  [crystal]
                           :phase :casting}]}
               (discard-all 0))
           {:players [{:hand  [crystal]
                       :phase :draw}]}))))

(deftest end-turn-test
  (testing "End turn"
    (is (= (-> {:players [{:deck     [crystal crystal crystal spark spark]
                           :discard  [crystal crystal crystal crystal]
                           :breaches [{:status         :opened
                                       :prepped-spells [spark]}]
                           :ability  {:charges 2}
                           :life     8
                           :aether   1
                           :phase    :draw}]}
               (end-turn 0))
           {:players [{:hand     [crystal crystal crystal spark spark]
                       :discard  [crystal crystal crystal crystal]
                       :breaches [{:status         :opened
                                   :prepped-spells [spark]}]
                       :ability  {:charges 2}
                       :life     8
                       :phase    :out-of-turn}]}))
    (is (= (-> {:players [{:hand    [spark]
                           :deck    [crystal crystal crystal spark spark]
                           :discard [crystal crystal crystal crystal]
                           :phase   :draw}]}
               (end-turn 0))
           {:players [{:hand    [spark crystal crystal crystal spark]
                       :deck    [spark]
                       :discard [crystal crystal crystal crystal]
                       :phase   :out-of-turn}]}))
    (is (= (-> {:players [{:deck    [spark]
                           :discard [crystal crystal crystal spark crystal]
                           :phase   :draw}]}
               (end-turn 0))
           {:players [{:hand  [spark crystal crystal crystal spark]
                       :deck  [crystal]
                       :phase :out-of-turn}]}))
    (is (= (-> {:players [{:hand  [crystal crystal crystal spark crystal spark]
                           :deck  [spark]
                           :phase :draw}]}
               (end-turn 0))
           {:players [{:hand  [crystal crystal crystal spark crystal spark]
                       :deck  [spark]
                       :phase :out-of-turn}]}))
    (is (= (-> {:players [{:breaches [{:status         :focused
                                       :prepped-spells [spark]
                                       :stage          2}]
                           :phase    :draw}]}
               (end-turn 0))
           {:players [{:breaches [{:status         :closed
                                   :prepped-spells [spark]
                                   :stage          2}]
                       :phase    :out-of-turn}]}))
    (is (= (-> {:players [{:earmarked-aether {#{:spell} 2}
                           :phase            :draw}]}
               (end-turn 0))
           {:players [{:phase :out-of-turn}]}))
    (is (= (-> {:current-player 0
                :turn-order     {:deck    [turn-order/player-2
                                           turn-order/player-3]
                                 :discard [turn-order/player-1]}
                :players        [{} {} {}]}
               (end-turn 0))
           {:current-player 1
            :turn-order     {:deck    [turn-order/player-3]
                             :discard [turn-order/player-1
                                       turn-order/player-2]}
            :players        [{} {} {}]}))
    (is (= (-> {:current-player 1
                :turn-order     {:deck    []
                                 :discard [turn-order/player-3
                                           turn-order/player-1
                                           turn-order/player-4
                                           turn-order/player-2]}
                :players        [{} {} {} {}]}
               (end-turn 1))
           {:current-player 2
            :turn-order     {:deck    [turn-order/player-1
                                       turn-order/player-2
                                       turn-order/player-4]
                             :discard [turn-order/player-3]}
            :players        [{} {} {} {}]}))
    (is (= (-> {:players [{:this-turn [{:gain :jade}]
                           :phase     :draw}]}
               (end-turn 0))
           {:players [{:phase :out-of-turn}]}))
    (is (= (-> {:players [{:breach-cost-reduction 3
                           :phase                 :draw}]}
               (end-turn 0))
           {:players [{:phase :out-of-turn}]}))))

(deftest draw-nemesis-card-test
  (testing "Draw nemesis card"
    (is (= (-> {:nemesis   {:deck    [{:name    :fugazi
                                       :type    :attack
                                       :effects [[:unleash]]}]
                            :unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:discard [{:name    :fugazi
                                   :type    :attack
                                   :effects [[:unleash]]}]
                        :unleash [[:damage-gravehold 1]]}
            :gravehold {:life 29}}))
    (is (= (-> {:nemesis {:deck [{:name :iznogood
                                  :type :power}]}}
               draw-nemesis-card)
           {:nemesis {:play-area [{:name :iznogood
                                   :type :power}]}}))
    (is (= (-> {:nemesis {:deck [{:name :bad-motherfucker
                                  :type :minion
                                  :life 2}]}}
               draw-nemesis-card)
           {:nemesis {:play-area [{:name     :bad-motherfucker
                                   :type     :minion
                                   :life     2
                                   :max-life 2}]}}))))

(deftest resolve-nemesis-card-test
  (testing "Resolve nemesis card"
    (testing "Power"
      (is (= (-> {:nemesis {:play-area [{:name  :iznogood
                                         :type  :power
                                         :power {:power   3
                                                 :effects [[:damage-gravehold 1]]}}]}}
                 resolve-nemesis-cards-in-play)
             {:nemesis {:play-area [{:name  :iznogood
                                     :type  :power
                                     :power {:power   2
                                             :effects [[:damage-gravehold 1]]}}]}}))
      (is (= (-> {:nemesis {:play-area [{:name  :iznogood
                                         :type  :power
                                         :power {:power   2
                                                 :effects [[:damage-gravehold 1]]}}]}}
                 resolve-nemesis-cards-in-play)
             {:nemesis {:play-area [{:name  :iznogood
                                     :type  :power
                                     :power {:power   1
                                             :effects [[:damage-gravehold 1]]}}]}}))
      (is (= (-> {:nemesis   {:play-area [{:name  :iznogood
                                           :type  :power
                                           :power {:power       1
                                                   :start-power 2
                                                   :effects     [[:damage-gravehold 1]]}}]}
                  :gravehold {:life 30}}
                 resolve-nemesis-cards-in-play)
             {:nemesis   {:discard [{:name  :iznogood
                                     :type  :power
                                     :power {:power       2
                                             :start-power 2
                                             :effects     [[:damage-gravehold 1]]}}]}
              :gravehold {:life 29}})))
    (testing "Minion"
      (is (= (-> {:nemesis   {:play-area [{:name       :bad-motherfucker
                                           :type       :minion
                                           :persistent {:effects [[:damage-gravehold 1]]}}]}
                  :gravehold {:life 30}}
                 resolve-nemesis-cards-in-play)
             {:nemesis   {:play-area [{:name       :bad-motherfucker
                                       :type       :minion
                                       :persistent {:effects [[:damage-gravehold 1]]}}]}
              :gravehold {:life 29}})))))

(deftest damage-minion-test
  (testing "Damage Minions"
    (is (= (-> {:nemesis {:life 50}}
               (deal-damage 1))
           {:nemesis {:life 49}}))
    (is (= (-> {:nemesis {:life      50
                          :play-area [{:name :bad-motherfucker
                                       :type :minion
                                       :life 5}]}}
               (deal-damage 1)
               (choose {:area      :nemesis
                        :player-no 0
                        :card-name :nemesis}))
           {:nemesis {:life      49
                      :play-area [{:name :bad-motherfucker
                                   :type :minion
                                   :life 5}]}}))
    (is (= (-> {:nemesis {:life      50
                          :play-area [{:name :bad-motherfucker
                                       :type :minion
                                       :life 5}]}}
               (deal-damage 1)
               (choose {:area      :minions
                        :player-no 0
                        :card-name :bad-motherfucker}))
           {:nemesis {:life      50
                      :play-area [{:name :bad-motherfucker
                                   :type :minion
                                   :life 4}]}}))
    (is (= (-> {:nemesis {:life      50
                          :play-area [{:name     :bad-motherfucker
                                       :type     :minion
                                       :life     1
                                       :max-life 2}]}}
               (deal-damage 1)
               (choose {:area      :minions
                        :player-no 0
                        :card-name :bad-motherfucker}))
           {:nemesis {:life    50
                      :discard [{:name     :bad-motherfucker
                                 :type     :minion
                                 :life     2
                                 :max-life 2}]}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [spark]}]
                           :phase    :casting}]
                :nemesis {:life      50
                          :play-area [{:name :bad-motherfucker
                                       :type :minion
                                       :life 2}]}}
               (cast-spell 0 0 :spark)
               (choose {:area      :nemesis
                        :player-no 0
                        :card-name :nemesis}))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :discard  [spark]
                       :phase    :casting}]
            :nemesis {:life      48
                      :play-area [{:name :bad-motherfucker
                                   :type :minion
                                   :life 2}]}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [spark]}]
                           :phase    :casting}]
                :nemesis {:life      50
                          :play-area [{:name     :bad-motherfucker
                                       :type     :minion
                                       :life     2
                                       :max-life 2}]}}
               (cast-spell 0 0 :spark)
               (choose {:area      :minions
                        :player-no 0
                        :card-name :bad-motherfucker}))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :discard  [spark]
                       :phase    :casting}]
            :nemesis {:life    50
                      :discard [{:name     :bad-motherfucker
                                 :type     :minion
                                 :life     2
                                 :max-life 2}]}}))))

(deftest exhaust-player-test
  (testing "Exhausted"
    (is (= (-> {:players   [{:breaches [{:status     :closed
                                         :focus-cost 2}]
                             :ability  {:charges 3}
                             :life     1}]
                :nemesis   {:unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}}
               (damage-player 0 1)
               (choose {:player-no 0
                        :breach-no 0}))
           {:players   [{:breaches [{:status :destroyed}]
                         :ability  {:charges 0}
                         :life     0}]
            :nemesis   {:unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}}))
    (is (= (-> {:players   [{:breaches [{:status         :opened
                                         :prepped-spells [spark]}]
                             :ability  {:charges 3}
                             :life     1}]
                :nemesis   {:unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}}
               (damage-player 0 1)
               (choose {:player-no 0
                        :breach-no 0}))
           {:players   [{:breaches [{:status :destroyed}]
                         :discard  [spark]
                         :ability  {:charges 0}
                         :life     0}]
            :nemesis   {:unleash [[:damage-gravehold 1]]}
            :gravehold {:life 28}}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:players   [{:breaches [{:status :destroyed}
                                                       {:status :closed}]
                                            :life     1}]
                               :nemesis   {:unleash [[:damage-gravehold 1]]}
                               :gravehold {:life 30}}
                              (damage-player 0 1)
                              (choose {:breach-no 0}))))
    (is (= (-> {:players   [{:breaches [{:status :opened}]
                             :ability  {:charges 3}
                             :life     1}]
                :nemesis   {:unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}}
               (damage-player 0 2)
               (choose {:player-no 0
                        :breach-no 0}))
           {:players   [{:breaches [{:status :destroyed}]
                         :ability  {:charges 0}
                         :life     0}]
            :nemesis   {:unleash [[:damage-gravehold 1]]}
            :gravehold {:life 26}}))
    (is (= (-> {:players   [{:breaches [{:status :opened}]
                             :ability  {:charges 3}
                             :life     0}]
                :nemesis   {:unleash [[:damage-gravehold 1]]}
                :gravehold {:life 30}}
               (damage-player 0 2))
           {:players   [{:breaches [{:status :opened}]
                         :ability  {:charges 3}
                         :life     0}]
            :nemesis   {:unleash [[:damage-gravehold 1]]}
            :gravehold {:life 26}}))
    (is (= (-> {:players [{:life 1}]}
               (heal-player 0 1))
           {:players [{:life 2}]}))
    (is (= (-> {:players [{:life 0}]}
               (heal-player 0 1))
           {:players [{:life 0}]}))))

(deftest game-over-test
  (testing "Game Over"
    (testing "Nemesis defeated"
      (is (= (-> {:real-game? true
                  :nemesis    {:life 2
                               :deck [{}]}}
                 (deal-damage 1))
             {:real-game? true
              :nemesis    {:life 1
                           :deck [{}]}}))
      (is (= (-> {:real-game? true
                  :nemesis    {:life 1
                               :deck [{}]}}
                 (deal-damage 1)
                 (update :game-over dissoc :text))
             {:real-game? true
              :nemesis    {:life 0
                           :deck [{}]}
              :game-over  {:conclusion :victory}})))
    (testing "Nemesis exhausted"
      (is (= (-> {:real-game? true
                  :nemesis    {:deck [{:name    :last-attack
                                       :type    :attack
                                       :effects [[:damage-gravehold 1]]}
                                      {}]}
                  :gravehold  {:life 30}}
                 draw-nemesis-card)
             {:real-game? true
              :nemesis    {:deck    [{}]
                           :discard [{:name    :last-attack
                                      :type    :attack
                                      :effects [[:damage-gravehold 1]]}]}
              :gravehold  {:life 29}}))
      (is (= (-> {:real-game? true
                  :nemesis    {:deck      [{:name    :last-attack
                                            :type    :attack
                                            :effects [[:damage-gravehold 1]]}]
                               :play-area [{}]}
                  :gravehold  {:life 30}}
                 draw-nemesis-card)
             {:real-game? true
              :nemesis    {:play-area [{}]
                           :discard   [{:name    :last-attack
                                        :type    :attack
                                        :effects [[:damage-gravehold 1]]}]}
              :gravehold  {:life 29}}))
      (is (= (-> {:real-game? true
                  :nemesis    {:deck [{:name    :last-attack
                                       :type    :attack
                                       :effects [[:damage-gravehold 1]]}]}
                  :gravehold  {:life 30}}
                 draw-nemesis-card
                 (update :game-over dissoc :text))
             {:real-game? true
              :nemesis    {:discard [{:name    :last-attack
                                      :type    :attack
                                      :effects [[:damage-gravehold 1]]}]}
              :gravehold  {:life 29}
              :game-over  {:conclusion :victory}})))
    (testing "Gravehold defeated"
      (is (= (-> {:real-game? true
                  :nemesis    {:deck    [{}]
                               :unleash [[:damage-gravehold 1]]}
                  :gravehold  {:life 2}}
                 unleash)
             {:real-game? true
              :nemesis    {:deck    [{}]
                           :unleash [[:damage-gravehold 1]]}
              :gravehold  {:life 1}}))
      (is (= (-> {:real-game? true
                  :nemesis    {:deck    [{}]
                               :unleash [[:damage-gravehold 1]]}
                  :gravehold  {:life 1}}
                 unleash
                 (update :game-over dissoc :text))
             {:real-game? true
              :nemesis    {:deck    [{}]
                           :unleash [[:damage-gravehold 1]]}
              :gravehold  {:life 0}
              :game-over  {:conclusion :defeat}}))
      (testing "before nemesis exhausted"
        (is (= (-> {:real-game? true
                    :nemesis    {:deck [{:name    :last-attack
                                         :type    :attack
                                         :effects [[:damage-gravehold 1]]}]}
                    :gravehold  {:life 1}}
                   draw-nemesis-card
                   (update :game-over dissoc :text))
               {:real-game? true
                :resolving  :last-attack
                :nemesis    {:play-area [{:name    :last-attack
                                          :type    :attack
                                          :effects [[:damage-gravehold 1]]}]}
                :gravehold  {:life 0}
                :game-over  {:conclusion :defeat}}))))
    (testing "Players exhausted"
      (is (= (-> {:real-game? true
                  :nemesis    {:deck [{}]}
                  :players    [{:life 2}
                               {:life 0}]}
                 (damage-player 0 1))
             {:real-game? true
              :nemesis    {:deck [{}]}
              :players    [{:life 1}
                           {:life 0}]}))
      (is (= (-> {:real-game? true
                  :nemesis    {:deck [{}]}
                  :players    [{:life 1}
                               {:life 0}]}
                 (damage-player 0 1)
                 (update :game-over dissoc :text))
             {:real-game? true
              :nemesis    {:deck [{}]}
              :players    [{:life           0
                            :revealed-cards 0}
                           {:life           0
                            :revealed-cards 0}]
              :game-over  {:conclusion :defeat}})))))

(deftest this-turn-test
  (testing "This turn"
    (let [jade (assoc jade :id 1)]
      (is (= (-> {:real-game? true
                  :supply     [{:card jade :pile-size 7}]
                  :players    [{:aether 2}]}
                 (buy-card 0 :jade))
             {:real-game? true
              :supply     [{:card jade :pile-size 6}]
              :players    [{:aether    0
                            :discard   [jade]
                            :this-turn [{:gain :jade}]}]})))
    (let [spark (assoc spark :id 1)]
      (is (= (-> {:real-game? true
                  :players    [{:hand     [spark]
                                :breaches [{:status :opened}]}]}
                 (prep-spell 0 0 :spark))
             {:real-game? true
              :players    [{:breaches  [{:status         :opened
                                         :prepped-spells [spark]}]
                            :this-turn [{:prep :spark :id 1}]}]})))
    (is (= (-> {:real-game? true
                :players    [{:hand [crystal]}]}
               (play 0 :crystal))
           {:real-game? true
            :players    [{:play-area [crystal]
                          :aether    1
                          :this-turn [{:play :crystal}]}]}))))

(deftest while-prepped-test
  (testing "While prepped"
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [{:name          :spell
                                                         :id            1
                                                         :while-prepped {:phase   :main
                                                                         :effects [[:gain-aether 1]]}}]}]
                           :phase    :casting}]}
               (use-while-prepped 0 0 :spell))
           {:players [{:breaches  [{:status         :opened
                                    :prepped-spells [{:name          :spell
                                                      :id            1
                                                      :while-prepped {:phase   :main
                                                                      :effects [[:gain-aether 1]]}}]}]
                       :aether    1
                       :this-turn [{:while-prepped 1}]
                       :phase     :main}]}))
    (is (= (-> {:players [{:breaches  [{:status         :opened
                                        :prepped-spells [{:name          :spell
                                                          :id            1
                                                          :while-prepped {:phase   :main
                                                                          :effects [[:gain-aether 1]]}}]}]
                           :this-turn [{:while-prepped 1}]
                           :phase     :casting}]}
               (use-while-prepped 0 0 :spell))
           {:players [{:breaches  [{:status         :opened
                                    :prepped-spells [{:name          :spell
                                                      :id            1
                                                      :while-prepped {:phase   :main
                                                                      :effects [[:gain-aether 1]]}}]}]
                       :aether    1
                       :this-turn [{:while-prepped 1}
                                   {:while-prepped 1}]
                       :phase     :main}]}))
    (is (thrown-with-msg? AssertionError #"While prepped error:"
                          (-> {:players [{:breaches  [{:status         :opened
                                                       :prepped-spells [{:name          :spell
                                                                         :id            1
                                                                         :while-prepped {:phase   :main
                                                                                         :once    true
                                                                                         :effects [[:gain-aether 1]]}}]}]
                                          :this-turn [{:while-prepped 1}]
                                          :phase     :main}]}
                              (use-while-prepped 0 0 :spell))))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [{:name          :spell
                                                         :id            1
                                                         :while-prepped {:phase    :main
                                                                         :can-use? [::power/can-afford? {:amount 1}]
                                                                         :effects  [[:pay {:amount 1}]]}}]}]
                           :aether   1
                           :phase    :main}]}
               (use-while-prepped 0 0 :spell))
           {:players [{:breaches  [{:status         :opened
                                    :prepped-spells [{:name          :spell
                                                      :id            1
                                                      :while-prepped {:phase    :main
                                                                      :can-use? [::power/can-afford? {:amount 1}]
                                                                      :effects  [[:pay {:amount 1}]]}}]}]
                       :aether    0
                       :this-turn [{:while-prepped 1}]
                       :phase     :main}]}))
    (is (thrown-with-msg? AssertionError #"Pay error:"
                          (-> {:players [{:breaches [{:status         :opened
                                                      :prepped-spells [{:name          :spell
                                                                        :id            1
                                                                        :while-prepped {:phase    :main
                                                                                        :can-use? [::power/can-afford? {:amount 1}]
                                                                                        :effects  [[:pay {:amount 1}]]}}]}]
                                          :aether   0
                                          :phase    :main}]}
                              (use-while-prepped 0 0 :spell))))
    (is (= (-> {:players [{:hand      [{:name          :spell
                                        :id            1
                                        :type          :spell
                                        :while-prepped {:phase   :main
                                                        :once    true
                                                        :effects [[:gain-aether 1]]}}]
                           :breaches  [{:status :opened}]
                           :this-turn [{:while-prepped 1}]
                           :phase     :main}]}
               (prep-spell 0 0 :spell)
               (use-while-prepped 0 0 :spell))
           {:players [{:breaches  [{:status         :opened
                                    :prepped-spells [{:name          :spell
                                                      :id            1
                                                      :type          :spell
                                                      :while-prepped {:phase   :main
                                                                      :once    true
                                                                      :effects [[:gain-aether 1]]}}]}]
                       :aether    1
                       :this-turn [{:while-prepped 1}]
                       :phase     :main}]}))
    (let [cairn-compass (assoc cairn-compass :id 2)]
      (is (= (-> {:real-game? true
                  :players    [{:hand      [cairn-compass]
                                :discard   [{:name :spell
                                             :id   1
                                             :type :spell}]
                                :breaches  [{:status :opened}]
                                :this-turn [{:while-prepped 1}]
                                :phase     :main}]}
                 (play 0 :cairn-compass)
                 (choose {:player-no 0 :card-id 1}))
             {:real-game? true
              :players    [{:play-area [cairn-compass]
                            :breaches  [{:status         :opened
                                         :prepped-spells [{:name :spell
                                                           :id   1
                                                           :type :spell}]}]
                            :this-turn [{:play :cairn-compass}
                                        {:prep :spell :id 1}]
                            :phase     :main}]})))))
