(ns aeons-end.spell-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.spell :refer :all]
            [aeons-end.cards.base :refer :all]
            [aeons-end.mages :refer [garnet-shard]]))

(deftest amplify-vision-test
  (testing "Amplify Vision"
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      0}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status     :focused
                                   :focus-cost 2
                                   :stage      1}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status     :focused
                                       :focus-cost 2
                                       :stage      3}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status :opened}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      3}
                                      {:status :opened}
                                      {:status     :closed
                                       :focus-cost 4
                                       :stage      3}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status :opened}
                                  {:status :opened}
                                  {:status     :closed
                                   :focus-cost 4
                                   :stage      3}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 48}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :prepped-spells [amplify-vision]}
                                      {:status :opened}
                                      {:status :opened}
                                      {:status     :closed
                                       :focus-cost 4
                                       :stage      3}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status :opened}
                                  {:status :opened}
                                  {:status :opened}
                                  {:status :opened}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [amplify-vision]}
                                      {:status     :closed
                                       :focus-cost 2
                                       :stage      0}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}
                                  {:status     :focused
                                   :focus-cost 2
                                   :stage      1}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [amplify-vision]}
                                      {:status     :focused
                                       :focus-cost 2
                                       :stage      3}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :amplify-vision))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}
                                  {:status :opened}]
                       :discard  [amplify-vision]}]
            :nemesis {:life 46}}))))

(deftest dark-fire-test
  (testing "Dark Fire"
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [crystal crystal spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose nil))
           {:players [{:breaches [{}]
                       :hand     [crystal crystal spark]
                       :discard  [dark-fire]}]
            :nemesis {:life 50}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose :spark))
           {:players [{:breaches [{}]
                       :discard  [dark-fire spark]}]
            :nemesis {:life 47}}))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                           :hand     [crystal crystal spark]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose [:crystal :crystal]))
           {:players [{:breaches [{}]
                       :hand     [spark]
                       :discard  [dark-fire crystal crystal]}]
            :nemesis {:life 44}}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]
                                          :hand     [crystal crystal spark]}]
                               :nemesis {:life 50}}
                              (cast-spell 0 0 :dark-fire)
                              (choose [:crystal :crystal :spark]))))
    (is (= (-> {:players [{:breaches [{:prepped-spells [dark-fire]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire))
           {:players [{:breaches [{}]
                       :discard  [dark-fire]}]
            :nemesis {:life 50}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [dark-fire]}]
                           :hand     [crystal]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose [:crystal]))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :discard  [dark-fire crystal]}]
            :nemesis {:life 46}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [dark-fire]}]
                           :hand     [crystal]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire)
               (choose nil))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :hand     [crystal]
                       :discard  [dark-fire]}]
            :nemesis {:life 49}}))
    (is (= (-> {:players [{:breaches [{:status         :opened
                                       :bonus-damage   1
                                       :prepped-spells [dark-fire]}]}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :dark-fire))
           {:players [{:breaches [{:status       :opened
                                   :bonus-damage 1}]
                       :discard  [dark-fire]}]
            :nemesis {:life 49}}))))

(deftest ignite-test
  (testing "Ignite"
    (is (= (-> {:players [{:breaches [{:prepped-spells [ignite]}]}
                          {:ability {:charges     0
                                     :charge-cost 4}}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :ignite)
               (choose {:player-no 1}))
           {:players [{:breaches [{}]
                       :discard  [ignite]}
                      {:ability {:charges     1
                                 :charge-cost 4}}]
            :nemesis {:life 48}}))
    (is (thrown-with-msg? AssertionError #"Choose error"
                          (-> {:players [{:breaches [{:prepped-spells [ignite]}]}
                                         {:ability {:charges     0
                                                    :charge-cost 4}}]
                               :nemesis {:life 50}}
                              (cast-spell 0 0 :ignite)
                              (choose {:player-no 0}))))
    (is (= (-> {:players [{:hand [garnet-shard]}
                          {:breaches [{:prepped-spells [ignite]}]
                           :ability  {:charges     0
                                      :charge-cost 4}}]
                :nemesis {:life 50}}
               (play 0 :garnet-shard)
               (choose {:player-no 1
                        :breach-no 0
                        :card-name :ignite})
               (choose {:player-no 1}))
           {:players [{:play-area [garnet-shard]}
                      {:breaches [{}]
                       :ability  {:charges     1
                                  :charge-cost 4}
                       :discard  [ignite]}]
            :nemesis {:life 48}}))))

(deftest radiance-test
  (testing "Radiance"
    (is (= (-> {:players [{:breaches [{:prepped-spells [radiance]}]
                           :deck     [crystal]}
                          {:deck [crystal crystal]}
                          {:deck [crystal]}
                          {}]
                :nemesis {:life 50}}
               (cast-spell 0 0 :radiance))
           {:players [{:breaches [{}]
                       :deck     [crystal]
                       :discard  [radiance]}
                      {:deck [crystal]
                       :hand [crystal]}
                      {:hand [crystal]}
                      {}]
            :nemesis {:life 45}}))))
