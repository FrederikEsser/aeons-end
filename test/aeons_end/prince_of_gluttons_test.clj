(ns aeons-end.prince-of-gluttons-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.prince-of-gluttons :as prince-of-gluttons :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.cards.gem :refer [burning-opal dread-diamond jade volcanic-glass]]
            [aeons-end.cards.relic :refer [flexing-dagger]]
            [aeons-end.cards.spell :refer []]
            [aeons-end.mages :refer [underearth-mantra]]
            [aeons-end.utils :as ut]))

(defn fixture [f]
  (with-rand-seed 124 (f)))

(use-fixtures :each fixture)

(deftest prince-of-gluttons-test
  (testing "Prince of Gluttons"
    (testing "Unleash"
      (ut/reset-ids! 0)
      (is (= (-> {:nemesis {:unleash [[::prince-of-gluttons/unleash]]}
                  :supply  [{:card jade :pile-size 7}]
                  :players [{} {} {}]}
                 unleash
                 (choose :jade))
             {:nemesis {:unleash  [[::prince-of-gluttons/unleash]]
                        :devoured [(assoc jade :id 1) (assoc jade :id 2)]}
              :supply  [{:card jade :pile-size 5}]
              :players [{} {} {}]}))
      (let [jade          (assoc jade :id 1)
            dread-diamond (assoc dread-diamond :id 2)]
        (is (= (-> {:nemesis   {:unleash [[::prince-of-gluttons/unleash]]}
                    :supply    [{:card jade :pile-size 3}]
                    :players   [{} {}]
                    :gravehold {:life 30}}
                   unleash
                   (choose :jade))
               {:nemesis   {:unleash  [[::prince-of-gluttons/unleash]]
                            :devoured [jade jade jade]}
                :supply    [{:card jade :pile-size 0}]
                :players   [{} {}]
                :gravehold {:life 30}}))
        (is (= (-> {:nemesis   {:unleash [[::prince-of-gluttons/unleash]]}
                    :supply    [{:card jade :pile-size 2}]
                    :players   [{} {}]
                    :gravehold {:life 30}}
                   unleash
                   (choose :jade))
               {:nemesis   {:unleash  [[::prince-of-gluttons/unleash]]
                            :devoured [jade jade]}
                :supply    [{:card jade :pile-size 0}]
                :players   [{} {}]
                :gravehold {:life 28}}))
        (is (= (-> {:nemesis   {:unleash [[::prince-of-gluttons/unleash]]}
                    :supply    [{:card jade :pile-size 1}
                                {:card flexing-dagger :pile-size 5}]
                    :players   [{} {}]
                    :gravehold {:life 30}}
                   unleash
                   (choose :jade))
               {:nemesis   {:unleash  [[::prince-of-gluttons/unleash]]
                            :devoured [jade]}
                :supply    [{:card jade :pile-size 0}
                            {:card flexing-dagger :pile-size 5}]
                :players   [{} {}]
                :gravehold {:life 26}}))
        (is (= (-> {:nemesis   {:unleash [[::prince-of-gluttons/unleash]]}
                    :supply    [{:card jade :pile-size 0}]
                    :players   [{} {}]
                    :gravehold {:life 30}}
                   unleash)
               {:nemesis   {:unleash [[::prince-of-gluttons/unleash]]}
                :supply    [{:card jade :pile-size 0}]
                :players   [{} {}]
                :gravehold {:life 24}}))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:nemesis {:unleash [[::prince-of-gluttons/unleash]]}
                                   :supply  [{:card jade :pile-size 0}
                                             {:card flexing-dagger :pile-size 5}]
                                   :players [{} {}]}
                                  unleash
                                  (choose :jade))))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:nemesis {:unleash [[::prince-of-gluttons/unleash]]}
                                   :supply  [{:card jade :pile-size 7}
                                             {:card dread-diamond :pile-size 7}]
                                   :players [{} {} {} {}]}
                                  unleash
                                  (choose :dread-diamond))))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:nemesis {:unleash  [[::prince-of-gluttons/unleash]]
                                             :devoured [jade jade]}
                                   :supply  [{:card jade :pile-size 5}]
                                   :players [{} {} {} {}]}
                                  unleash
                                  (choose :devoured))))
        (is (= (-> {:nemesis {:unleash [[::prince-of-gluttons/unleash]]}
                    :supply  [{:card jade :pile-size 0}
                              {:card dread-diamond :pile-size 7}]
                    :players [{} {} {} {}]}
                   unleash
                   (choose :dread-diamond))
               {:nemesis {:unleash  [[::prince-of-gluttons/unleash]]
                          :devoured [dread-diamond dread-diamond]}
                :supply  [{:card jade :pile-size 0}
                          {:card dread-diamond :pile-size 5}]
                :players [{} {} {} {}]}))))
    (testing "Gain from devoured"
      (let [jade (assoc jade :id 1)]
        (is (= (-> {:real-game? true
                    :nemesis    {:devoured [jade jade jade]}
                    :supply     [{:card jade :pile-size 4}]
                    :players    [{:aether 2}]}
                   (buy-card 0 :devoured))
               {:real-game? true
                :nemesis    {:devoured [jade jade]}
                :supply     [{:card jade :pile-size 4}]
                :players    [{:discard   [jade]
                              :aether    0
                              :this-turn [{:gain :jade}]}]}))
        (is (thrown-with-msg? AssertionError #"Pay error:"
                              (-> {:nemesis {:devoured [jade jade jade]}
                                   :supply  [{:card jade :pile-size 4}]
                                   :players [{:aether 1}]}
                                  (buy-card 0 :devoured))))
        (is (= (-> {:nemesis   {:devoured [jade jade jade]}
                    :supply    [{:card jade :pile-size 4}]
                    :players   [{:ability (assoc underearth-mantra :charges 4)}]
                    :gravehold {:life 20}}
                   (activate-ability 0)
                   (choose :devoured))
               {:nemesis   {:devoured [jade jade]}
                :supply    [{:card jade :pile-size 4}]
                :players   [{:ability (assoc underearth-mantra :charges 0)
                             :discard [jade]}]
                :gravehold {:life 24}}))
        (is (= (-> {:nemesis   {:devoured [jade jade jade]}
                    :supply    [{:card jade :pile-size 4}]
                    :players   [{:ability (assoc underearth-mantra :charges 4)}]
                    :gravehold {:life 20}}
                   (activate-ability 0)
                   (choose :jade))
               {:nemesis   {:devoured [jade jade jade]}
                :supply    [{:card jade :pile-size 3}]
                :players   [{:ability (assoc underearth-mantra :charges 0)
                             :discard [jade]}]
                :gravehold {:life 24}}))
        (is (= (-> {:nemesis   {:devoured [burning-opal]}
                    :supply    [{:card burning-opal :pile-size 6}]
                    :players   [{:ability (assoc underearth-mantra :charges 4)}]
                    :gravehold {:life 20}}
                   (activate-ability 0))
               {:nemesis   {:devoured [burning-opal]}
                :supply    [{:card burning-opal :pile-size 6}]
                :players   [{:ability (assoc underearth-mantra :charges 0)}]
                :gravehold {:life 24}}))
        (let [volcanic-glass (assoc volcanic-glass :id 3)]
          (is (= (-> {:current-player 0
                      :nemesis        {:devoured [volcanic-glass volcanic-glass]}
                      :supply         [{:card volcanic-glass :pile-size 1}]
                      :players        [{:aether 5}
                                       {}]}
                     (buy-card 0 :devoured)
                     (choose {:player-no 1}))
                 {:current-player 0
                  :nemesis        {:devoured []}
                  :supply         [{:card volcanic-glass :pile-size 1}]
                  :players        [{:discard [volcanic-glass]
                                    :aether  0}
                                   {:deck           [volcanic-glass]
                                    :revealed-cards 1}]}))
          (is (= (-> {:current-player 0
                      :nemesis        {:devoured [volcanic-glass]}
                      :supply         [{:card volcanic-glass :pile-size 1}]
                      :players        [{:aether 5}
                                       {}]}
                     (buy-card 0 :volcanic-glass)
                     (choose {:player-no 1}))
                 {:current-player 0
                  :nemesis        {:devoured []}
                  :supply         [{:card volcanic-glass :pile-size 0}]
                  :players        [{:discard [volcanic-glass]
                                    :aether  0}
                                   {:deck           [volcanic-glass]
                                    :revealed-cards 1}]}))
          (is (= (-> {:current-player 0
                      :nemesis        {:devoured [jade volcanic-glass]}
                      :supply         [{:card volcanic-glass :pile-size 1}]
                      :players        [{:aether 5}
                                       {}]}
                     (buy-card 0 :devoured)
                     (choose {:player-no 1}))
                 {:current-player 0
                  :nemesis        {:devoured [jade]}
                  :supply         [{:card volcanic-glass :pile-size 0}]
                  :players        [{:discard [volcanic-glass]
                                    :aether  0}
                                   {:deck           [volcanic-glass]
                                    :revealed-cards 1}]}))
          (is (= (-> {:current-player 0
                      :nemesis        {:devoured [volcanic-glass jade]}
                      :supply         [{:card volcanic-glass :pile-size 1}]
                      :players        [{:aether 5}
                                       {}]}
                     (buy-card 0 :volcanic-glass))
                 {:current-player 0
                  :nemesis        {:devoured [volcanic-glass jade]}
                  :supply         [{:card volcanic-glass :pile-size 0}]
                  :players        [{:discard [volcanic-glass]
                                    :aether  2}
                                   {}]})))))))
