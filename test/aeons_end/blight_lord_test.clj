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
                :players [{:life 9}]}))))))
