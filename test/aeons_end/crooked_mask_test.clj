(ns aeons-end.crooked-mask-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.crooked-mask :as crooked-mask :refer :all]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.turn-order :as turn-order]
            [aeons-end.cards.gem :refer []]
            [aeons-end.cards.relic :refer [fiend-catcher]]
            [aeons-end.cards.spell :refer []]))

(defn fixture [f]
  (with-rand-seed 124 (f)))

(use-fixtures :each fixture)

(def corruption-card {:id       1
                      :name     :corruption
                      :type     :corruption
                      :text     "Destroy this"
                      :effects  [[:destroy-this]]
                      :on-trash [[::crooked-mask/corruption-on-trash]]})

(def corruption-card-2 (assoc corruption-card :id 2
                                              :name :corruption-2))

(defn do-resolve-corruption [game player-no]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   [[::crooked-mask/resolve-corruption]]})
      check-stack))

(deftest crooked-mask-test
  (testing "Crooked Mask"
    (testing "Unleash"
      (is (= (-> {:nemesis {:corruption-deck [corruption-card]
                            :unleash         [[::crooked-mask/unleash]]}
                  :players [{}]}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis {:corruption-deck []
                        :unleash         [[::crooked-mask/unleash]]}
              :players [{:deck [corruption-card]}]}))
      (is (= (-> {:nemesis {:corruption-deck [corruption-card]
                            :unleash         [[::crooked-mask/unleash]]}
                  :players [{:deck    [crystal crystal]
                             :discard [spark spark]}]}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis {:corruption-deck []
                        :unleash         [[::crooked-mask/unleash]]}
              :players [{:deck [crystal crystal spark corruption-card spark]}]}))
      (is (= (-> {:nemesis {:corruption-deck [corruption-card]
                            :unleash         [[::crooked-mask/unleash]]}
                  :players [{:deck [crystal crystal spark spark]}]}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis {:corruption-deck []
                        :unleash         [[::crooked-mask/unleash]]}
              :players [{:deck [spark spark crystal corruption-card crystal]}]}))
      (is (= (-> {:nemesis   {:corruption-deck []
                              :unleash         [[::crooked-mask/unleash]]}
                  :players   [{:deck    [crystal crystal]
                               :discard [spark spark]}]
                  :gravehold {:life 30}}
                 unleash
                 (choose {:player-no 0}))
             {:nemesis   {:corruption-deck []
                          :unleash         [[::crooked-mask/unleash]]}
              :players   [{:deck [crystal spark spark crystal]}]
              :gravehold {:life 28}})))
    (testing "When destroyed"
      (is (= (-> {:difficulty :normal
                  :nemesis    {:corruption-deck [corruption-card-2]}
                  :players    [{:hand [corruption-card]}]}
                 (play 0 :corruption))
             {:difficulty :normal
              :nemesis    {:corruption-deck [corruption-card-2 corruption-card]}
              :players    [{}]}))
      (is (= (-> {:difficulty :normal
                  :nemesis    {:corruption-deck [corruption-card-2]}
                  :players    [{:hand    [fiend-catcher]
                                :discard [corruption-card]}]
                  :turn-order {:deck [turn-order/player-1]}}
                 (play 0 :fiend-catcher)
                 (choose {:area :discard :player-no 0 :card-name :corruption :card-id 1}))
             {:difficulty :normal
              :nemesis    {:corruption-deck [corruption-card-2 corruption-card]}
              :players    [{:play-area [fiend-catcher]}]
              :turn-order {:deck           [turn-order/player-1]
                           :revealed-cards 1}}))
      (is (= (-> {:difficulty :expert
                  :nemesis    {:corruption-deck [corruption-card-2]}
                  :players    [{:hand [corruption-card]}
                               {}]}
                 (play 0 :corruption)
                 (choose {:player-no 1}))
             {:difficulty :expert
              :nemesis    {:corruption-deck [corruption-card-2]}
              :players    [{}
                           {:discard [corruption-card]}]}))
      (is (= (-> {:difficulty :expert
                  :players    [{:hand [fiend-catcher corruption-card]}
                               {}]
                  :turn-order {:deck [turn-order/player-1]}}
                 (play 0 :fiend-catcher)
                 (choose {:area :hand :player-no 0 :card-name :corruption})
                 (choose {:player-no 0}))
             {:difficulty :expert
              :players    [{:play-area [fiend-catcher]
                            :discard   [corruption-card]}
                           {}]
              :turn-order {:deck           [turn-order/player-1]
                           :revealed-cards 1}})))
    (testing "Resolve corruption"
      (is (= (-> {:difficulty :normal
                  :nemesis    {}
                  :players    [{}]}
                 (do-resolve-corruption 0))
             {:difficulty :normal
              :nemesis    {}
              :players    [{}]}))
      (is (= (-> {:difficulty :normal
                  :players    [{:hand [{:name :corruption :type :corruption}]}]}
                 (do-resolve-corruption 0)
                 (choose :corruption))
             {:difficulty :normal
              :players    [{:play-area [{:name :corruption :type :corruption}]}]}))
      (is (= (-> {:difficulty :normal
                  :players    [{:hand [{:name :corruption :type :corruption}
                                       {:name :corruption-2 :type :corruption}]}]}
                 (do-resolve-corruption 0)
                 (choose :corruption-2)
                 (choose :corruption))
             {:difficulty :normal
              :players    [{:play-area [{:name :corruption-2 :type :corruption}
                                        {:name :corruption :type :corruption}]}]}))
      (is (= (-> {:nemesis {:at-start-casting [[::crooked-mask/resolve-corruption]]}
                  :players [{:hand  [{:name :corruption :type :corruption} crystal]
                             :phase :out-of-turn}]}
                 (set-phase 0 :casting)
                 (choose :corruption))
             {:nemesis {:at-start-casting [[::crooked-mask/resolve-corruption]]}
              :players [{:hand      [crystal]
                         :play-area [{:name :corruption :type :corruption}]
                         :phase     :casting}]}))
      (is (= (-> {:nemesis {:at-start-casting [[::crooked-mask/resolve-corruption]]}
                  :players [{:hand  [{:name :corruption :type :corruption}
                                     {:name :corruption-2 :type :corruption}]
                             :phase :out-of-turn}]}
                 (set-phase 0 :casting)
                 (choose :corruption-2)
                 (choose :corruption))
             {:nemesis {:at-start-casting [[::crooked-mask/resolve-corruption]]}
              :players [{:play-area [{:name :corruption-2 :type :corruption}
                                     {:name :corruption :type :corruption}]
                         :phase     :casting}]}))
      (is (= (-> {:nemesis {:at-end-main [[::crooked-mask/resolve-corruption]]}
                  :players [{:hand  [{:name :corruption :type :corruption}]
                             :phase :main}]}
                 (set-phase 0 :draw)
                 (choose :corruption))
             {:nemesis {:at-end-main [[::crooked-mask/resolve-corruption]]}
              :players [{:play-area [{:name :corruption :type :corruption}]
                         :phase     :draw}]})))))
