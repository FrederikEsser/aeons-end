(ns aeons-end.crooked-mask-test
  (:require [clojure.test :refer :all]
            [aeons-end.test-utils :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.nemeses.crooked-mask :as crooked-mask :refer :all]
            [aeons-end.cards.attack :as attack]
            [aeons-end.cards.starter :refer :all]
            [aeons-end.turn-order :as turn-order]
            [aeons-end.cards.gem :refer [haunted-berylite jade sifters-pearl]]
            [aeons-end.cards.relic :refer [mages-totem fiend-catcher temporal-helix]]
            [aeons-end.cards.spell :refer [ignite radiance]]))

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

(deftest corruption-card-test
  (testing "Corruption cards"
    (testing "Blind Abandon"
      (let [blind-abandon (assoc blind-abandon :id 1)]
        (is (= (-> {:difficulty :normal
                    :nemesis    {}
                    :players    [{:hand [blind-abandon crystal]
                                  :life 10}]}
                   (play 0 :blind-abandon)
                   (choose :crystal))
               {:difficulty :normal
                :nemesis    {:corruption-deck [blind-abandon]}
                :players    [{:life 9}]
                :trash      [crystal]}))
        (is (= (-> {:difficulty :normal
                    :nemesis    {}
                    :players    [{:hand [blind-abandon corruption-card]
                                  :life 10}]}
                   (play 0 :blind-abandon))
               {:difficulty :normal
                :nemesis    {:corruption-deck [blind-abandon]}
                :players    [{:hand [corruption-card]
                              :life 9}]}))))
    (testing "Contagion"
      (let [contagion     (assoc contagion :id 1)
            crystal       (assoc crystal :id 2)
            fiend-catcher (assoc fiend-catcher :id 3)]
        (is (= (-> {:difficulty :normal
                    :nemesis    {}
                    :players    [{:hand    [contagion]
                                  :discard [crystal]
                                  :life    10}]}
                   (play 0 :contagion)
                   (choose {:player-no 0 :card-name :crystal :card-id 2}))
               {:difficulty :normal
                :nemesis    {:corruption-deck [contagion]}
                :players    [{:hand [crystal]
                              :life 9}]}))
        (is (= (-> {:difficulty :normal
                    :nemesis    {}
                    :players    [{:hand    [contagion]
                                  :discard [corruption-card fiend-catcher]
                                  :life    10}]}
                   (play 0 :contagion))
               {:difficulty :normal
                :nemesis    {:corruption-deck [contagion]}
                :players    [{:discard [corruption-card fiend-catcher]
                              :life    9}]}))))
    (testing "Delirium Veil"
      (let [delirium-veil (assoc delirium-veil :id 1)]
        (is (= (-> {:difficulty :normal
                    :nemesis    {}
                    :players    [{:hand     [delirium-veil]
                                  :breaches [{:status :opened}
                                             {:status     :closed
                                              :focus-cost 4
                                              :stage      2}]}]
                    :gravehold  {:life 30}}
                   (play 0 :delirium-veil)
                   (choose {:player-no 0 :breach-no 1}))
               {:difficulty :normal
                :nemesis    {:corruption-deck [delirium-veil]}
                :players    [{:breaches [{:status :opened}
                                         {:status     :focused
                                          :focus-cost 4
                                          :stage      3}]}]
                :gravehold  {:life 28}}))
        (is (= (-> {:difficulty :normal
                    :nemesis    {}
                    :players    [{:hand     [delirium-veil]
                                  :breaches [{:status :opened}
                                             {:status :destroyed}]}]
                    :gravehold  {:life 30}}
                   (play 0 :delirium-veil))
               {:difficulty :normal
                :nemesis    {:corruption-deck [delirium-veil]}
                :players    [{:breaches [{:status :opened}
                                         {:status :destroyed}]}]
                :gravehold  {:life 28}}))))
    (testing "Dire Wisdom"
      (let [dire-wisdom (assoc dire-wisdom :id 1)
            radiance    (assoc radiance :id 2)]
        (is (= (-> {:difficulty :normal
                    :supply     [{:card radiance :pile-size 5}]
                    :nemesis    {:corruption-deck [corruption-card corruption-card corruption-card corruption-card]}
                    :players    [{:hand [dire-wisdom]
                                  :deck [crystal]}]}
                   (play 0 :dire-wisdom)
                   (choose :radiance))
               {:difficulty :normal
                :supply     [{:card radiance :pile-size 4}]
                :nemesis    {:corruption-deck [corruption-card dire-wisdom]}
                :players    [{:deck    [corruption-card corruption-card corruption-card crystal]
                              :discard [radiance]}]}))
        (is (= (-> {:difficulty :normal
                    :supply     [{:card radiance :pile-size 0}
                                 {:card fiend-catcher :pile-size 5}]
                    :nemesis    {:corruption-deck [corruption-card corruption-card corruption-card corruption-card]}
                    :players    [{:hand [dire-wisdom]
                                  :deck [crystal]}]}
                   (play 0 :dire-wisdom))
               {:difficulty :normal
                :supply     [{:card radiance :pile-size 0}
                             {:card fiend-catcher :pile-size 5}]
                :nemesis    {:corruption-deck [corruption-card dire-wisdom]}
                :players    [{:deck [corruption-card corruption-card corruption-card crystal]}]}))))
    (testing "Grim Sight"
      (let [grim-sight (assoc grim-sight :id 1)]
        (is (= (-> {:difficulty :normal
                    :nemesis    {}
                    :players    [{:hand [grim-sight]
                                  :deck [crystal]}]
                    :gravehold  {:life 30}}
                   (play 0 :grim-sight)
                   (choose :crystal))
               {:difficulty :normal
                :nemesis    {:corruption-deck [grim-sight]}
                :players    [{}]
                :gravehold  {:life 28}
                :trash      [crystal]}))
        (is (= (-> {:difficulty :normal
                    :nemesis    {}
                    :players    [{:hand [grim-sight]
                                  :deck [crystal]}]
                    :gravehold  {:life 30}}
                   (play 0 :grim-sight)
                   (choose nil))
               {:difficulty :normal
                :nemesis    {:corruption-deck [grim-sight]}
                :players    [{:deck           [crystal]
                              :revealed-cards 1}]
                :gravehold  {:life 28}}))))
    (testing "Insatiable Avarice"
      (let [insatiable-avarice (assoc insatiable-avarice :id 1)
            haunted-berylite   (assoc haunted-berylite :id 2)]
        (is (= (-> {:difficulty :normal
                    :supply     [{:card haunted-berylite :pile-size 7}]
                    :nemesis    {}
                    :players    [{:hand [insatiable-avarice]
                                  :life 10}]}
                   (play 0 :insatiable-avarice)
                   (choose :haunted-berylite))
               {:difficulty :normal
                :supply     [{:card haunted-berylite :pile-size 6}]
                :nemesis    {:corruption-deck [insatiable-avarice]}
                :players    [{:hand [haunted-berylite]
                              :life 8}]}))
        (is (= (-> {:difficulty :normal
                    :supply     [{:card jade :pile-size 0}
                                 {:card haunted-berylite :pile-size 7}
                                 {:card fiend-catcher :pile-size 5}
                                 {:card radiance :pile-size 5}]
                    :nemesis    {}
                    :players    [{:hand [insatiable-avarice]
                                  :life 10}]}
                   (play 0 :insatiable-avarice))
               {:difficulty :normal
                :supply     [{:card jade :pile-size 0}
                             {:card haunted-berylite :pile-size 7}
                             {:card fiend-catcher :pile-size 5}
                             {:card radiance :pile-size 5}]
                :nemesis    {:corruption-deck [insatiable-avarice]}
                :players    [{:life 8}]}))
        (is (= (-> {:difficulty :normal
                    :supply     [{:card haunted-berylite :pile-size 7}
                                 {:card sifters-pearl :pile-size 0}]
                    :nemesis    {}
                    :players    [{:hand [insatiable-avarice]
                                  :life 10}]}
                   (play 0 :insatiable-avarice)
                   (choose :haunted-berylite))
               {:difficulty :normal
                :supply     [{:card haunted-berylite :pile-size 6}
                             {:card sifters-pearl :pile-size 0}]
                :nemesis    {:corruption-deck [insatiable-avarice]}
                :players    [{:hand [haunted-berylite]
                              :life 8}]}))
        (is (thrown-with-msg? AssertionError #"Choose error:"
                              (-> {:difficulty :normal
                                   :supply     [{:card haunted-berylite :pile-size 7}
                                                {:card sifters-pearl :pile-size 0}]
                                   :nemesis    {}
                                   :players    [{:hand [insatiable-avarice]
                                                 :life 10}]}
                                  (play 0 :insatiable-avarice)
                                  (choose :sifter's-pearl))))))
    (testing "Reckless Might"
      (let [reckless-might (assoc reckless-might :id 1)
            mages-totem    (assoc mages-totem :id 2)
            temporal-helix (assoc temporal-helix :id 3)]
        (is (= (-> {:difficulty :normal
                    :supply     [{:card mages-totem :pile-size 5}
                                 {:card temporal-helix :pile-size 5}]
                    :nemesis    {}
                    :players    [{:hand [reckless-might]
                                  :deck [crystal]
                                  :life 10}]}
                   (play 0 :reckless-might)
                   (choose :mage's-totem))
               {:difficulty :normal
                :supply     [{:card mages-totem :pile-size 4}
                             {:card temporal-helix :pile-size 5}]
                :nemesis    {:corruption-deck [reckless-might]}
                :players    [{:deck           [mages-totem crystal]
                              :revealed-cards 1
                              :life           9}]}))
        (is (= (-> {:difficulty :normal
                    :supply     [{:card mages-totem :pile-size 5}
                                 {:card temporal-helix :pile-size 5}]
                    :nemesis    {}
                    :players    [{:hand [reckless-might]
                                  :deck [crystal]
                                  :life 10}]}
                   (play 0 :reckless-might)
                   (choose :temporal-helix))
               {:difficulty :normal
                :supply     [{:card mages-totem :pile-size 5}
                             {:card temporal-helix :pile-size 4}]
                :nemesis    {:corruption-deck [reckless-might]}
                :players    [{:deck           [temporal-helix crystal]
                              :revealed-cards 1
                              :life           6}]}))
        (is (= (-> {:difficulty :normal
                    :supply     [{:card jade :pile-size 7}
                                 {:card mages-totem :pile-size 0}
                                 {:card temporal-helix :pile-size 0}
                                 {:card radiance :pile-size 5}]
                    :nemesis    {}
                    :players    [{:hand [reckless-might]
                                  :deck [crystal]
                                  :life 10}]}
                   (play 0 :reckless-might))
               {:difficulty :normal
                :supply     [{:card jade :pile-size 7}
                             {:card mages-totem :pile-size 0}
                             {:card temporal-helix :pile-size 0}
                             {:card radiance :pile-size 5}]
                :nemesis    {:corruption-deck [reckless-might]}
                :players    [{:deck [crystal]
                              :life 10}]}))))))

(deftest corrupter-test
  (testing "Corrupter"
    (is (= (-> {:nemesis {:play-area       [(assoc corrupter :life 6)]
                          :corruption-deck [corruption-card corruption-card-2]}
                :players [{:deck [crystal spark]}]}
               (deal-damage 2)
               (choose {:area :minions :player-no 0 :card-name :corrupter}))
           {:nemesis {:play-area       [(assoc corrupter :life 4)]
                      :corruption-deck [corruption-card-2]}
            :players [{:deck [corruption-card crystal spark]}]}))
    (is (= (-> {:nemesis {:play-area       [(assoc corrupter :life 1)]
                          :corruption-deck [corruption-card corruption-card-2]}
                :players [{:deck [crystal spark]}]}
               (deal-damage 1)
               (choose {:area :minions :player-no 0 :card-name :corrupter}))
           {:nemesis {:discard         [(assoc corrupter :life 0)]
                      :corruption-deck [corruption-card-2]}
            :players [{:deck [corruption-card crystal spark]}]}))
    (is (= (-> {:nemesis {:play-area       [(assoc corrupter :life 1)]
                          :corruption-deck [corruption-card corruption-card-2]}
                :players [{:deck [crystal spark]}]}
               (deal-damage 0)
               (choose {:area :minions :player-no 0 :card-name :corrupter}))
           {:nemesis {:play-area       [(assoc corrupter :life 1)]
                      :corruption-deck [corruption-card corruption-card-2]}
            :players [{:deck [crystal spark]}]}))))

(deftest pain-sower-test
  (testing "Pain Sower"
    (is (= (-> {:current-player :nemesis
                :nemesis        {:play-area [pain-sower]}
                :players        [{:breaches [{:status     :closed
                                              :focus-cost 4
                                              :stage      0}]
                                  :life     10}
                                 {:breaches [{:status     :closed
                                              :focus-cost 4
                                              :stage      0}]
                                  :life     10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0})
               (choose {:player-no 1 :breach-no 0}))
           {:current-player :nemesis
            :nemesis        {:play-area [pain-sower]}
            :players        [{:breaches [{:status     :closed
                                          :focus-cost 4
                                          :stage      0}]
                              :life     8}
                             {:breaches [{:status     :closed
                                          :focus-cost 4
                                          :stage      1}]
                              :life     10}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:current-player :nemesis
                               :nemesis        {:play-area [pain-sower]}
                               :players        [{:breaches [{:status     :closed
                                                             :focus-cost 4
                                                             :stage      0}]
                                                 :life     10}
                                                {:breaches [{:status     :closed
                                                             :focus-cost 4
                                                             :stage      0}]
                                                 :life     10}]}
                              (resolve-nemesis-cards-in-play)
                              (choose {:player-no 0})
                              (choose {:player-no 0 :breach-no 0}))))
    (is (= (-> {:current-player :nemesis
                :nemesis        {:play-area [pain-sower]}
                :players        [{:breaches [{:status     :closed
                                              :focus-cost 4
                                              :stage      3}]
                                  :life     10}
                                 {:breaches [{:status :opened}]
                                  :life     2}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0}))
           {:current-player :nemesis
            :nemesis        {:play-area [pain-sower]}
            :players        [{:breaches [{:status     :closed
                                          :focus-cost 4
                                          :stage      3}]
                              :life     8}
                             {:breaches [{:status :opened}]
                              :life     2}]}))
    (is (= (-> {:current-player :nemesis
                :nemesis        {:play-area [pain-sower]}
                :players        [{:breaches [{:status     :closed
                                              :focus-cost 4
                                              :stage      3}]
                                  :life     10}]}
               (resolve-nemesis-cards-in-play)
               (choose {:player-no 0})
               (choose {:player-no 0 :breach-no 0}))
           {:current-player :nemesis
            :nemesis        {:play-area [pain-sower]}
            :players        [{:breaches [{:status :opened}]
                              :life     8}]}))))

(deftest tempt-test
  (testing "Tempt"
    (let [ignite (assoc ignite :id 1)]
      (is (= (-> {:nemesis {:deck [tempt]}
                  :supply  [{:card fiend-catcher :pile-size 5}
                            {:card ignite :pile-size 5}]
                  :players [{:hand [crystal crystal spark]
                             :life 10}]}
                 draw-nemesis-card
                 (choose {:player-no 0})
                 (choose :ignite))
             {:nemesis {:discard [tempt]}
              :supply  [{:card fiend-catcher :pile-size 5}
                        {:card ignite :pile-size 4}]
              :players [{:hand    [spark]
                         :discard [crystal crystal ignite]
                         :life    7}]})))
    (let [fiend-catcher (assoc fiend-catcher :id 1)]
      (is (= (-> {:nemesis {:deck [tempt]}
                  :supply  [{:card fiend-catcher :pile-size 5}
                            {:card ignite :pile-size 5}]
                  :players [{:hand [spark]
                             :life 10}]}
                 draw-nemesis-card
                 (choose {:player-no 0})
                 (choose :fiend-catcher))
             {:nemesis {:discard [tempt]}
              :supply  [{:card fiend-catcher :pile-size 4}
                        {:card ignite :pile-size 5}]
              :players [{:hand    [spark]
                         :discard [fiend-catcher]
                         :life    7}]})))
    (is (= (-> {:nemesis {:deck [tempt]}
                :supply  [{:card ignite :pile-size 0}
                          {:card radiance :pile-size 5}]
                :players [{:hand [crystal fiend-catcher spark ignite]
                           :life 10}
                          {:hand [crystal]
                           :life 10}]}
               draw-nemesis-card
               (choose {:player-no 1}))
           {:nemesis {:discard [tempt]}
            :supply  [{:card ignite :pile-size 0}
                      {:card radiance :pile-size 5}]
            :players [{:hand [crystal fiend-catcher spark ignite]
                       :life 10}
                      {:discard [crystal]
                       :life    7}]}))
    (is (thrown-with-msg? AssertionError #"Choose error:"
                          (-> {:nemesis {:deck [tempt]}
                               :supply  [{:card ignite :pile-size 4}
                                         {:card radiance :pile-size 5}]
                               :players [{:hand [crystal crystal fiend-catcher spark ignite]
                                          :life 10}
                                         {:hand [crystal]
                                          :life 10}]}
                              draw-nemesis-card
                              (choose {:player-no 1}))))))

(deftest vex-test
  (testing "Vex"
    (is (= (-> {:nemesis   {:deck [vex pain-sower twisting-madness bedlam-sage]}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:deck    [bedlam-sage]
                        :discard [pain-sower twisting-madness vex]}
            :gravehold {:life 30}}))
    (is (= (-> {:nemesis   {:deck [vex twisting-madness bedlam-sage attack/banish]}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:deck    [attack/banish]
                        :discard [twisting-madness bedlam-sage vex]}
            :gravehold {:life 26}}))
    (is (= (-> {:nemesis   {:deck [vex bedlam-sage attack/banish attack/quell]}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:deck    [attack/quell]
                        :discard [bedlam-sage attack/banish vex]}
            :gravehold {:life 22}}))
    (is (= (-> {:nemesis   {:deck [vex bedlam-sage]}
                :gravehold {:life 30}}
               draw-nemesis-card)
           {:nemesis   {:discard [bedlam-sage vex]}
            :gravehold {:life 26}}))))
