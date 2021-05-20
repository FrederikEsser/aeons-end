(ns aeons-end.relics-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.base :refer [crystal spark]]
            [aeons-end.cards.relics :refer :all]
            [aeons-end.cards.gems :refer [jade]]
            [aeons-end.cards.spells :refer [dark-fire radiance]]))

(deftest unstable-prism-test
  (testing "Unstable Prism"
    (is (= (-> {:players [{:hand [unstable-prism]}]}
               (play 0 :unstable-prism))
           {:players [{:play-area [unstable-prism]
                       :aether    2}]}))
    (is (= (-> {:players [{:hand [unstable-prism crystal]}]}
               (play 0 :unstable-prism)
               (choose []))
           {:players [{:hand      [crystal]
                       :play-area [unstable-prism]
                       :aether    2}]}))
    (is (= (-> {:players [{:hand [unstable-prism crystal]}]}
               (play 0 :unstable-prism)
               (choose :crystal))
           {:players [{:play-area [unstable-prism]
                       :aether    2}]
            :trash   [crystal]}))
    (is (= (-> {:players [{:hand [unstable-prism jade]}]}
               (play 0 :unstable-prism)
               (choose :jade))
           {:players [{:play-area [unstable-prism]
                       :aether    4}]
            :trash   [jade]}))
    (is (= (-> {:players [{:hand [unstable-prism unstable-prism]}]}
               (play 0 :unstable-prism))
           {:players [{:hand      [unstable-prism]
                       :play-area [unstable-prism]
                       :aether    2}]}))))

(deftest vortex-gauntlet-test
  (testing "Vortex Gauntlet"
    (let [spark (assoc spark :id 1)]
      (is (= (-> {:nemesis {:life 50}
                  :players [{:breaches [{:prepped-spells [spark]}]
                             :hand     [vortex-gauntlet]}]}
                 (play 0 :vortex-gauntlet)
                 (choose {:player-no 0
                          :breach-no 0
                          :card-name :spark}))
             {:nemesis {:life 49}
              :players [{:breaches  [{}]
                         :hand      [spark]
                         :play-area [vortex-gauntlet]}]}))
      (is (= (-> {:nemesis {:life 50}
                  :players [{:hand [vortex-gauntlet]}]}
                 (play 0 :vortex-gauntlet))
             {:nemesis {:life 50}
              :players [{:play-area [vortex-gauntlet]}]}))
      (is (= (-> {:nemesis {:life 50}
                  :players [{:hand [vortex-gauntlet]}
                            {:breaches [{:prepped-spells [spark]}]}]}
                 (play 0 :vortex-gauntlet)
                 (choose {:player-no 1
                          :breach-no 0
                          :card-name :spark}))
             {:nemesis {:life 49}
              :players [{:play-area [vortex-gauntlet]}
                        {:breaches [{}]
                         :hand     [spark]}]})))
    (let [dark-fire-1 (assoc dark-fire :id 1)
          dark-fire-2 (assoc dark-fire :id 2)]
      (is (= (-> {:nemesis {:life 50}
                  :players [{:breaches [{:prepped-spells [dark-fire-1]}]
                             :hand     [vortex-gauntlet dark-fire-2 crystal]}]}
                 (play 0 :vortex-gauntlet)
                 (choose {:player-no 0
                          :breach-no 0
                          :card-name :dark-fire})
                 (choose [:crystal :dark-fire]))            ; discard 2 cards to make the cast Dark Fire deal damage
             {:nemesis {:life 44}
              :players [{:breaches  [{}]
                         :hand      [dark-fire-1]
                         :play-area [vortex-gauntlet]
                         :discard   [crystal dark-fire-2]}]})))
    (let [radiance (assoc radiance :id 1)]
      (is (= (-> {:nemesis {:life 50}
                  :players [{:hand [vortex-gauntlet]}
                            {:breaches [{:prepped-spells [radiance]}]
                             :discard  [crystal]}]}
                 (play 0 :vortex-gauntlet)
                 (choose {:player-no 1
                          :breach-no 0
                          :card-name :radiance}))
             {:nemesis {:life 45}
              :players [{:play-area [vortex-gauntlet]}
                        {:breaches [{}]
                         :hand     [crystal]
                         :deck     [radiance]}]})))))
