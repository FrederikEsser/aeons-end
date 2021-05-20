(ns aeons-end.relics-test
  (:require [clojure.test :refer :all]
            [aeons-end.commands :refer :all]
            [aeons-end.operations :refer [choose]]
            [aeons-end.cards.base :refer [crystal spark]]
            [aeons-end.cards.relics :refer :all]
            [aeons-end.cards.gems :refer [jade]]))

(deftest unstable-prism-test
  (testing "Unstable Prism"
    (is (= (-> {:players [{:hand [unstable-prism]}]}
               (play 0 :unstable-prism)
               (choose :aether))
           {:players [{:play-area [unstable-prism]
                       :aether    2}]}))
    (is (= (-> {:players [{:hand [unstable-prism]}]}
               (play 0 :unstable-prism)
               (choose :play-gem))
           {:players [{:play-area [unstable-prism]}]}))
    (is (= (-> {:players [{:hand [unstable-prism crystal]}]}
               (play 0 :unstable-prism)
               (choose :play-gem)
               (choose :crystal))
           {:players [{:play-area [unstable-prism]
                       :aether    2}]
            :trash   [crystal]}))
    (is (= (-> {:players [{:hand [unstable-prism jade]}]}
               (play 0 :unstable-prism)
               (choose :play-gem)
               (choose :jade))
           {:players [{:play-area [unstable-prism]
                       :aether    4}]
            :trash   [jade]}))
    (is (= (-> {:players [{:hand [unstable-prism unstable-prism]}]}
               (play 0 :unstable-prism)
               (choose :play-gem))
           {:players [{:hand      [unstable-prism]
                       :play-area [unstable-prism]}]}))))
