(ns aeons-end.test-utils
  (:require [clojure.test :refer :all]
            [aeons-end.operations :refer [push-effect-stack check-stack choose]]
            [aeons-end.cards.common]
            [aeons-end.nemesis]
            [aeons-end.turn-order]))

(defn rand-with-seed
  ([seed]
   (.nextFloat seed))
  ([seed n]
   (* n (.nextFloat seed))))

(defn shuffle-with-seed
  "Return a random permutation of coll"
  {:added  "1.2"
   :static true}
  [^java.util.Collection coll seed]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al seed)
    (clojure.lang.RT/vector (.toArray al))))

(defmacro with-rand-seed
  "Sets seed for calls to random in body. Beware of lazy seqs!"
  [seed & body]
  `(let [g# (java.util.Random. ~seed)]
     (with-redefs [rand     (partial rand-with-seed g#)
                   rand-int #(.nextInt g# %)
                   shuffle  #(shuffle-with-seed % g#)]
       ~@body)))

(deftest rand-test
  (with-rand-seed 123
                  (is (= 2 (rand-int 10)))
                  (is (= 0.2372438907623291 (rand)))
                  (is (= 9.90898847579956 (rand 10)))
                  (is (= [7 5 6 2 0 1 8 4 3 9] (shuffle (range 10))))))

(defn unleash [game]
  (-> game
      (push-effect-stack {:effects [[:unleash]]})
      check-stack))

(defn draw-nemesis-card [game]
  (let [{:keys [name]} (-> game :nemesis :deck first)]
   (-> game
       (push-effect-stack {:effects [[:draw-nemesis-card]]})
       check-stack
       (choose name))))

(defn resolve-nemesis-cards-in-play [game]
  (let [{:keys [name]} (-> game :nemesis :play-area first)]
  (-> game
      (push-effect-stack {:effects [[:resolve-nemesis-cards-in-play]]})
        check-stack
        (choose name))))

(defn deal-damage [game damage]
  (-> game
      (push-effect-stack {:effects [[:deal-damage damage]]})
      check-stack))

(defn damage-player [game player-no damage]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   [[:damage-player damage]]})
      check-stack))

(defn heal-player [game player-no life]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   [[:heal {:life life}]]})
      check-stack))

(defn set-phase [game player-no phase]
  (-> game
      (push-effect-stack {:player-no player-no
                          :effects   [[:set-phase {:phase phase}]]})
      check-stack))

(defn get-trigger [{:keys [id name trigger]} & [trigger-id]]
  (merge {:id (or trigger-id 1)}
         (when id {:card-id id})
         (when name {:name name})
         trigger))

(defn get-project-trigger [{:keys [name type trigger]}]
  (merge (if (= :project type)
           {:duration :game}
           {:duration name})
         {:id   1
          :name name}
         trigger))
