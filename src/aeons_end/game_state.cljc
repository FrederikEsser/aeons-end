(ns aeons-end.game-state
  (:require [aeons-end.setup :as setup]
            [aeons-end.commands :as commands]
            [aeons-end.front-end-view :as front-end]))

(defonce game-state (atom {}))

(defn get-game []
  (-> @game-state
      :game
      first
      (assoc :can-undo? true)))

(defn view []
  (-> @game-state :game first front-end/view-game))

(defn undo []
  (let [{:keys [can-undo?]} (-> @game-state :game first)]
    (assert can-undo? "Unable to undo last move.")
    (swap! game-state update :game (partial drop 1))
    (view)))

(defn restart []
  (swap! game-state update :game (partial take-last 1))
  (view))

(defn start-game []
  (let [game (setup/create-game)]
    (swap! game-state assoc :game (-> game
                                      (commands/start-game)
                                      list))
    (view)))

(defn play [card-name]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/play current-player card-name)))
    (view)))

(defn play-all-gems []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/play-all-gems current-player)))
    (view)))

(defn choose [option]
  (let [game (get-game)]
    (swap! game-state update :game conj (-> game
                                            #_(op/choose option)))
    (view)))

(defn buy [card-name]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/buy-card current-player card-name)))
    (view)))

(defn charge-ability []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/charge-ability current-player)))
    (view)))

(defn activate-ability []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/activate-ability current-player)))
    (view)))

(defn focus-breach [breach-no]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/focus-breach current-player breach-no)))
    (view)))

(defn open-breach [breach-no]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/open-breach current-player breach-no)))
    (view)))

(defn prep-spell [card-name breach-no]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/prep-spell current-player card-name breach-no)))
    (view)))

(defn cast-spell [card-name breach-no]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/cast-spell current-player card-name breach-no)))
    (view)))

(defn discard [card-name]
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/discard current-player card-name)))
    (view)))

(defn discard-all []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/discard-all current-player)))
    (view)))

(defn end-turn []
  (let [{:keys [current-player] :as game} (get-game)]
    (swap! game-state update :game conj (-> game
                                            (commands/end-turn current-player)))
    (view)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello World!"))
