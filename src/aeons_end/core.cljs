(ns aeons-end.core
  (:require
    [reagent.core :as r]
    [aeons-end.commands :as cmd]
    [clojure.string :as string]
    [aeons-end.utils :as ut]))

;; -------------------------
;; Views

(def all-sets (->> [] #_kingdom/kingdom-cards
                   (map :set)
                   set))

(defonce state (r/atom {:sets        all-sets
                        :setup-game? true
                        :selection   []
                        :num-players 2
                        :players     ["Big Johnny" "Ivor the Engine Driver" "Dirty Maggie Mae"]}))

(defn select! [option]
  (swap! state update :selection conj option))

(defn remove-idx [coll idx]
  (let [v     (vec coll)
        start (subvec v 0 idx)
        end   (subvec v (inc idx) (count v))]
    (vec (concat start end))))

(defn deselect! [idx]
  (swap! state update :selection remove-idx idx))

(defn button-style [& [disabled types number-of-cards]]
  (merge {:color            (cond (and (:ruins types)
                                       disabled) "#7B3610"
                                  disabled :grey
                                  (:night types) :white
                                  (:landmark types) "#256A3D"
                                  (:hex types) "#5A487A"
                                  :else :black)
          :font-weight      :bold
          :background-color (cond
                              (:night types) "#464040"
                              (:duration types) "#FF9E37"
                              (:reaction types) "#A8BFD3"
                              (:reserve types) "#D3B86B"
                              (:ruins types) "#B06B24"
                              (:action types) "#F3EEDF"
                              (:treasure types) "#FFE64F"
                              (:victory types) "#9FD688"
                              (:curse types) "#B890D7"
                              (:artifact types) "#F9CD88"
                              (:event types) "#C6C8C5"
                              (:landmark types) "#60B574"
                              (:project types) "#FCA19A"
                              (:boon types) "#F6E359"
                              (:hex types) "#9677B3"
                              (:state types) "#F1EBEB")
          :border-color     (cond
                              (zero? number-of-cards) :red
                              (:curse types) "#9F76B8"
                              (:shelter types) "#E76F59"
                              (:victory types) "#6DB954"
                              (:treasure types) "#EFD34E"
                              (:reaction types) "#6295CE"
                              (:reserve types) "#C6A85C"
                              (:duration types) "#F1820E"
                              (:ruins types) "#7B3610"
                              (:attack types) "#B40000"
                              (:action types) "#DED7C4"
                              (:night types) "#413B3B"
                              (:artifact types) "#B4763B"
                              (:event types) "#97998E"
                              (:landmark types) "#459A5D"
                              (:project types) "#EF8984"
                              (:boon types) "#AD9727"
                              (:hex types) "#5A487A"
                              (:state types) "#CE9883"
                              :else :grey)
          :border-width     2}
         (when (:attack types)
           {:border-style :dotted})))

(defn mapk [f coll]
  (->> coll
       (map (fn [e]
              (-> e f (with-meta {:key (random-uuid)}))))
       doall))

(defn mapk-indexed [f coll]
  (->> coll
       (map-indexed (fn [i e]
                      (with-meta (f i e) {:key (random-uuid)})))
       doall))

(defn map-tag [tag coll & [style]]
  (mapk (fn [x] [tag style x]) coll))

(defn set-selector []
  (fn [sets set-name]
    [:div
     (ut/format-name set-name)
     [:input {:type      :checkbox
              :checked   (contains? sets set-name)
              :on-change #(if (contains? sets set-name)
                            (swap! state update :sets disj set-name)
                            (swap! state update :sets conj set-name))}]]))

(defn setup-player [idx]
  [:div
   (str "Player " (inc idx) ": ")
   [:input {:type      :text
            :on-change #(swap! state assoc-in [:players idx] (-> % .-target .-value))
            :value     (get-in @state [:players idx]) #_player}]])

(defn home-page []
  (fn []
    (let [{:keys [setup-game? selection boons-unfolded? hexes-unfolded?]} @state]
      [:div [:h2 "Aeon's End"]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
