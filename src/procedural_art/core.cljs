(ns procedural-art.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(defonce app-state (atom nil))

(defn label-str
  [label s]
  (str label ": " s "\n"))

(def sierpinski-arrowhead
  {:start [:a]
   :rules {:a [:b :- :a :- :b]
           :b [:a :+ :b :+ :a]
           :- [:-]
           :+ [:+]}})

(def koch-curve
  {:start [:f]
   :rules {:f [:f :+ :f :- :f :- :f :+ :f]
           :+ [:+]
           :- [:-]}})

(def l-system koch-curve)

(defn expand-symbol
  [symbol]
  (get (:rules l-system) symbol))

(defn next-symbols
  [symbols]
  (mapcat expand-symbol symbols))

(defn transform-n-times
  [f n start]
  (->> (repeat n 1)
       (reduce (fn [prior _] (f prior)) start)))



(defn to-radians [degrees]
  (* degrees (/ Math/PI 180)))

(defn add-location-to-history
  [{:keys [pos history] :as state}]
  (update state :history #(conj % pos)))

(defn turn [state angle]
  (update state :heading #(+ angle %)))

(defn left [state] (turn state (to-radians 90)))
(defn right [state] (turn state (to-radians -90)))

(defn forward
  [{:keys [heading step-size] :as state}]
  (let [dx (* step-size (Math/cos heading))
        dy (* step-size (Math/sin heading))]
    (-> state
        add-location-to-history
        (update-in [:pos :x] #(+ % dx))
        (update-in [:pos :y] #(+ % dy)))))

(defn apply-move-to-state
  [state move]
  (let [{:keys [handlers]} state
        action (move handlers)]
    (-> state
        action)))

(defn apply-moves
  [start-state moves]
  (reduce (fn [state move]
            (apply-move-to-state state move))
          start-state
          moves))

(defn debug-render
  [symbols]
  (->> symbols
       (map #(name %))
       (clojure.string/join " ")))

(def handlers
  {:f forward
   :+ left
   :- right})

(def default-state
  {:pos {:x 0 :y 0}
   :heading 0
   :step-size 1
   :history nil
   :handlers handlers})

(defn view-pos
  [idx {:keys [x y]}]
  (let [s (str "<" x ", " y ">")]
    [:div {:key idx} s]))

(defn rect [width height color]
  [:rect {:width width
          :height height
          :fill color}])

(defn circle [cx cy r fill]
  [:circle {:cx cx :cy cy :r r :fill fill}])

(defn text [t x y font-size text-anchor fill]
  [:text {:x x
          :y y
          :font-size font-size
          :text-anchor text-anchor
          :fill fill} t])

(defn polyline [point-list]
  [:polyline {:fill "none"
              :stroke "black"
              :points (->> point-list
                           (map (fn [{:keys [x y]}]
                                  (str x "," y)))
                           (clojure.string/join " "))}])
(def svg-side "300")

(defn svg-wrapper [& body]
  [:svg {:version "1.1"
         :baseProfile "full"
         :width svg-side
         :height svg-side
         :xmlns "http://www.w3.org/2000/svg"} body])

(defn svg-view [& body]
  [:svg {:width svg-side
         :height svg-side
         :viewBox (str "0 0 " svg-side " " svg-side)} body])

(defn extract-points [state]
  (cons (:pos state) (:history state)))

(defn get-max [points]
  (reduce (fn [{:keys [max-x max-y]} {:keys [x y]}]
            {:max-x (if (> x max-x) x max-x)
             :max-y (if (> y max-y) y max-y)})
          {:max-x 0 :max-y 0}
          points))

(defn make-point-scaler [scalar]
  (fn [p]
    (reduce (fn [result [k v]]
              (assoc result k (* scalar v)))
            {}
            p)))

(defn state->svg [state]
  (let [points (extract-points state)
        {:keys [max-x max-y]} (get-max points)
        dv (str (max max-x max-y))
        scaler (make-point-scaler (/ svg-side dv))]
    (svg-wrapper (svg-view (->> points
                                (map scaler)
                                polyline)))))

(defn show-history [state]
  (let [points (extract-points state)]
    [:div
     [:div "History"]
     (map-indexed view-pos points)]))

(defn state-debug []
  (let [{:keys [pos heading history step-size] :as state} @app-state]
    [:div
     [:h1 "System Debug"]
     [:div (label-str "Heading" heading)]
     [:div (label-str "Step Size" step-size)]
     (state->svg state)]))

(reagent/render-component [state-debug]
                          (. js/document (getElementById "app")))

(def moves-from-iteration
  (transform-n-times next-symbols 4 [:f]))

(def sample-moves moves-from-iteration)

(reset! app-state (apply-moves default-state sample-moves))

(defn on-js-reload []
  (reset! app-state (apply-moves default-state sample-moves))

  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
