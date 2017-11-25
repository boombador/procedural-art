(ns procedural-art.core
    (:require [reagent.core :as reagent :refer [atom]]
              [procedural-art.lsystems :as l]
              [procedural-art.svg :as s]
              [procedural-art.view :as v]))

; parts: lsystem
;; logic for iterating sequence
;; logic for applying rules to 
; views - components: history, svg-scene,
; svg - svg, circle, rect, polyline

(enable-console-print!)

(defonce app-state (atom nil))

(defn label-str
  [label s]
  (str label ": " s "\n"))

(defn add-location-to-history
  [{:keys [pos history] :as state}]
  (update state :history #(conj % pos)))

(defn turn [state angle]
  (update state :heading #(+ angle %)))

(defn to-radians [degrees]
  (* degrees (/ Math/PI 180)))

(defn forward
  [{:keys [heading step-size] :as state}]
  (let [dx (* step-size (Math/cos heading))
        dy (* step-size (Math/sin heading))]
    (-> state
        add-location-to-history
        (update-in [:pos :x] #(+ % dx))
        (update-in [:pos :y] #(+ % dy)))))

(defn left [state] (turn state (to-radians 90)))
(defn right [state] (turn state (to-radians -90)))

(def koch-curve
  {:start [:f]
   :rules {:f [:f :+ :f :- :f :- :f :+ :f]
           :+ [:+]
           :- [:-]}
   :handlers {:f forward
              :+ (fn [state] (turn state (to-radians 90)))
              :- (fn [state] (turn state (to-radians -90)))}})
              :+ (fn [{:keys [angle] :as state}] (turn state (to-radians angle)))

(def sierpinski-arrowhead
  {:start [:a]
   :rules {:a [:b :- :a :- :b]
           :b [:a :+ :b :+ :a]
           :- [:-]
           :+ [:+]}
   :handlers {:a forward
              :b forward
              :+ (fn [state] (turn state (to-radians 60)))
              :- (fn [state] (turn state (to-radians -60)))}})

; dragon-curve not working
(def dragon-curve
  {:start [:f :x]
   :rules {:x [:x :+ :y :f :+]
           :y [:- :f :x :- :y]}
   :handlers {:f forward
              :+ (fn [state] (turn state (to-radians 90)))
              :- (fn [state] (turn state (to-radians -90)))}})

(defn make-symbols-iterator
  "use the l-system rules to create a function to iterate symbols"
  [rules]
  (fn [symbols]
    (mapcat #(rules %) symbols)))

(defn apply-move-to-state
  [state handlers move]
  (let [action (move handlers)]
    (action state)))

(defn apply-moves
  [start-state handlers moves]
  (reduce (fn [state move]
            (apply-move-to-state state handlers move))
          start-state
          moves))

(defn evt-set-angle
  [e]
  (let [val (.. e -target -value)]
    (swap! app-state assoc :angle val)))

(defn page-l-system []
  (let [state @app-state]
    [:div
     (state->svg state)]))
     [:div.controls

(reagent/render-component [page-l-system]
                          (. js/document (getElementById "app")))

(def default-state
  {:pos {:x 0 :y 0}
   :heading 0
   :step-size 1
   :history nil})

(defn generate-system
  "take an l system and number of iterations, return a state"
  [{:keys [rules start handlers]} iterations]
  (let [next-symbols (make-symbols-iterator rules)
        generations (iterate next-symbols start)
        moves (nth generations iterations)]
    (apply-moves default-state handlers moves)))

(def test-gen (generate-system koch-curve 4))

(reset! app-state test-gen)

(defn on-js-reload []
  (reset! app-state test-gen))
