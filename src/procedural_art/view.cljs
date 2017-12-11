(ns procedural-art.view
  (:require [procedural-art.svg :as s]))

(def svg-side "300")

(defn label-str
  [label s]
  (str label ": " s "\n"))

(defn show-debug [{:keys [heading step-size] :as state}]
  [:div 
   [:h1 "System Debug"]
   [:div (label-str "Heading" heading)]
   [:div (label-str "Step Size" step-size)]
   (show-history state)])

(defn view-pos
  [idx {:keys [x y]}]
  (let [s (str "<" x ", " y ">")]
    [:div {:key idx} s]))

(defn extract-points [state]
  (cons (:pos state) (:history state)))

(defn get-max [points]
  (reduce (fn [{:keys [max-x max-y]} {:keys [x y]}]
            {:max-x (if (> x max-x) x max-x)
             :max-y (if (> y max-y) y max-y)})
          {:max-x 0 :max-y 0}
          points))

(defn show-history [state]
  (let [points (extract-points state)]
    [:div
     [:div "History"]
     (map-indexed view-pos points)]))

(defn make-point-scaler [scalar]
  (fn [p]
    (reduce (fn [result [k v]]
              (assoc result k (* scalar v)))
            {}
            p)))

(defn state->svg [state]
  (let [points (extract-points state)
        {:keys [max-x max-y]} (get-max points)
        max-coord (max max-x max-y 1)
        scaler (make-point-scaler (/ (js/parseInt svg-side) max-coord))]
    (s/svg-wrapper {:side svg-side}
                   (s/svg-view {:side svg-side}
                               (->> points
                                    (map scaler)
                                    s/polyline)))))

(defn slider [value min max handler]
  [:input {:type "range" :value value :min min :max max
           :style {:width "100%"}
           :on-change handler}])

(defn parameter-controls
  [{:keys [angle iterations l-system]} app-state]
  [:div {:style {:width "50%"
                 :float "left"}}
   (slider angle 0 90 (fn [e]
                        (let [val (.-target.value e)]
                          (swap! app-state assoc-in [:system-parameters :angle] val))))
   (slider iterations 1 4 (fn [e]
                            (let [val (.-target.value e)
                                  num (js/parseInt val)]
                              (swap! app-state assoc-in [:system-parameters :iterations] num))))])
