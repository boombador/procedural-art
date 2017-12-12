(ns procedural-art.view
  (:require [procedural-art.svg :as s]
            [cljsjs.react-bootstrap]
            [reagent.core :as reagent :refer [atom]]))

(def svg-side "300")

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

(defn make-point-scaler [scalar]
  (fn [p]
    (reduce (fn [result [k v]]
              (assoc result k (* scalar v)))
            {}
            p)))


(defn navbar
  [title source]
  [:nav.navbar.navbar-inverse.navbar-fixed-top
   [:div.container
    [:div.navbar-header
     [:a.navbar-brand {:href "/"} title]]
    [:div#navbar.collapse.navbar-collapse
     [:ul.nav.navbar-nav
      [:li [:a {:href source} "Source"]]]]]])

(defn explainer
  [source]
  [:div.explainer
   [:p "An L-System uses a starting string and a collection of rules for iteratively replacing characters with new strings. As the process repeats complex fractal patterns can emerge. This project takes a basic string rewriting system and combines it with a turtle-based rendering system to generate SVG representations of the systems."]
   [:p "This page allows you to watch the effect of tweaking the parameters that convert the iterated string into an SVG representation of lines. There is lots of good information on "
    [:a {:href "https://en.wikipedia.org/wiki/L-system"} "Wikipedia"]
    "."]
   [:p "Visit the "
    [:a {:href source} "source code"]
    " of this project for more precise information, updates may be forthcoming."]])

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



(defn slider [label value min max handler]
  [:label {:style {:display "block"}}
   (str label ": " value)
   [:input {:type "range" :value value :min min :max max
            :style {:width "100%"}
            :on-change handler}]])

(defn change-value
  [evt]
  (.-target.value evt))

(defn to-int
  [str-int]
  (js/parseInt str-int))

(defn parameter-controls
  [{:keys [angle iterations l-system]} app-state]
  [:div {:class "controls"}
   (slider "Angle" angle 0 90 (fn [e]
                        (let [val (.-target.value e)]
                          (swap! app-state assoc-in [:system-parameters :angle] val))))
   (slider "Iterations" iterations 1 4 (fn [e]
                            (swap! app-state
                                   assoc-in
                                   [:system-parameters :iterations]
                                   (-> e change-value to-int))))])

(def Button (reagent/adapt-react-class (aget js/ReactBootstrap "Button")))

(defn interactive-l-system
  [app-state params rendered]
  (let [title "L-System Generator" 
        source "https://github.com/boombador/procedural-art"]
    [:div
     (navbar title source)
     [:div.container {:style {:margin-top "60px"}}
      [:div.row [:div.col-md-12 [:h1 "Interactive L-System"]]]
      [:div.row 
       [:div.col-md-4 (parameter-controls params app-state)]
       [:div.col-md-8 {:style {:text-align "center"}} (state->svg rendered)]]
      (explainer source)]]))
