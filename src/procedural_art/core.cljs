(ns procedural-art.core
    (:require [reagent.core :as reagent :refer [atom]]
              [procedural-art.view :as v]
              [procedural-art.lsystems :as l]))

(enable-console-print!)

(defn symbol-folder
  [handlers params]
  (fn [state move-symbol]
    (let [handler (get handlers move-symbol)]
      (handler params state))))

(defn iterate-by-rules
  [rules start iterations]
  (nth (iterate (fn [symbols]
                  (mapcat #(rules %) symbols))
                start)
       iterations))

(defn render-l-system
  [params system]
  (let [{{:keys [handlers state]} :render
         {:keys [start rules]} :logic} system
        reducing-fn (symbol-folder handlers params)
        moves (iterate-by-rules rules start (:iterations params))]
    (reduce reducing-fn state moves)))

(defn get-l-system
  [l-system-id]
  (nth (filter #(= l-system-id (:id %)) l/all-systems) 0))

(defn load-l-system!
  [l-system-id]
  (swap! app-state
         (fn [state system]
           (-> state
               (assoc :active-system-id l-system-id)
               (assoc :system-parameters (-> system :render :params))))
         (get-l-system l-system-id)))

(defonce app-state (atom {:active-system-id "koch-curve"
                          :system-parameters {:iterations 2 :angle 90}}))

(defn page-l-system []
  (let [{params :system-parameters id :active-system-id} @app-state
        sys (get-l-system id)
        rendered (render-l-system params sys)]
    (v/interactive-l-system app-state params rendered)))

(reagent/render-component [page-l-system]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
;  (load-l-system! "koch-curve")
  )
