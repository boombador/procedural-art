(ns procedural-art.svg)

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
  (if (> 2 (count point-list))
    nil
    [:polyline {:fill "none"
                :stroke "black"
                :points (->> point-list
                             (map (fn [{:keys [x y]}]
                                    (str x "," y)))
                             (clojure.string/join " "))}]))

(defn svg-wrapper [{:keys [side]} body]
  [:svg {:version "1.1"
         :baseProfile "full"
         :width side
         :height side
         :xmlns "http://www.w3.org/2000/svg"}
   body])

(defn svg-view [{:keys [side]} body]
  [:svg {:width side
         :height side
         :viewBox (str "0 0 " side " " side)}
   body])
