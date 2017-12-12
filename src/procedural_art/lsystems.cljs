(ns procedural-art.lsystems)

(defn add-location-to-history
  [{:keys [pos history] :as state}]
  (update state :history #(conj % pos)))

(defn turn [state angle]
  (update state :heading #(+ angle %)))

(defn to-radians [degrees]
  (* degrees (/ Math/PI 180)))

(defn forward
  [params {:keys [heading step-size] :as state}]
  (let [dx (* step-size (Math/cos heading))
        dy (* step-size (Math/sin heading))]
    (-> state
        add-location-to-history
        (update-in [:pos :x] #(+ % dx))
        (update-in [:pos :y] #(+ % dy)))))

(def default-state {:pos {:x 0 :y 0}
                    :heading 0
                    :step-size 1
                    :history nil})

(def koch-curve
  {:id "koch-curve"
   :logic {:start [:f]
           :rules {:f [:f :+ :f :- :f :- :f :+ :f]
                   :+ [:+]
                   :- [:-]}}
   :render {:handlers {:f forward
                       :+ (fn [{:keys [angle]} state] (turn state (to-radians angle)))
                       :- (fn [{:keys [angle]} state] (turn state (to-radians (- angle))))}
            :state default-state
            :params {:angle 90
                     :iterations 2}}})

(def sierpinski-arrowhead
  {:id "sierpinski-arrowhead"
   :logic {:start [:a]
           :rules {:a [:b :- :a :- :b]
                   :b [:a :+ :b :+ :a]
                   :- [:-]
                   :+ [:+]}}
   :render {:handlers {:a forward
                       :b forward
                       :+ (fn [state] (turn state (to-radians 60)))
                       :- (fn [state] (turn state (to-radians -60)))}
            :state default-state
            :params {:angle 90
                     :iterations 2}}})

(def dragon-curve
  {:start [:f :x]
   :rules {:x [:x :+ :y :f :+]
           :y [:- :f :x :- :y]}
   :handlers {:f forward
              :+ (fn [state] (turn state (to-radians 90)))
              :- (fn [state] (turn state (to-radians -90)))}})

(def all-systems
  [koch-curve
   sierpinski-arrowhead])
