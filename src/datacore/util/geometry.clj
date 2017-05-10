(ns datacore.util.geometry)

(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))

(defn point-distance [[x1 y1] [x2 y2]]
  (sqrt (+ (abs (- x1 x2))
           (abs (- y1 y2)))))

(defn bbox-center [{:keys [min-x min-y
                           width height]}]
  [(+ min-x (/ width 2))
   (+ min-y (/ height 2))])

(defn bbox-distance [bbox1 bbox2]
  (point-distance (bbox-center bbox1)
                  (bbox-center bbox2)))

(defn extend-bbox [{:keys [min-x min-y min-z
                           max-x max-y max-z
                           width height depth] :as bbox} padding]
  (merge bbox
   {:min-x (- min-x padding)
    :max-x (+ max-x padding)
    :width (+ width (* 2 padding))
    :max-y (+ max-y padding)
    :min-y (- min-y padding)
    :height (+ height (* 2 padding))
    :min-z (- min-z padding)
    :max-z (+ max-z padding)
    :depth (+ depth (* 2 padding))}))
