(ns datacore.util)

(defn deep-merge
  "Recursively merges maps. If keys are not maps, the last value wins."
  [& vals]
  (cond (every? map? vals)
        (apply merge-with deep-merge vals)

        :else
        (last vals)))
