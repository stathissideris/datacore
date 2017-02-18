(ns datacore.util)

(defn deep-merge
  "Recursively merges maps. If keys are not maps, the last value wins."
  [& vals]
  (cond (every? map? vals)
        (apply merge-with deep-merge vals)

        :else
        (last vals)))

(comment
  ;;incomplete diff impl

  (defn- bigger? [[x1 y1] [x2 y2]]
    (and (> x2 x1) (> y2 y1)))

  (defn- points [ia ibs]
    (mapv (partial vector ia) ibs))

  (defn- build-chains [current-chains verticals]
    (if-not verticals
      current-chains
      (let [make-chain (fn [old-chain]
                         (map (partial conj old-chain)
                              (filter (partial bigger? (last old-chain)) (first verticals))))
            new-chains (into current-chains (mapcat make-chain current-chains))]
        (build-chains new-chains (next verticals)))))

  (defn diff [a b]
    (let [eq-b (reduce (fn [m [idx member]]
                         (update m member (fnil conj []) (inc idx)))
                       {} (map-indexed vector b))
          p    (map-indexed (fn [idx x] (points (inc idx) (eq-b x))) a)]
      (prn p)
      (prn '----)
      (map rest (sort (build-chains #{[[0 0]]} p))))))

(defn longest [xs ys] (if (> (count xs) (count ys)) xs ys))

(def lcs
  (memoize
   (fn [[x & xs] [y & ys]]
     (cond
      (or (= x nil) (= y nil)) nil
      (= x y) (cons x (lcs xs ys))
      :else (longest (lcs (cons x xs) ys) (lcs xs (cons y ys)))))))

(defn- diffs [as bs common]
  (loop [[a & ra :as as]     as
         [b & rb :as bs]     bs
         [c & rc :as common] common
         diffs               []]
    ;;(prn 'a a '- 'b b '- 'c c)
    (if (and (not a) (not b))
      diffs
      (cond (= a b c)  (recur ra rb rc (conj diffs [:same a]))
            (and (not= a c) (not= b c)) (recur ra rb common (conj diffs [:edit a b]))
            (not= a c) (recur ra bs common (conj diffs [:delete a]))
            (not= b c) (recur as rb common (conj diffs [:add b]))))))

(defn diff [a b]
  (diffs a b (lcs a b)))

#_(pprint (diff [0 1 2 3 4 5 6 7 8] [1 2 3 :a :b :c 4 5 6]))
#_(pprint (diff [0 1 2 3 :e :f :g 4 5 6 7 8] [1 2 3 :a :b :c 4 5 6]))
#_(pprint (diff [0 1 2 3 :e :f :g 4 5 6 7 8] [1 2 3 :a :b :c 4 5 10 6]))
