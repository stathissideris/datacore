(ns datacore.util
  (:require [clojure.string :as str]))

(defn camel->kebab [from]
  (let [s (str/split (name from) #"(?=[A-Z])" )]
    (apply str (interpose "-" (map str/lower-case s)))))

(defn kebab->camel [from]
  (let [s (str/split (name from) #"\-")]
    (apply str (first s) (map str/capitalize (next s)))))

(defn capitalize-first
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1))
           (subs s 1)))))

(defn deep-merge
  "Recursively merges maps. If keys are not maps, the last value wins."
  [& vals]
  (cond (every? map? vals)
        (apply merge-with deep-merge vals)

        :else
        (last vals)))

(defn take-exactly
  "Will always return n number of items, and if coll is shorter will
  compensate with repeated missing items."
  [n coll missing]
  (concat (take n coll)
          (repeat (- n (count coll)) missing)))

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

(defn memoize-with
  "Memoize a bit of code using your own cache atom and key."
  [cache-atom k fun]
  (if-let [e (find @cache-atom k)]
    (val e)
    (let [ret (fun)]
      (swap! cache-atom assoc k ret)
      ret)))

(defn lcs [x y]
  (let [cache (atom {}) ;;local memoization, new cache for every call of lcs
        lcs*  (fn lcs* [[x & xs :as xx] [y & ys :as yy]]
                (memoize-with
                 cache [xx yy]
                 #(cond
                    (or (= x nil) (= y nil)) nil
                    (= x y) (cons x (lcs* xs ys))
                    :else (longest (lcs* (cons x xs) ys) (lcs* xs (cons y ys))))))]
    (lcs* x y)))

(defn- seq-diffs [as bs common]
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

(defn seq-diff [a b]
  (seq-diffs a b (lcs a b)))

#_(pprint (diff [0 1 2 3 4 5 6 7 8] [1 2 3 :a :b :c 4 5 6]))
#_(pprint (diff [0 1 2 3 :e :f :g 4 5 6 7 8] [1 2 3 :a :b :c 4 5 6]))
#_(pprint (diff [0 1 2 3 :e :f :g 4 5 6 7 8] [1 2 3 :a :b :c 4 5 10 6]))
