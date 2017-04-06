(ns datacore.util
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [clojure.core.match :refer [match]]
            [clojure.data :as data]
            [clojure.set :as set]))

(defn camel->kebab [from]
  (let [s (str/split (name from) #"(?=[A-Z])" )]
    (apply str (interpose "-" (map str/lower-case s)))))

(defn kebab->camel [from]
  (let [s (str/split (name from) #"\-")]
    (apply str (first s) (map str/capitalize (next s)))))

(defn capitalize-first
  "Capitalizes first letter and leaves the rest of the string unchanged"
  [^CharSequence s]
  (let [s (.toString s)]
    (if (< (count s) 2)
      (.toUpperCase s)
      (str (.toUpperCase (subs s 0 1))
           (subs s 1)))))

(defn string->data-key [s]
  (-> s
      str/lower-case
      (str/replace #"[\(\)\[\]:,{}&^%$#@\.]" "")
      (str/replace #"[/]" " ")
      (str/replace " " "-")
      (str/replace "_" "-")
      keyword))

(defn data-key->label [k]
  (as-> k $
      (name $)
      (str/replace $ "-" " ")
      (str/split $ #" ")
      (map str/capitalize $)
      (str/join " " $)))

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

(defn time-in-millis [] (System/currentTimeMillis))

(defn truncate-string [s len]
  (if (> (count s) len)
    (str (subs s 0 (- len 3)) "...")
    s))

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
            (not= b c) (recur as rb common (conj diffs [:insert b]))))))

(defn seq-diff [a b]
  (seq-diffs a b (lcs a b)))

(defn seq-diff-indices
  "Perform a diff on sequences a and b and output a sequence of edits
  which when applied to sequence a would result in sequence b."
  [a b]
  (loop [diffs    (partition-by first (seq-diff a b))
         new-diff []
         index    0]
    (let [diff (first diffs)]
      (if-not diff
        (vec (apply concat new-diff))
        (condp = (ffirst diff)
          :delete (recur (next diffs)
                         (conj new-diff (map #(vector :delete index (last %)) diff))
                         index)
          :insert (recur (next diffs)
                         (conj new-diff (map #(vector :insert index (last %)) (reverse diff)))
                         (+ index (count diff)))
          :same   (recur (next diffs)
                         new-diff
                         (+ index (count diff)))
          :edit   (recur (next diffs)
                         (conj new-diff (map-indexed #(vector :edit (+ index %1) (second %2) (last %2)) diff))
                         (+ index (count diff))))))))
(s/fdef seq-diff-indices
  :args (s/cat :a (s/coll-of any?) :b (s/coll-of any?))
  :ret  (s/or :delete (s/cat :type #{:delete} :index nat-int? :item any?)
              :insert (s/cat :type #{:insert} :index nat-int? :item any?)
              :edit   (s/cat :type #{:edit} :index nat-int? :old-item any? :new-item any?)))

(defprotocol TreeDiff
  (tree-diff [a b]))

(defn- update-path [diff fun & args]
  (update diff 1 #(apply fun % args)))

(defn- path-conj [diff item]
  (update diff 1 conj item))

(defn- prepend-path [prefix diff]
  (update diff 1 #(vec (concat prefix %))))

(def ^:dynamic ^:private tree-diff-path [])

(defn- set-path [diff]
  (if (< (count tree-diff-path) (count (get diff 1)))
    diff
    (assoc diff 1 tree-diff-path)))

(defmacro with-path [path & code]
  `(binding [~'tree-diff-path ~path]
     ~@code))

(extend-protocol TreeDiff
  nil
  (tree-diff [a b]
    [[:edit [] a b]])

  Object
  (tree-diff [a b]
    [(if (= a b)
       [:same [] a]
       [:edit [] a b])])

  java.util.Map
  (tree-diff [a b]
    ;;(prn 'map 'tree-diff-path tree-diff-path)
    (cond
      (not (map? b))
      [(set-path [:edit [] a b])]
      (= a b)
      [(set-path [:same [] a])]
      :else
      (let [ka           (set (keys a))
            kb           (set (keys b))
            common-keys  (set/intersection ka kb)
            edited-keys  (filter #(not= (get a %) (get b %)) common-keys)
            deleted-keys (set/difference ka kb)
            added-keys   (set/difference kb ka)]
        (apply concat
               (map #(vector :dissoc [%] (get a %)) deleted-keys)
               (map #(vector :assoc [%] (get b %)) added-keys)
               (map #(map (partial prepend-path [%]) (tree-diff (get a %) (get b %))) edited-keys)))))

  java.util.List
  (tree-diff [a b]
    ;;(prn 'list 'tree-diff-path tree-diff-path)
    (cond
      (not (instance? java.util.List b))
      [(set-path [:edit [] a b])]
      (= a b)
      [(set-path [:same [] a])]
      :else
      (doall
       (mapcat
        (fn [diff]
          (match [diff]
                 [[:edit path old new]]
                 (cond (and (map? old) (map? new))
                       (map (partial prepend-path path) (tree-diff old new))
                       (and (sequential? old) (sequential? new))
                       (map (partial prepend-path path) (tree-diff old new))
                       :else
                       [diff])
                 [[:insert _ _]] [diff]
                 [[:delete _ _]] [diff]))
        (map #(update-path % vector) (seq-diff-indices a b)))))))

(s/def ::tree-path (s/coll-of any?))
(s/def ::diff-item (s/or :same   (s/cat :type #{:same}   :path ::tree-path :item any?)
                         :delete (s/cat :type #{:delete} :path ::tree-path :item any?)
                         :insert (s/cat :type #{:insert} :path ::tree-path :item any?)
                         :assoc  (s/cat :type #{:assoc}  :path ::tree-path :item any?)
                         :dissoc (s/cat :type #{:dissoc} :path ::tree-path :item any?)
                         :edit   (s/cat :type #{:edit}   :path ::tree-path :old any? :new any?)))
(declare patch)
(s/def ::tree-diff-input any?)
(s/fdef tree-diff
  :args (s/cat :a ::tree-diff-input, :b ::tree-diff-input)
  :ret  (s/coll-of ::diff-item)
  :fn   (fn [{:keys [args ret]}]
          (= (:b args) (patch (:a args) (mapv #(vec (s/unform ::diff-item %)) ret)))))

(defn- vec-dissoc [v idx]
  (vec
   (concat
    (subvec v 0 idx)
    (subvec v (inc idx) (count v)))))

(defn- vec-insert [v idx value]
  (vec
   (concat
    (subvec v 0 idx)
    [value]
    (subvec v idx (count v)))))

(defn- patch-diff-item [x diff-item]
  (match [diff-item]
    [[:same _ _]]      x
    [[:delete path v]] (if (= 1 (count path))
                         (vec-dissoc x (first path))
                         (update-in x (butlast path) vec-dissoc (last path)))
    [[:insert path v]] (if (= 1 (count path))
                         (vec-insert x (first path) v)
                         (update-in x (butlast path) vec-insert (last path) v))
    [[:assoc path v]]  (assoc-in x path v)
    [[:dissoc path v]] (if (= 1 (count path))
                         (dissoc x (first path))
                         (update-in x (butlast path) dissoc (last path)))
    [[:edit path _ v]] (if (empty? path) v (assoc-in x path v))))

(defn patch [x diff]
  (reduce patch-diff-item x diff))
(s/fdef patch
  :args (s/cat :x any? :diff (s/coll-of ::diff-item)))
