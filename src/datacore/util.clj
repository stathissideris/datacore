(ns datacore.util
  (:refer-clojure :exclude [meta alter-meta! future])
  (:require [clojure.string :as str]
            [clojure.spec :as s]
            [clojure.core.match :refer [match]]
            [clojure.data :as data]
            [clojure.set :as set]
            [clojure.repl :as repl])
  (:import java.util.WeakHashMap))

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

(defn kw-str [kw]
  (some-> kw str (subs 1)))

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

(defn- flatten-keys* [a ks m]
  (if (map? m)
    (reduce into (map (fn [[k v]] (flatten-keys* a (conj ks k) v)) (seq m)))
    (assoc a ks m)))

(defn flatten-keys [m] (flatten-keys* {} [] m))

(defn time-in-millis [] (System/currentTimeMillis))

(defn truncate-string [s len]
  (if (> (count s) len)
    (str (subs s 0 (- len 3)) "...")
    s))

(defn index-of [x coll]
  (first
   (map first
        (filter #(= (second %) x)
                (map-indexed vector coll)))))

(defmacro with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  {:added "1.0"}
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(def uncaught-exception (atom nil))
(defn handle-uncaught [throwable]
  (reset! uncaught-exception throwable)
  (let [trace (with-err-str (repl/pst throwable 150))]
    (println "UNCAUGHT EXCEPTION" trace)))

(defmacro future [& body]
  `(clojure.core/future
     (try
       ~@body
       (catch Exception e#
         (handle-uncaught e#)))))

(defn resource->external-form [x]
  (if (str/starts-with? x "file:")
    x
    (.toExternalForm (.getResource Class x))))

;;;;;;;;;;;;;;;;;;;; meta ;;;;;;;;;;;;;;;;;;;;

(def ^:private meta-map (WeakHashMap.))
(defn meta [o]
  (.get meta-map o))

(defn add-meta! [o m]
  (.put meta-map o (merge (.get meta-map o) m)))

(defn alter-meta! [o fun & args]
  (locking meta-map
    (.put meta-map o (apply fun (or (meta o) {}) args))
    (meta o)))

;;;;;;;;;;;;;;;;;;;; diff ;;;;;;;;;;;;;;;;;;;;

;;;;; incomplete fast diff implementation ;;;;;

(comment

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

;;;;; simple diff ;;;;;

(defn- diff-item
  [a b]
  (cond
    (and (not (nil? a))
         (not (nil? b))) (if (= a b)
                           [:same b]
                           [:edit b])
    (and (nil? a)
         (not (nil? b))) [:insert b]

    (and (not (nil? a))
         (nil? b))       [:delete a]))

(defn simple-list-diff [a-list b-list]
  (for [idx (range (max (count a-list) (count b-list)))]
    (let [a (nth a-list idx nil)
          b (nth b-list idx nil)]
      (diff-item a b))))

;;;;; naive and slow diff ;;;;;

(defn longest [xs ys] (if (> (count xs) (count ys)) xs ys))

(defn memoize-with
  "Memoize a bit of code using your own cache atom and key."
  [cache-atom k fun]
  (if-let [e (find @cache-atom k)]
    (val e)
    (let [ret (fun)]
      (swap! cache-atom assoc k ret)
      ret)))

(defn- lcs [x y]
  (let [fix-nil (fn [x] (if (= ::nil x) nil x))
        x       (map (fnil identity ::nil) x)
        y       (map (fnil identity ::nil) y)
        cache   (atom {}) ;;local memoization, new cache for every call of lcs
        lcs*    (fn lcs* [[x & xs :as xx] [y & ys :as yy]]
                  (memoize-with
                   cache [xx yy]
                   #(cond
                      (or (= x nil) (= y nil)) nil
                      (= x y) (cons x (lcs* xs ys))
                      :else (longest (lcs* (cons x xs) ys) (lcs* xs (cons y ys))))))]
    (map fix-nil (lcs* x y))))

;;TODO replace this with a more performant implementation
(defn- seq-diffs [as bs common]
  (let [fix-nil (fn [x] (if (= ::nil x) nil x))]
    (loop [[a & ra :as as]     (map (fnil identity ::nil) as)
           [b & rb :as bs]     (map (fnil identity ::nil) bs)
           [c & rc :as common] (map (fnil identity ::nil) common)
           diffs               []]
      ;;(prn 'a a '- 'b b '- 'c c)
      (if (and (not a) (not b))
        diffs
        (cond (= a b c)  (recur ra rb rc (conj diffs [:same (fix-nil a)]))
              (and (not= a c) (not= b c)) (recur ra rb common (conj diffs [:edit (fix-nil a) (fix-nil b)]))
              (not= a c) (recur ra bs common (conj diffs [:delete (fix-nil a)]))
              (not= b c) (recur as rb common (conj diffs [:insert (fix-nil b)])))))))

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
                         (conj new-diff (map #(hash-map :type :delete
                                                        :index index
                                                        :value (last %)) diff))
                         index)
          :insert (recur (next diffs)
                         (conj new-diff (map #(hash-map :type :insert
                                                        :index index
                                                        :value (last %)) (reverse diff)))
                         (+ index (count diff)))
          :same   (recur (next diffs)
                         new-diff
                         (+ index (count diff)))
          :edit   (recur (next diffs)
                         (conj new-diff (map-indexed #(hash-map :type :edit
                                                                :index (+ index %1)
                                                                :old (second %2)
                                                                :value (last %2)) diff))
                         (+ index (count diff))))))))

(defprotocol TreeDiff
  (tree-diff [a b]))

(defn- prepend-path [prefix diff]
  (update diff :path #(vec (concat prefix %))))

(def ^:dynamic ^:private tree-diff-path [])

(defn- set-path [diff]
  (if (< (count tree-diff-path) (count (get diff 1)))
    diff
    (assoc diff :path tree-diff-path)))

(extend-protocol TreeDiff
  nil
  (tree-diff [a b]
    (if (nil? b)
      [{:type :same :path [] :value nil :struct :atom}]
      [{:type :edit :path [] :old a :value b :struct :atom}]))

  Object
  (tree-diff [a b]
    (if (= a b)
      [{:type :same :path [] :value a :struct :atom}]
      [{:type :edit :path [] :old a :value b :struct :atom}]))

  java.util.Map
  (tree-diff [a b]
    (cond
      (not (map? b))
      [(set-path {:type :edit :path [] :old a :value b :struct :map})]
      (= a b)
      [(set-path {:type :same :path [] :value a :struct :map})]
      :else
      (let [ka           (set (keys a))
            kb           (set (keys b))
            common-keys  (set/intersection ka kb)
            edited-keys  (filter #(not= (get a %) (get b %)) common-keys)
            deleted-keys (set/difference ka kb)
            added-keys   (set/difference kb ka)]
        (map #(if (or (nil? (:struct %)) (= :atom (:struct %)))
                (assoc % :struct :map)
                %)
             (apply concat
                    (for [k deleted-keys] {:type :dissoc :path [k] :value (get a k)})
                    (for [k added-keys] {:type :assoc :path [k] :value (get b k)})
                    (for [k edited-keys]
                      (map (partial prepend-path [k]) (tree-diff (get a k) (get b k)))))))))

  java.util.List
  (tree-diff [a b]
    (cond
      (not (instance? java.util.List b))
      [(set-path {:type :edit :path [] :old a :value b :struct :vector})]
      (or (identical? a b) (= a b))
      [(set-path {:type :same :path [] :value a :struct :vector})]
      :else
      (doall
       (mapcat
        (fn [{:keys [type path old value] :as diff}]
          (condp = type
                 :edit
                 (cond (and (map? old) (map? value))
                       (map (partial prepend-path path) (tree-diff old value))
                       (and (sequential? old) (sequential? value))
                       (map (partial prepend-path path) (tree-diff old value))
                       :else
                       [diff])
                 :insert [diff]
                 :delete [diff]))
        (map #(as-> % $
                (dissoc $ :index)
                (assoc $ :path [(:index %)])
                (if (or (nil? (:struct $)) (= :atom (:struct $))) (assoc $ :struct :vector) $))
             (seq-diff-indices a b)))))))

(s/def ::path (s/coll-of any?))
(s/def ::value (s/nilable any?))
(s/def ::old (s/nilable any?))
(s/def ::struct #{:atom :vector :map})
(s/def ::type #{:same :delete :insert :assoc :dissoc :edit})
(defn- vector-path? [path] (-> path last nat-int?))
(let [d (fn [type spec] (s/and spec #(= type (:type %))))]
  (s/def ::diff-item (s/or :same   (d :same (s/keys :req-un [::type ::path ::value ::struct]))
                           :delete (s/and (d :delete (s/keys :req-un [::type ::path ::value ::struct]))
                                          #(vector-path? (:path %)))
                           :insert (s/and (d :insert (s/keys :req-un [::type ::path ::value ::struct]))
                                          #(vector-path? (:path %)))
                           :assoc  (d :assoc (s/keys :req-un [::type ::path ::value ::struct]))
                           :dissoc (d :dissoc (s/keys :req-un [::type ::path ::value ::struct]))
                           :edit   (s/and (d :edit (s/keys :req-un [::type ::path ::old ::value ::struct]))
                                          #(not= (:old %) (:value %))))))
(declare patch)
(s/def ::tree-diff-input any?)
(s/fdef tree-diff
  :args (s/cat :a ::tree-diff-input, :b ::tree-diff-input)
  :ret  (s/coll-of ::diff-item)
  :fn   (fn [{:keys [args ret]}]
          (= (:b args) (patch (:a args) (map second ret))))) ;;second to remove tag

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

(defn- patch-diff-item [x {:keys [type path old value]}]
  (condp = type
    :same   x
    :delete (if (= 1 (count path))
              (vec-dissoc x (first path))
              (update-in x (butlast path) vec-dissoc (last path)))
    :insert (if (= 1 (count path))
              (vec-insert x (first path) value)
              (update-in x (butlast path) vec-insert (last path) value))
    :assoc  (assoc-in x path value)
    :dissoc (if (= 1 (count path))
              (dissoc x (first path))
              (update-in x (butlast path) dissoc (last path)))
    :edit   (if (empty? path) value (assoc-in x path value))))

(defn patch [x diff]
  (reduce patch-diff-item x diff))
(s/fdef patch
  :args (s/cat :x any? :diff (s/coll-of ::diff-item)))
