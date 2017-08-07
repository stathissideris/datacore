(ns dev
  (:refer-clojure :exclude [method-sig])
  (:require [clojure.tools.namespace.repl :as repl :refer (clear refresh-all)]
            [datacore.util :as util]
            [clojure.spec.test :as stest]
            [datacore.main]))

(defn refresh []
  (repl/refresh))

(defn instrument-all []
  (doall
   (sort
    (for [s (stest/instrumentable-syms)]
      (stest/instrument s)))))

#_(defn run-all-my-tests []
    (reset)
    (test/run-all-tests #"datacore.*test$"))

#_(defn run-selected-tests [& namespaces]
    (reset)
    (apply test/run-tests namespaces))

(defn superclasses [clazz]
  (.getSuperclass clazz))

(defn all-declared-methods [clazz]
  (->> clazz .getDeclaredMethods seq (sort-by #(.getName %))))

(defn uniquify [coll]
  (let [non-unique? (->> coll frequencies (filter #(-> % val (> 1))) keys set)]
    (:coll
     (reduce (fn [{:keys [coll counts] :as m} x]
               (if (non-unique? x)
                 (-> m
                     (update :coll conj (str x (get counts x 1)))
                     (update-in [:counts x] (fnil inc 1)))
                 (update m :coll conj x)))
             {:coll   []
              :counts {}} coll))))

(defn method-sig [method]
  (if (.isVarArgs method)
    ['this 'var-args]
    (let [params
          (into
           ['this]
           (->> method
                .getParameterTypes
                seq
                (map (fn [p] (symbol (util/camel->kebab (.getSimpleName p)))))))]
      (->> params (map str) uniquify (map symbol) vec))))

(defn add-hints [params method]
  (vec
   (cons
    (first params)
    (map (fn [p tp]
           (if (or (= Object tp) (.isArray tp))
             p
            `(~'with-meta ~p {:tag ~(symbol (.getName tp))})))
         (rest params)
         (seq (.getParameterTypes method))))))

(defn dummy-methods [clazz]
  (->> clazz
       all-declared-methods
       (map
        (fn [method]
          (let [method-name (symbol (.getName method))]
            `(~method-name ~(method-sig method)
              (~'prn ~'CALLED (quote ~method-name) ~(method-sig method))))))))

(defn delegate-methods [clazz]
  (->> clazz
       all-declared-methods
       (map
        (fn [method]
          (let [method-name (symbol (.getName method))]
            `(~method-name ~(method-sig method)
              (~(symbol (str "." method-name)) ~@(method-sig method))))))))

(defn delegate-deref-methods [clazz]
  (->> clazz
       all-declared-methods
       (map
        (fn [method]
          (let [method-name (symbol (.getName method))]
            `(~method-name ~(method-sig method)
              (~(symbol (str "." method-name)) ~@(cons `(deref ~(first (method-sig method)))
                                                       (rest (method-sig method))))))))))

(defn reify-template [classes]
  (let [[[clazz _]] classes]
    `(~'reify ~(symbol (.getSimpleName clazz))
      ~@(apply concat
         (for [[clazz generate] classes]
           (cons
            `(~'comment ~clazz ~generate)
            (condp = generate
              :dummy (dummy-methods clazz)
              :delegate (delegate-methods clazz)
              :delegate-deref (delegate-deref-methods clazz))))))))

(def ^:dynamic trace-proxy-depth -2)

(defn pad [n]
  (apply str (repeat n " ")))

(defn- trace-proxy-fn [fun]
  (let [[name args & body] fun]
    `(~name ~args
      (binding [trace-proxy-depth (+ 2 trace-proxy-depth)]
        (println (str (dev/pad trace-proxy-depth) "CALL: " (pr-str (list '~name ~@args))))
        (let [ret# (do ~@body)]
          (println (str (dev/pad trace-proxy-depth) "=> " (pr-str ret#)))
          ret#)))))

(defmacro trace-proxy
  "Does not work for proxies containing multi-arity functions"
  [code]
  (let [[op classes args & fns] code]
    `(~op ~classes ~args
      ~@(map trace-proxy-fn fns))))

#_(pprint (dev/reify-template [[ObservableList :dummy]
                               [javafx.beans.Observable :dummy]
                               [java.util.List :delegate-deref]
                               [java.util.Collection :delegate-deref]
                               [java.lang.Iterable :delegate-deref]]))

(defn start []
  (datacore.main/init))
