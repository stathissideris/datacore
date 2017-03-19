(ns datacore.reflection)

(defn- provided
  [cond fun x]
  (if cond
    (fun x)
    x))

(defn- append
  [xs x]
  (conj (vec xs) x))

(defn- arity-of-method
  [method]
  (->> method .getParameterTypes alength))

(defn arities
  [fun]
  (let [all-declared-methods    (.getDeclaredMethods (class fun))
        methods-named           (fn [name]
                                  (filter #(= (.getName %) name) all-declared-methods))
        methods-named-invoke    (methods-named "invoke")
        methods-named-do-invoke (methods-named "doInvoke")
        is-rest-fn              (seq methods-named-do-invoke)]
    (->> methods-named-invoke
         (map arity-of-method)
         sort
         (provided is-rest-fn
                   (fn [v] (append v :rest))))))
