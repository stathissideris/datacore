(ns datacore.cells
  (:refer-clojure :exclude [swap!])
  (:require [clojure.data.priority-map :as pm]))

(def ^:private id (ref 0))
(def ^:private cells (ref {})) ;;map of cell IDs to cell values
(def ^:private links (ref {})) ;;map of cell IDs to sets of sinks

(defn link! [source sink]
  (dosync
   (alter links update source
          (fn [x] (if-not x #{sink} (conj x sink))))))

(def ^:private ^:dynamic *cell-context* nil)

(declare set-value!)
(defn- value [cell]
  (when *cell-context*
    (link! cell *cell-context*))
  (if (.-formula cell)
    (binding [*cell-context* cell]
      (let [{:keys [cache thunk]} (get @cells cell)]
        (or cache (set-value! cell (thunk)))))
    (get @cells cell)))

(defn- thunk [cell]
  (let [t (:thunk (get @cells cell))]
    (when-not t
      (throw (ex-info "Cell is not a formula" {:cell cell})))
    t))

(deftype CellID [id formula]
  clojure.lang.IRef
  (deref [this] (value this)))

(defmethod print-method CellID
  [x w]
  (.write w (pr-str {:id (.-id x)
                     :formula? (.-formula x)})))

(defn cell? [x] (instance? CellID x))
(defn formula? [x] (and (cell? x) (.-formula x)))
(defn input? [x] (and (cell? x) (not (.-formula x))))

(defn- set-value! [cell x]
  (dosync
   (if (formula? cell)
     (alter cells assoc-in [cell :cache] x)
     (alter cells assoc cell x))
   x))

(defn- register-cell! [x formula?]
  (dosync
   (let [current-id @id
         cell       (CellID. current-id formula?)]
     (alter id inc)
     (if formula?
       (alter cells assoc cell {:thunk x})
       (alter cells assoc cell x))
     cell)))

(defn cell [x]
  (register-cell! x false))

(defn formula [fun]
  (register-cell! fun true))

(defmacro cell= [& code]
  `(formula (fn [] ~@code)))

(defn- cells-into-pm [pm cells]
  (reduce (fn [pm cell]
            (assoc pm cell (.-id cell)))
          pm cells))

(defn- propagate [pri-map]
  (when-let [next (first (peek pri-map))]
    (let [popq  (pop pri-map)
          old   @next
          new   ((thunk next))
          diff? (not= new old)]
      (when diff? (set-value! next new))
      (recur (if-not diff?
               popq ;;continue to next thing in priority map
               (cells-into-pm popq (get @links next))))))) ;;add all sinks of cell to priority map before continuing

(defn swap! [cell fun & args]
  (if (.-formula cell)
    (throw (ex-info "Cannot swap, cell is a formula" {:cell cell}))
    (dosync
     (let [current   @cell
           new-value (apply fun current args)]
       (when-not (= current new-value)
         (alter cells assoc cell new-value)
         (propagate (cells-into-pm (pm/priority-map) (get @links cell))))
       new-value))))

(comment
  (def foo (cell 100))
  (def bar (cell 2))
  (def baz (cell= (prn "calc baz!" (* 2 @foo @bar))
                  (* 2 @foo @bar)))
  ;;or
  (def baz (formula #(do
                       (prn "calc baz!" (* 2 @foo @bar))
                       (* 2 @foo @bar))))
  @baz

  (swap! foo inc)
  (swap! bar inc)
  )
