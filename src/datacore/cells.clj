(ns datacore.cells
  (:refer-clojure :exclude [swap! reset!])
  (:require [clojure.data.priority-map :as pm]
            [clojure.set :as set]))

(def ^:private id (ref 0))
(def ^:private cells (ref {})) ;;map of cell IDs to cell values
(def ^:private links (ref {})) ;;map of cell IDs to sets of sinks

(def ^:dynamic *detect-cycles* true)

(declare set-value! formula?)

(defn- cycles? [links cell]
  (loop [sinks   (get links cell)
         visited #{cell}]
    (when (seq sinks)
      (if (seq (set/intersection sinks visited))
        true
        (recur (set (mapcat links sinks)) (into visited sinks))))))

(defn- add-link [links source sink]
  (update links source
          (fn [x] (if-not x #{sink} (conj x sink)))))

(defn link! [source sink]
  (when-not (formula? sink)
    (throw (ex-info "Cannot add link, sink is not a formula"
                    {:source source :sink sink})))
  (dosync
   (when-not (get-in @links [source sink])
     (if (and *detect-cycles* (cycles? (add-link @links source sink) source))
       (throw (ex-info "Cannot add link, cycle detected"
                       {:source source :sink sink}))
       (alter links add-link source sink)))))

(def ^:private ^:dynamic *cell-context* nil)

(defn- value [cell]
  (when *cell-context*
    (link! cell *cell-context*))
  (if (.-formula cell)
    (binding [*cell-context* cell]
      (let [{:keys [cache thunk]} (get @cells cell)]
        (or cache
            (let [new-value (thunk)]
              (dosync (set-value! cell new-value))))))
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
  (if (formula? cell)
    (alter cells assoc-in [cell :cache] x)
    (alter cells assoc cell x))
  x)

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
  (let [cell (register-cell! fun true)]
    @cell ;;to initialize cache and establish links
    cell))

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
         (set-value! cell new-value)
         (propagate (cells-into-pm (pm/priority-map) (get @links cell))))
       new-value))))

(defn reset! [cell value]
  (swap! cell (fn [& _] value)))

(comment
  (def foo (cell 100))
  (def bar (cell 2))
  (def baz (cell= (prn "calc baz!" (* 2 @foo @bar))
                  (* 2 @foo @bar)))
  (def boo (cell= (prn "calc boo!" (* 2 @foo))
                  (* 2 @foo)))
  ;;or
  (def baz (formula #(do
                       (prn "calc baz!" (* 2 @foo @bar))
                       (* 2 @foo @bar))))
  @baz

  (swap! foo inc)
  (swap! bar inc)
  )
