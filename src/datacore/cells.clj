(ns datacore.cells
  (:refer-clojure :exclude [swap! reset!])
  (:require [clojure.data.priority-map :as pm]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table]]
            [clojure.walk :as walk]))

(def ^:private id (ref 0))
(def ^:private cells (ref {})) ;;map of cell IDs to cell values
(def ^:private sinks (ref {})) ;;map of cell IDs to sets of sinks
(def ^:private sources (ref {})) ;;map of cell IDs to sets of sources

(def ^:dynamic *detect-cycles* true)

(declare set-value! set-error! touch! formula?)

(defn cell->debug [c]
  (let [v (get @cells c)]
    {:id       (.-id c)
     :label    (.-label c)
     :formula? (.-formula c)
     ;;:enabled? (:enabled? v)
     :value    (if (.-formula c)
                 (:cache v)
                 v)
     :error    (:error v)
     :code     (not-empty
                (apply
                 list
                 (walk/postwalk
                  (fn [x]
                    (if (and (symbol? x)
                             (= "clojure.core" (namespace x)))
                      (-> x name symbol)
                      x))
                  (:code v))))
     :sinks    (not-empty (set (map #(.-id %) (get @sinks c))))
     :sources  (not-empty (set (map #(.-id %) (get @sources c))))}))

(defn all-cells []
  (for [c (keys @cells)]
    (cell->debug c)))

(defn print-cells [cells]
  (print-table (map #(update % :error (fn [e] (when e (.getMessage e)))) cells)))

(defn- cycles? [sink-map cell]
  (loop [sinks   (get sink-map cell)
         visited #{cell}]
    (when (seq sinks)
      (if (seq (set/intersection sinks visited))
        true
        (recur (set (mapcat sink-map sinks)) (into visited sinks))))))

(defn- add-link [sinks source sink]
  (update sinks source
          (fn [x] (if-not x #{sink} (conj x sink)))))

(defn cell-code [cell]
  (:code (get @cells cell)))

(defn link! [source sink]
  (when-not (formula? sink)
    (throw (ex-info "Cannot add link, sink is not a formula"
                    {:source source
                     :sink sink
                     :source-code (cell-code source)
                     :sink-code (cell-code sink)})))
  (dosync
   (when-not (get-in @sinks [source sink])
     (if (and *detect-cycles* (cycles? (add-link @sinks source sink) source))
       (throw (ex-info "Cannot add link, cycle detected"
                       {:source source
                        :sink sink
                        :source-code (cell-code source)
                        :sink-code (cell-code sink)}))
       (do
         (alter sinks add-link source sink)
         (alter sources add-link sink source))))))

(def ^:private ^:dynamic *cell-context* nil)

(defn- calc-formula! [cell]
  (let [{:keys [thunk identity enabled?]} (get @cells cell)]
    (try
      (let [new-value (if enabled? (thunk) (identity))]
        (dosync (set-value! cell new-value)))
      (catch Exception e
        (dosync (set-error! cell (ex-info "Error initializing formula cell" {:thunk thunk} e)))))))

(defn- value [cell]
  (if-let [v (get @cells cell)]
    (if (.-formula cell)
      (or (:cache v)
          (calc-formula! cell))
      v)
    ::destroyed))

(defn- thunk [cell]
  (let [t (:thunk (get @cells cell))]
    (when-not t
      (throw (ex-info "Cell is not a formula" {:cell cell})))
    t))

(deftype CellID [id label formula]
  clojure.lang.IRef
  (deref [this] (value this)))

(comment
  (defmethod print-method CellID
    [x w]
    (.write w (pr-str {:id (.-id x)
                       :formula? (.-formula x)}))))

(defn cell? [x] (= (class x) datacore.cells.CellID))
(defn formula? [x] (and (cell? x) (.-formula x)))
(defn input? [x] (and (cell? x) (not (.-formula x))))

(defn- set-value! [cell x]
  (if (formula? cell)
    (alter cells assoc-in [cell :cache] x)
    (alter cells assoc cell x))
  x)

(defn- clear-cache! [cell]
  (if (formula? cell)
    (alter cells update cell dissoc :cache)
    ;;TODO throw exception
    ))

(defn mute! [cell]
  (if (formula? cell)
    (dosync (alter cells assoc-in [cell :enabled?] false)
            (touch! cell))
    ;;TODO throw exception
    ))

(defn unmute! [cell]
  (if (formula? cell)
    (dosync (alter cells assoc-in [cell :enabled?] true)
            (touch! cell))
    ;;TODO throw exception
    ))

(defn- set-error! [cell e]
  (if (formula? cell)
    (alter cells assoc-in [cell :error] e)
    ;;TODO throw exception
    )
  e)

(defn destroy! [cell]
  (dosync
   (let [cell-sinks (get @sinks cell)]
    ;;remove cell from sinks of its sources
    (doseq [source (get @sources cell)]
      (alter sources update source disj cell))
    ;;remove from registry
    (alter cells dissoc cell)
    ;;remove cell's sinks
    (alter sinks dissoc cell)
    ;;destroy its sinks that have just one source
    (doseq [sink cell-sinks]
      (when (= 1 (count (get @sources sink)))
        (destroy! sink))))))

(defn- register-cell! [x {:keys [formula? code label sources]}]
  (dosync
   (let [current-id @id
         cell       (CellID. current-id (keyword label) formula?)]
     (alter id inc)
     (if formula?
       (do
         (alter cells assoc cell {:thunk    (fn [] (apply x (map deref sources)))
                                  :identity (fn [] (deref (first sources)))
                                  :enabled? true
                                  :code     code})
         (doseq [source sources]
           (link! source cell)))
       (alter cells assoc cell x))
     cell)))

(defn cell
  ([x]
   (cell nil x))
  ([label x]
   (register-cell! x {:formula? false :label label})))

(defmacro defcell [name x]
  `(def ~name (cell ~(keyword name) ~x)))

(defn formula
  [fun & cells]
  (let [options (if (map? (first cells)) (first cells) {})
        cells   (if (map? (first cells)) (rest cells) cells)]
    (register-cell! fun (merge options {:formula? true
                                        :sources  cells}))))

(defmacro deformula
  [name fun & cells]
  `(def ~name (formula ~fun {:label ~(keyword name)} ~@cells)))

(defn- cells-into-pm [pm cells]
  (reduce (fn [pm cell]
            (assoc pm cell (.-id cell)))
          pm cells))

(defn- exception? [x]
  (instance? Exception x))

(defn- propagate [pri-map]
  (when-let [cell (first (peek pri-map))]
    ;;(prn (cell->debug cell))
    (let [popq  (pop pri-map)
          old   (:cache (get @cells cell))
          new   (calc-formula! cell)
          diff? (not= new old)]
      ;;(prn 'old-value old 'new-value new)
      (if (exception? new)
        (set-error! cell new)
        (when diff? (set-value! cell new)))
      (recur (if-not diff?
               popq ;;continue to next cell in priority map
               (cells-into-pm popq (get @sinks cell))))))) ;;add all sinks of cell to priority map before continuing

(defn touch! [cell]
  (if-not (.-formula cell)
    (throw (ex-info "Cannot touch, cell is not a formula" {:cell cell}))
    (dosync
     (let [new-value (calc-formula! cell)]
       (set-value! cell new-value)
       (propagate (cells-into-pm (pm/priority-map) (get @sinks cell)))
       new-value))))

(defn swap! [cell fun & args]
  (if (.-formula cell)
    (throw (ex-info "Cannot swap, cell is a formula" {:cell cell}))
    (dosync
     (let [current   @cell
           new-value (apply fun current args)]
       (when-not (= current new-value)
         (set-value! cell new-value)
         (propagate (cells-into-pm (pm/priority-map) (get @sinks cell))))
       new-value))))

(defn reset! [cell value]
  (swap! cell (fn [& _] value)))

(comment
  (defcell foo 100)
  (defcell bar 2)
  (defcell= baz
    (prn "calc baz!" (* 2 @foo @bar))
    (* 2 @foo @bar))
  (defcell= boo
    (prn "calc boo!" (* 2 @foo))
    (* 2 @foo))
  (defcell= boz
    (/ 10 @bar))

  ;;or
  (def baz (formula #(do
                       (prn "calc baz!" (* 2 @foo @bar))
                       (* 2 @foo @bar))))
  @baz

  (swap! foo inc)
  (swap! bar inc)
  )
