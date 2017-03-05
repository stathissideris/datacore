(ns datacore.cells
  (:refer-clojure :exclude [swap! reset!])
  (:require [clojure.data.priority-map :as pm]
            [clojure.set :as set]
            [datacore.error :as error]
            [clojure.pprint :refer [print-table]]
            [clojure.walk :as walk]))

(def ^:private id (ref 0))
(def ^:private cells (ref {})) ;;map of cell IDs to cell values
(def ^:private links (ref {})) ;;map of cell IDs to sets of sinks

(def ^:dynamic *detect-cycles* false)

(declare set-value! set-error! formula?)

(defn all-cells []
  (for [[c v] @cells]
    {:id       (.-id c)
     :label    (.-label c)
     :formula? (.-formula c)
     :value    (if (.-formula c)
                 (:cache v)
                 v)
     :error    (when (:error v) (.getMessage (:error v)))
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
     :sinks    (not-empty (set (map #(.-id %) (get @links c))))}))

(defn print-all-cells []
  (print-table (all-cells)))

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
   (when-not (get-in @links [source sink])
     (if (and *detect-cycles* (cycles? (add-link @links source sink) source))
       (throw (ex-info "Cannot add link, cycle detected"
                       {:source source
                        :sink sink
                        :source-code (cell-code source)
                        :sink-code (cell-code sink)}))
       (alter links add-link source sink)))))

(def ^:private ^:dynamic *cell-context* nil)

(defn- value [cell]
  (when *cell-context*
    (link! cell *cell-context*))
  (if (.-formula cell)
    (binding [*cell-context* cell]
      (let [{:keys [cache thunk code]} (get @cells cell)]
        (or cache
            (try
              (let [new-value (thunk)]
                (dosync (set-value! cell new-value)))
              (catch Exception e
                (dosync (set-error! cell (ex-info "Error initializing formula cell" e {:thunk thunk}))))))))
    (get @cells cell)))

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

(defn- set-error! [cell e]
  (if (formula? cell)
    (alter cells assoc-in [cell :error] e)
    ;;TODO throw exception
    )
  e)

(defn- register-cell! [x {:keys [formula? code label]}]
  (dosync
   (let [current-id @id
         cell       (CellID. current-id label formula?)]
     (alter id inc)
     (if formula?
       (alter cells assoc cell {:thunk x :code code})
       (alter cells assoc cell x))
     cell)))

(defn cell [x]
  (register-cell! x {:formula? false}))

(defmacro defcell [name x]
  `(def ~name
     (register-cell! ~x {:formula? false
                         :label (quote ~name)})))

(defn formula
  ([fun]
   (formula fun nil))
  ([fun {:keys [code label] :as options}]
   (let [cell (register-cell! fun (merge options {:formula? true}))]
     (value cell) ;;to initialize cache and establish links
     cell)))

(defmacro cell= [code]
  `(formula (fn [] ~code)
            {:code (quote ~code)}))

(defmacro defcell= [name code]
  `(def
     ~name
     (formula (fn [] ~code)
              {:code  (quote ~code)
               :label (quote ~name)})))

(defn- cells-into-pm [pm cells]
  (reduce (fn [pm cell]
            (assoc pm cell (.-id cell)))
          pm cells))

(defn- exception? [x]
  (instance? Exception x))

(defn- propagate [pri-map]
  (when-let [next (first (peek pri-map))]
    (let [popq  (pop pri-map)
          old   (value next)
          new   (try ((thunk next))
                     (catch Exception e
                       e))
          diff? (not= new old)]
      (if (exception? new)
        (set-error! next new)
        (when diff? (set-value! next new)))
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
