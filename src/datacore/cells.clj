(ns datacore.cells
  (:refer-clojure :exclude [swap! reset!])
  (:require [clojure.data.priority-map :as pm]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table pprint] :as pp]
            [clojure.walk :as walk]
            [clojure.core :as core]
            [clojure.spec :as s]))

(def ^:private cell-counter (atom -1))

(defrecord CellID [id])

(defn cell-id? [cell-id] (= (class cell-id) datacore.cells.CellID))

(s/def ::cells-graph (s/keys :req-un [::cells ::sinks ::sources]))

(s/def ::cells (s/map-of cell-id? ::cell))
(s/def ::sinks (s/map-of cell-id? (s/coll-of cell-id?)))
(s/def ::sources (s/map-of cell-id? (s/coll-of cell-id?)))

(s/def ::cell (s/or :input ::input-cell
                    :formula ::formula-cell))
(s/def ::input-cell (s/keys :req-un [::id ::value ::formula? ::label ::code]))
(s/def ::formula-cell (s/keys :req-un [::id ::fun ::sources-list ::formula? ::enabled? ::label ::code]
                              :opt-un [::value]))
(s/def ::id integer?)
(s/def ::value any?)
(s/def ::formula? boolean?)
(s/def ::enabled boolean?)
(s/def ::label (s/nilable keyword?))
(s/def ::code any?)
(s/def ::fun ifn?)
(s/def ::cell-sources-list (s/coll-of cell-id?))

(defn make-cells []
  {:cells {}     ;;map of cell IDs to cell values
   :sinks {}     ;;map of cell IDs to sets of sinks
   :sources {}}) ;;map of cell IDs to sets of sources
(s/fdef make-cells
  :ret ::cells-graph)

(def ^:private global-cells
  (atom (make-cells)))

(def ^:dynamic *detect-cycles* true)

(defn cell->debug
  ([cell-id]
   (cell->debug @global-cells cell-id))
  ([cells cell-id]
   (let [c (get-in cells [:cells cell-id])]
     {:id       (:id c)
      :label    (:label c)
      :formula? (:formula? c)
      :enabled? (if (:formula? c)
                  (:enabled? c)
                  "N/A")
      :value    (:value c)
      :error    (:error c)
      #_:code     #_(not-empty
                     (apply
                      list
                      (walk/postwalk
                       (fn [x]
                         (if (and (symbol? x)
                                  (= "clojure.core" (namespace x)))
                           (-> x name symbol)
                           x))
                       (:code v))))
      :sinks    (not-empty (set (map #(.-id %) (get-in cells [:sinks cell-id]))))
      :sources  (not-empty (set (map #(.-id %) (get-in cells [:sources cell-id]))))})))

(defn all-cells []
  (let [cells @global-cells]
    (for [ids (keys (:cells cells))]
      (cell->debug cells ids))))

(defn print-cells [cells]
  (print-table (map #(update % :error (fn [e] (when e (.getMessage e)))) cells)))

(defn- cycles? [sink-map cell]
  (loop [sinks   (get sink-map cell)
         visited #{cell}]
    (when (seq sinks)
      (if (seq (set/intersection sinks visited))
        true
        (recur (set (mapcat sink-map sinks)) (into visited sinks))))))

(defn- add-link [m from to]
  (update m from (fn [x] (if-not x #{to} (conj x to)))))

(declare formula?)
(defn link [cells source sink]
  (when-not (formula? cells sink)
    (throw (ex-info "Cannot add link, sink is not a formula"
                    {:source source
                     :sink sink})))
  (if (get-in cells [:sinks source sink])
    cells
    (let [new-cells (-> cells
                        (update :sinks add-link source sink)
                        (update :sources add-link sink source))]
      (if (and *detect-cycles* (cycles? (:sinks new-cells) source))
        (throw (ex-info "Cannot add link, cycle detected"
                        {:source source
                         :sink sink}))
        new-cells))))
(s/fdef link
 :args (s/cat :cells-graph ::cells-graph :source cell-id? :sink cell-id?)
 :ret  ::cells-graph)

(defn link! [source sink]
  (core/swap! global-cells link source sink))

(defn- all-blank-sources [cells cell-id]
  (loop [current-cells [cell-id]
         sources   #{}]
    (if-not (seq current-cells)
      sources
      (let [s (->> current-cells
                   (mapcat #(get-in cells [:sources %]))
                   (remove nil?)
                   (remove #(contains? (get-in cells [:cells %]) :value)))]
        (recur (concat (rest current-cells) s) (into sources s))))))

(defn- cells-into-pm [pm cells]
  (reduce (fn [pm cell]
            (assoc pm cell cell))
          pm cells))

(defn- current-value [cells cell-id]
  (get-in cells [:cells cell-id :value]))

(defn- lookup [cells cell-id]
  (if-let [cell (get-in cells [:cells cell-id])]
    cell
    (throw (ex-info "Cell not found in cells" {:cell-id cell-id
                                               :cells   cells}))))

(defn- calc-formula [cells {:keys [fun sources-list enabled?] :as cell}]
  (try
    (let [new-value (if enabled?
                      (apply fun (map (partial current-value cells) sources-list))
                      (current-value cells (first sources-list)))]
      (-> cell
          (assoc :value new-value)
          (dissoc :error)))
    (catch Exception e
      (assoc cell :error (ex-info "Error updating formula cell" {:cell cell} e)))))
(s/fdef calc-formula
  :args (s/cat :cells-graph ::cells-graph :formula ::formula-cell)
  :ret  ::formula-cell)

(defn- pull [cells cell-id]
  (let [pm (cells-into-pm (pm/priority-map-keyfn #(.-id %))
                          (conj (all-blank-sources cells cell-id) cell-id))]
    (reduce (fn [cells [_ source-id]]
              (assoc-in cells [:cells source-id]
                        (calc-formula cells (lookup cells source-id)))) cells pm)))

(defn value
  ([cell-id]
   (current-value
    (core/swap! global-cells #(second (value % cell-id)))
    cell-id))
  ([cells cell-id]
   (if-let [c (get-in cells [:cells cell-id])]
     (if (and (formula? cells cell-id) (not (:value c)))
       (let [new-cells (pull cells cell-id)]
         [(current-value new-cells cell-id) new-cells])
       [(:value c) cells])
     ::destroyed)))

(defn error
  [cells cell-id]
  (get-in cells [:cells cell-id :error]))

(defn formula?
  ([cell-id]
   (and (cell-id? cell-id) (formula? @global-cells cell-id)))
  ([cells cell-id] (get-in cells [:cells cell-id :formula?])))

(def input? (complement formula?))

(defn- update-formula [cells cell-id fun & args]
  (if (formula? cells cell-id)
    (update-in cells [:cells cell-id] #(apply fun % args))
    (throw (ex-info "Operation failed, cell is not a formula"
                    {:cell-id cell-id
                     :cell    (get-in cells [:cells cell-id])}))))

(defn set-error [cells cell-id e]
  (update-formula cells cell-id assoc :error e))

(defn- set-error! [cell-id e]
  (core/swap! global-cells set-error cell-id e))

(defn- sources [cells cell-id]
  (get-in cells [:sources cell-id]))

(defn- sinks [cells cell-id]
  (get-in cells [:sinks cell-id]))

(defn unlink [cells source sink]
  (-> cells
      (update-in [:sinks source] disj sink)
      (update-in [:sources sink] disj source)
      (update-in [:cells sink :sources-list]
                 (fn [coll] (mapv #(if (= % source) ::unlinked %) coll)))))
(s/fdef unlink
 :args (s/cat :cells-graph ::cells-graph :source cell-id? :sink cell-id?)
 :ret  ::cells-graph)

(defn unlink! [source sink]
  (core/swap! global-cells unlink source sink))

(defn destroy [cells cell-id]
  (as-> cells $
    (update $ :cells dissoc cell-id)
    (reduce (fn [cells sink-id] (unlink cells cell-id sink-id))
            $ (sinks cells cell-id))
    (reduce (fn [cells source-id] (unlink cells source-id cell-id))
            $ (sources cells cell-id))
    (update $ :sinks dissoc cell-id)))
(s/fdef destroy
 :args (s/cat :cells-graph ::cells-graph :cell-id cell-id?)
 :ret  ::cells-graph)

(defn destroy! [cell-id]
  (core/swap! global-cells destroy cell-id))

(defn- register-cell [cells cell-id v {:keys [formula? code label sources] :as options}]
  (let [id        (.-id cell-id)
        new-cells (assoc-in cells [:cells cell-id]
                            (if formula?
                              {:id           id
                               :fun          v
                               :sources-list (vec sources)
                               :formula?     true
                               :enabled?     true
                               :label        label
                               :code         code}
                              {:id       id
                               :value    v
                               :formula? false
                               :label    label
                               :code     code}))]
    (if-not (seq sources)
      new-cells
      (reduce (fn [cells source] (link cells source cell-id)) new-cells sources))))

(defn- new-cell-id []
  (CellID. (core/swap! cell-counter inc)))

(defn make-cell
  ([cells x] (make-cell cells nil x))
  ([cells label x]
   (let [id (new-cell-id)]
     [id (register-cell cells id x {:formula? false :label label})])))
(s/fdef make-cell
  :args (s/alt :unlabeled (s/cat :cells-graph ::cells-graph :value any?)
               :labeled   (s/cat :cells-graph ::cells-graph :label (s/nilable keyword?) :value any?))
  :ret  (s/cat :id cell-id? :new-cells ::cells-graph))

(defn cell
  ([x]
   (cell nil x))
  ([label x]
   (let [id (new-cell-id)]
     (core/swap! global-cells register-cell id x {:formula? false :label label})
     id)))
(s/fdef cell
  :args (s/alt :unlabeled (s/cat :value any?)
               :labeled   (s/cat :label (s/nilable keyword?) :value any?))
  :ret  cell-id?)

(defmacro defcell [name x]
  `(def ~name (cell ~(keyword name) ~x)))

(defn option-map? [x]
  (and (not (cell-id? x)) (map? x)))

(defn make-formula
  [cells fun & sources]
  (let [options (if (not (cell-id? (last sources))) (last sources) {})
        sources (if (not (cell-id? (last sources))) (butlast sources) sources)]
    (let [id (new-cell-id)]
      [id (register-cell cells id fun (merge options {:formula? true
                                                      :sources  sources}))])))
(s/fdef make-formula
  :args (s/cat :cells-graph ::cells-graph
               :function ifn?
               :sources (s/+ cell-id?)
               :options (s/? option-map?))
  :ret  (s/cat :id cell-id? :new-cells ::cells-graph))

(defn formula
  [fun & sources]
  (let [options (if (not (cell-id? (last sources))) (last sources) {})
        sources (if (not (cell-id? (last sources))) (butlast sources) sources)]
    (let [id (new-cell-id)]
      (core/swap! global-cells register-cell id fun (merge options {:formula? true
                                                                    :sources  sources}))
      id)))
(s/fdef formula
  :args (s/cat :function ifn? :sources (s/+ cell-id?) :options (s/? option-map?))
  :ret  cell-id?)

(defmacro deformula
  [name fun & cells]
  `(def ~name (formula ~fun ~@cells {:label ~(keyword name)})))

(defn- exception? [x]
  (instance? Exception x))

(defn- push [cells pri-map]
  (if-let [cell-id (first (peek pri-map))]
    (let [remaining (pop pri-map)
          cell      (get-in cells [:cells cell-id])
          new-cell  (calc-formula cells (lookup cells cell-id))
          diff?     (not= (:value cell)
                          (:value new-cell))]
      (recur (assoc-in cells [:cells cell-id] new-cell)
             (if (and diff? (not (:error new-cell)))
               (cells-into-pm remaining (get-in cells [:sinks cell-id]))
               remaining)))
    cells))

(defn touch [cells cell-id]
  (if-not (formula? cells cell-id)
    (throw (ex-info "Cannot touch, cell is not a formula" {:cell cell-id}))
    (let [new-cell (calc-formula cells (lookup cells cell-id))]
      (-> cells
          (assoc-in [:cells cell-id] new-cell)
          (push (cells-into-pm (pm/priority-map-keyfn #(.-id %))
                               (get-in cells [:sinks cell-id])))))))

(defn touch! [cell-id]
  (core/swap! global-cells touch cell-id))

(defn mute [cells cell-id]
  (-> cells
      (update-formula cell-id assoc :enabled? false)
      (touch cell-id)))

(defn mute! [cell-id]
  (core/swap! global-cells mute cell-id))

(defn unmute [cells cell-id]
  (-> cells
      (update-formula cell-id assoc :enabled? true)
      (touch cell-id)))

(defn unmute! [cell-id]
  (core/swap! global-cells unmute cell-id))

(defn swap [cells cell-id fun args]
  (if (formula? cells cell-id)
    (throw (ex-info "Cannot swap, cell is a formula" {:cell cell-id}))
    (let [current   (get-in cells [:cells cell-id :value])
          new-value (apply fun current args)]
      (if (= current new-value)
        cells
        (-> cells
            (assoc-in [:cells cell-id :value] new-value)
            (push (cells-into-pm (pm/priority-map-keyfn #(.-id %))
                                 (get-in cells [:sinks cell-id]))))))))

(defn swap! [cell-id fun & args]
  (get-in (core/swap! global-cells swap cell-id fun args)
          [:cells cell-id :value]))

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
