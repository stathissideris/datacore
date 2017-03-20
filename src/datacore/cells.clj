(ns datacore.cells
  (:refer-clojure :exclude [swap! reset! add-watch remove-watch])
  (:require [clojure.data.priority-map :as pm]
            [clojure.set :as set]
            [clojure.pprint :refer [print-table pprint] :as pp]
            [clojure.walk :as walk]
            [clojure.core :as core]
            [clojure.spec :as s]
            [datacore.util :as util]
            [datacore.reflection :as reflection]))

;; TODO
;; mutation for labels and meta map
;; watches
;; pluggable caching strategy
;; pluggabe execution strategy

(def ^:private cell-counter (atom -1))

(defrecord CellID [id])

(defn cell-id? [cell-id] (= (class cell-id) datacore.cells.CellID))

(defn make-cells []
  {:cells {}     ;;map of cell IDs to cell values
   :sinks {}     ;;map of cell IDs to sets of sinks
   :sources {}   ;;map of cell IDs to sets of sources
})

(def ^:private global-cells (atom (make-cells)))
(def watches (atom {}))
(core/add-watch
 global-cells ::global
 (fn [_ _ old new]
   (let [w        @watches
         cell-ids (keys (:cells new))]
     (doseq [cell-id cell-ids]
       (when-let [cell-watches (get w cell-id)]
         (let [old-value (-> old :cells (get cell-id) :value)
               new-value (-> new :cells (get cell-id) :value)]
           (when-not (= old-value new-value)
             (doseq [[key fun] cell-watches]
               (fun key old-value new-value)))))))))

(def ^:private global-watches (atom {}))

(defn formula?
  ([cell-id]
   (and (cell-id? cell-id) (formula? @global-cells cell-id)))
  ([cells cell-id] (get-in cells [:cells cell-id :formula?])))

(def input? (complement formula?))

(s/def ::cells-graph (s/keys :req-un [::cells ::sinks ::sources]))

(s/def ::cell-id cell-id?)

(s/def ::cells (s/map-of ::cell-id ::cell))
(s/def ::sinks (s/map-of ::cell-id (s/coll-of ::cell-id)))
(s/def ::sources (s/map-of ::cell-id (s/coll-of ::cell-id)))
(s/def ::watches (s/map-of ::cell-id (s/map-of any? ifn?)))

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
(s/def ::cell-sources-list (s/coll-of (s/or :cell ::cell-id
                                            :unlinked #{::unlinked})))
(s/fdef make-cells
  :ret ::cells-graph)

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
  (print-table
   (map #(-> %
             (update :value (fn [v] (util/truncate-string (pr-str v) 35)))
             (update :error (fn [e] (when e (.getMessage e))))) cells)))

(defn- cycles? [sink-map cell]
  (loop [sinks   (get sink-map cell)
         visited #{cell}]
    (when (seq sinks)
      (if (seq (set/intersection sinks visited))
        true
        (recur (set (mapcat sink-map sinks)) (into visited sinks))))))

(defn- add-link [m from to]
  (update m from (fn [x] (if-not x #{to} (conj x to)))))

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
 :args (s/cat :cells ::cells-graph :source ::cell-id :sink ::cell-id)
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

(defn- lookup [cells cell-id]
  (if-let [cell (get-in cells [:cells cell-id])]
    cell
    (throw (ex-info "Cell not found in cells" {:cell-id cell-id
                                               :cells   cells}))))

(defn- current-value [cells cell-id]
  (if-let [cell (get-in cells [:cells cell-id])]
    (:value cell)
    ::destroyed))

(defn- calc-formula [cells {:keys [fun sources-list enabled?] :as cell}]
  (let [input-value #(when-not (= ::unlinked %) (current-value cells %))]
    (try
      (-> cell
          (dissoc :error)
          (assoc :value (if enabled?
                          (apply fun (map input-value sources-list))
                          (input-value (first sources-list)))))
      (catch Exception e
        (assoc cell :error (ex-info "Error updating formula cell" {:cell cell} e))))))
(s/fdef calc-formula
  :args (s/cat :cells ::cells-graph :formula ::formula-cell)
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
     [::destroyed cells])))

(defn error
  [cells cell-id]
  (get-in cells [:cells cell-id :error]))

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

(declare touch)
(defn unlink
  ([cells source sink]
   (unlink cells source sink true))
  ([cells source sink push?]
   (if (= source ::unlinked)
     cells
     (as-> cells $
       (update-in $ [:sinks source] disj sink)
       (update-in $ [:sources sink] disj source)
       (update-in $ [:cells sink :sources-list]
                  (fn [coll] (mapv #(if (= % source) ::unlinked %) coll)))
       (update-in $ [:cells sink] dissoc :value)
       (if-not push? $ (touch $ sink))))))
(s/fdef unlink
  :args (s/cat :cells ::cells-graph
               :source (s/or :source ::cell-id
                             :unlinked #{::unlinked})
               :sink ::cell-id
               :push? (s/? boolean?))
  :ret  ::cells-graph)

(defn unlink! [source sink]
  (core/swap! global-cells unlink source sink))

(defn destroy [cells cell-id]
  (as-> cells $
    (reduce (fn [cells sink-id] (unlink cells cell-id sink-id false))
            $ (sinks cells cell-id))
    (reduce (fn [cells source-id] (unlink cells source-id cell-id false))
            $ (sources cells cell-id))
    (update $ :sinks dissoc cell-id)
    (update $ :cells dissoc cell-id)
    (reduce (fn [cells sink-id] (touch cells sink-id))
            $ (sinks cells cell-id))))
(s/fdef destroy
 :args (s/cat :cells ::cells-graph :cell-id ::cell-id)
 :ret  ::cells-graph)

(defn destroy! [cell-id]
  (core/swap! global-cells destroy cell-id))

(defn unlink-slot
  ([cells sink-id slot-idx]
   (unlink-slot cells sink-id slot-idx true))
  ([cells sink-id slot-idx push?]
   (let [sink   (lookup cells sink-id)
         source (nth (:sources-list sink) slot-idx)]
     (unlink cells source sink-id push?))))
(s/fdef unlink-slot
  :args (s/cat :cells ::cells-graph :sink ::cell-id :slot nat-int? :push? (s/? boolean?))
  :ret ::cells-graph)

(defn unlink-slot! [sink-id slot-idx]
  (core/swap! global-cells unlink-slot sink-id slot-idx))

(defn link-slot [cells source-id sink-id slot-idx]
  (-> cells
      (unlink-slot sink-id slot-idx false)
      (link source-id sink-id)
      (assoc-in [:cells sink-id :sources-list slot-idx] source-id)
      (touch sink-id)))
(s/fdef link-slot
  :args (s/cat :cells ::cells-graph, :source ::cell-id, :sink ::cell-id :slot nat-int?)
  :ret ::cells-graph)

(defn link-slot! [source-id sink-id slot-idx]
  (core/swap! global-cells link-slot source-id sink-id slot-idx))

(defn- upstream [cells cell-id]
  (first (sources cells cell-id)))

(defn- downstream [cells cell-id]
  (first (sinks cells cell-id)))

(defn linear-move-up [cells cell-id]
  (let [cell (lookup cells cell-id)]
    (when (= ::unlinked (upstream cells cell-id))
      (throw (ex-info "Cannot move cell up because there is no upstream cell" {:cell cell})))
    (when-not (= 1 (count (sources cells cell-id)))
      (throw (ex-info "Only cells with exactly one source can be moved up" {:cell cell})))
    (when-not (>= 1 (count (sources cells (upstream cells cell-id))))
      (throw (ex-info "Only cells whose upstream cell has at most one source can be moved up"
                      {:cell cell
                       :upstream (lookup cells (upstream cells cell-id))})))
    (when (input? cells (upstream cells cell-id))
      (throw (ex-info "Can't move up if its source is an input cell"
                      {:cell cell
                       :upstream (lookup cells (upstream cells cell-id))})))

    (let [parent      (upstream cells cell-id)
          grandparent (upstream cells parent)
          child       (downstream cells cell-id)]
      (cond-> cells
        ;;pull the wires
        :always     (unlink parent cell-id false)
        grandparent (unlink grandparent parent false)
        child       (unlink cell-id child false)
        ;;connect the wires
        grandparent (link-slot grandparent cell-id 0)
        :always     (link-slot cell-id parent 0)
        child       (link-slot parent child 0)))))

(defn linear-move-up! [cell-id]
  (core/swap! global-cells linear-move-up cell-id))

(defn linear-move-down [cells cell-id]
  (let [cell (lookup cells cell-id)]
    (when-not (downstream cells cell-id)
      (throw (ex-info "Cannot move cell down because there is no downstream cell" {:cell cell})))
    (when-not (= 1 (count (sinks cells cell-id)))
      (throw (ex-info "Only cells with exactly one sink can be moved down" {:cell cell})))
    (when-not (>= 1 (count (sinks cells (downstream cells cell-id))))
      (throw (ex-info "Only cells whose downstream cell has at most one sink can be moved down"
                      {:cell cell
                       :downstream (lookup cells (downstream cells cell-id))})))
    (let [child      (downstream cells cell-id)
          grandchild (downstream cells child)
          parent     (upstream cells cell-id)]
      (cond-> cells
        ;;pull the wires
        :always    (unlink cell-id child)
        grandchild (unlink child grandchild)
        parent     (unlink parent cell-id)
        ;;connect the wires
        parent     (link-slot parent child 0)
        :always    (link-slot child cell-id 0)
        grandchild (link-slot cell-id grandchild 0)))))

(defn linear-move-down! [cell-id]
  (core/swap! global-cells linear-move-down cell-id))

(defn linear-insert [cells parent cell child]
  (when-not (= 1 (count (sinks cells parent)))
    (throw (ex-info "Linear insert parent has to have one sink only"
                    {:parent (lookup cells parent)
                     :cell   (lookup cells cell)
                     :child  (lookup cells child)})))
  (when-not (= 1 (count (sources cells child)))
    (throw (ex-info "Linear insert child has to have one source only"
                    {:parent (lookup cells parent)
                     :cell   (lookup cells cell)
                     :child  (lookup cells child)})))

  (-> cells
      (unlink parent child false)
      (link-slot parent cell 0)
      (link-slot cell child 0)))

(defn linear-insert! [parent cell child]
  (core/swap! global-cells linear-insert parent cell child))

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
      (reduce (fn [cells source]
                (if (= ::unlinked source)
                  cells
                  (link cells source cell-id)))
              new-cells sources))))

(defn- new-cell-id []
  (CellID. (core/swap! cell-counter inc)))

(defn make-cell
  ([cells x] (make-cell cells nil x))
  ([cells label x]
   (let [id (new-cell-id)]
     [id (register-cell cells id x {:formula? false :label label})])))
(s/fdef make-cell
  :args (s/alt :unlabeled (s/cat :cells ::cells-graph :value any?)
               :labeled   (s/cat :cells ::cells-graph :label (s/nilable keyword?) :value any?))
  :ret  (s/cat :id ::cell-id :new-cells ::cells-graph))

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
  :ret  ::cell-id)

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
  :args (s/cat :cells ::cells-graph
               :function ifn?
               :sources (s/+ ::cell-id)
               :options (s/? option-map?))
  :ret  (s/cat :id ::cell-id :new-cells ::cells-graph))

(defn formula
  [fun & sources]
  (let [options (if (not (cell-id? (last sources))) (last sources) {})
        sources (if (not (cell-id? (last sources))) (butlast sources) sources)]
    (let [id (new-cell-id)]
      (core/swap! global-cells register-cell id fun (merge options {:formula? true
                                                                    :sources  sources}))
      id)))
(s/fdef formula
  :args (s/cat :function ifn?
               :sources (s/+ (s/or :source ::cell-id
                                   :unlinked #{::unlinked}))
               :options (s/? option-map?))
  :ret  ::cell-id)

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
(s/fdef touch
  :args (s/cat :cells ::cells-graph :cell ::cell-id)
  :ret ::cells-graph)

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

(defn- new-sources [old-sources fun]
  (let [arities (set (reflection/arities fun))]
    (if (contains? arities :rest)
      old-sources
      (vec (util/take-exactly (apply max (disj arities :rest))
                              old-sources
                              ::unlinked)))))

(defn swap-function [cells cell-id fun]
  (when-not (formula? cells cell-id)
    (throw (ex-info "Cannot swap-function, cell is not a formula" {:cell cell-id})))
  (let [current-fun     (get-in cells [:cells cell-id :fun])
        sources         (get-in cells [:cells cell-id :sources-list])
        new-sources     (new-sources sources fun)
        removed-sources (set/difference (set sources) (set new-sources))]
    (if (= fun current-fun)
      cells
      (as-> cells $
          (assoc-in $ [:cells cell-id :fun] fun)
          (assoc-in $ [:cells cell-id :sources-list] new-sources)
          (reduce (fn [cells removed] (unlink cells removed cell-id false))
                  $ removed-sources)
          (touch $ cell-id)))))
(s/fdef swap-function
  :args (s/cat :cells ::cells-graph :cell ::cell-id :fun ifn?)
  :ret ::cells-graph)

(defn swap-function! [cell-id fun]
  (core/swap! global-cells swap-function cell-id fun))

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

(defn linked?
  ([source sink]
   (linked? @global-cells source sink))
  ([cells source sink]
   (some? (get-in cells [:sinks source sink]))))

(defn add-watch! [cell-id key fun]
  (core/swap! watches assoc-in [cell-id key] fun))

(defn remove-watch! [cell-id key]
  (core/swap! watches update cell-id dissoc key))
