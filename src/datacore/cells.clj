(ns datacore.cells
  (:refer-clojure :exclude [swap!])
  (:require [clojure.data.priority-map :as pm]))

(def ^:private id (ref 0))
(def ^:private cells (ref {})) ;;map of cell IDs to cell values
(def ^:private links (ref {})) ;;map of cell IDs to sets of sinks

(defn link! [cell1 cell2]
  (dosync
   (alter links update (.-id cell2)
          (fn [x] (if-not x #{(.-id cell1)} (conj x (.-id cell1)))))))

(def ^:private ^:dynamic *cell-context* nil)

(defn value [cell]
  (when *cell-context*
    (link! *cell-context* cell))
  (if (.-formula cell)
    (binding [*cell-context* cell]
      ((get @cells (.-id cell))))
    (get @cells (.-id cell))))

(deftype CellID [id formula]
  clojure.lang.IRef
  (deref [this] (value this)))

(defn cell? [x] (instance? CellID x))

(defn- register-cell! [x formula?]
  (dosync
   (let [current-id @id]
     (alter id inc)
     (alter cells assoc current-id x)
     (CellID. current-id formula?))))

(defn cell [x]
  (register-cell! x false))

(defn formula [fun]
  (register-cell! fun true))

(defmacro cell= [& code]
  `(formula (fn [] ~@code)))

(comment
 (defn- propagate [pri-map]
   (when-let [next (first (peek pri-map))]
     (let [popq (pop pri-map)
           old  (x/get (.-prev next))
           new  (if-let [f (x/get (.-thunk next))] (f) (x/get (.-state next)))]
       (recur (if (= new old)
                popq ;;continue to next thing in priority map
                (reduce #(assoc %1 %2 (x/get (.-rank %2))) ;;add all sinks of cell to priority map before continuing
                        popq
                        (x/get (.-sinks next)))))))))

(defn swap! [cell fun & args]
  (if (.-formula cell)
    (throw (ex-info "Cannot swap, cell is a formula" {:cell cell}))
    (dosync
     (let [current   @cell
           new-value (apply fun current args)]
       (when-not (= current new-value)
         (alter cells assoc (.-id cell) new-value)
         (doseq [linked (get @links (.-id cell))]
           ((get @cells linked))))
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
