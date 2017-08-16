  (:refer-clojure :exclude [swap! reset! meta alter-meta!])
(ns datacore.cells-test
  (:require [datacore.cells :refer :all]
            [clojure.test :refer :all]
            [clojure.core :as core]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(def global-cells @#'datacore.cells/global-cells)
(core/swap! global-cells (fn [_] (make-cells)))

(deftest test-specs
  (is (true?
       (s/valid? :datacore.cells/cell
                 {:id       8
                  :value    {}
                  :formula? false
                  :label    :state
                  :code     nil})))
  (is (true?
       (s/valid? :datacore.cells/input-cell
                 {:id       8
                  :value    {}
                  :formula? false
                  :label    :state
                  :code     nil})))
  (is (true?
       (s/valid? :datacore.cells/cell
                 {:id           11
                  :fun          :sources
                  :sources-list [#datacore.cells.CellID{:id 10}]
                  :formula?     true
                  :enabled?     true
                  :label        :sources
                  :code         nil})))
  (is (true?
       (s/valid? :datacore.cells/formula-cell
                 {:id           11
                  :fun          :sources
                  :sources-list [#datacore.cells.CellID{:id 10}]
                  :formula?     true
                  :enabled?     true
                  :label        :sources
                  :code         nil}))))

(def cycles? @#'datacore.cells/cycles?)
(deftest test-cycles?
  (is (nil?  (cycles? {:a #{:b :c}} :a)))
  (is (true? (cycles? {:a #{:b :c} :b #{:a :z}} :a)))
  (is (true? (cycles? {:a #{:b :c} :b #{:d} :d #{:e} :e #{:a}} :a)))
  (is (true? (cycles? {:a #{:b :c} :b #{:d} :d #{:e} :e #{:b}} :a)))
  (is (true? (cycles? {:a #{:b :c} :b #{:d} :d #{:e} :e #{:b}} :d)))
  (is (nil?  (cycles? {:a #{:b :c} :b #{:d} :d #{:e} :e #{:f}} :a))))

(def all-blank-sources @#'datacore.cells/all-blank-sources)
(deftest test-all-blank-sources
  (let [cells     (make-cells)
        [a cells] (make-cell cells :a 100)
        [b cells] (make-formula cells (fn [x] (+ x 3)) a {:label :b})
        [c cells] (make-cell cells :c 200)
        [d cells] (make-formula cells (fn [x] (+ x 300)) c {:label :d})
        [e cells] (make-formula cells (fn [x y] (* x y)) b d {:label :e})]
    (is (= #{b d} (all-blank-sources cells e)))))

(deftest test-value
  (let [cells     (make-cells)
        [a cells] (make-cell cells 100)
        [b cells] (make-formula cells (fn [x] (+ x 3)) a)
        [c cells] (make-cell cells 200)
        [d cells] (make-formula cells (fn [x] (+ x 300)) c)
        [e cells] (make-formula cells (fn [x y] (* x y)) b d)]
    (is (= (* (+ 100 3)
              (+ 200 300)) (first (value cells e))))))

(deftest test-propagation
  (testing "one level"
    (let [a (cell 100)
          b (cell 2)
          c (formula (partial * 2) a b)]
      (is (= 400 (value c)))

      (testing "- 1"
        (swap! a inc)
        (is (= 101 (value a)))
        (is (= 2 (value b)))
        (is (= (* 101 2 2) (value c))))

      (testing "- 2"
        (swap! b inc)
        (is (= 101 (value a)))
        (is (= 3 (value b)))
        (is (= (* 101 3 2) (value c))))))

  (testing "one level - no change"
    (let [a (cell 100)
          b (cell 2)
          c (formula (partial * 2) a b)]
      (is (= 400 (value c)))
      (swap! a identity)
      (is (= 100 (value a)))
      (is (= 2 (value b)))
      (is (= 400 (value c)))))

  (testing "two levels"
    (let [a (cell 100)
          b (cell 2)
          c (formula (partial * 2) a b)
          d (formula (partial * 10) c)]
      (is (= 400 (value c)))
      (is (= 4000 (value d)))

      (testing "- 1"
        (swap! a inc)
        (is (= 101 (value a)))
        (is (= 2 (value b)))
        (is (= (* 101 2 2) (value c)))
        (is (= (* 101 2 2 10) (value d))))

      (testing "- 2"
        (swap! b inc)
        (is (= 101 (value a)))
        (is (= 3 (value b)))
        (is (= (* 101 3 2) (value c)))
        (is (= (* 101 3 2 10) (value d))))))

  (testing "propagation happens once when swapping and reading value"
    (let [log (atom 0)
          a   (cell 10)
          b   (formula (fn [x] (core/swap! log inc) (* 2 x)) a)]
      (swap! a inc)
      (is (= 1 @log))
      (value b)
      (is (= 1 @log))))

  (testing "propagation happens once when resetting and reading value"
    (let [log (atom 0)
          a   (cell 10)
          b   (formula (fn [x] (core/swap! log inc) (* 2 x)) a)]
      (reset! a 200)
      (is (= 1 @log))
      (value b)
      (is (= 1 @log))))

  (testing "long chain 1"
    (let [chain (reduce (fn [chain _]
                          (conj chain (formula inc (last chain))))
                        [(cell 0)] (range 5))]
      (swap! (first chain) #(+ % 5))
      (doall (map-indexed (fn [i c] (is (= (+ i 5) (value c)))) chain))))

  (testing "long chain 2"
    (let [chain (reduce (fn [chain _]
                          (conj chain (formula inc (last chain))))
                        [(cell 0)] (range 5))
          touch (atom 0)
          chain (conj chain (formula (fn [x]
                                       (core/swap! touch inc)
                                       x)
                                     (last chain)))]
      (swap! (first chain) #(+ % 1000))
      (is (= 1 @touch))))

  (testing "chain 3"
    (let [log (atom [])
          a   (cell :a 100)
          b   (formula (fn [x]
                         (core/swap! log conj :b)
                         (+ x 10))
                       a
                       {:label :b})
          c   (formula (fn [x]
                         (core/swap! log conj :c)
                         (+ x 20))
                       a
                       {:label :c})
          d   (formula (fn [b c]
                         (core/swap! log conj :d)
                         (+ b c))
                       b c
                       {:label :d})]
      (swap! a inc)
      (is (= [:b :c :d] @log))))

  (testing "lazy seqs"
    (let [a (cell :a (range 10))
          b (formula (partial map inc) a {:label :b})
          c (formula (partial map inc) b {:label :c})]
      (value c)
      (is (not (realized? (value b))))
      (is (not (realized? (value c))))
      (doall (value c))
      (is (realized? (value b)))
      (is (realized? (value c))))))

(deftest test-altering-graph
  (testing "mute 1"
    (let [cells     (make-cells)
          [a cells] (make-cell cells 100)
          [b cells] (make-formula cells (fn [x] (+ x 3)) a)
          cells     (mute cells b)]
      (is (false? (some-> cells :cells (get b) :enabled?)))))

  (testing "mute 2"
    (let [a (cell :a 100)
          b (formula (partial * 10) a)
          c (formula (partial + 1) b)]
      (is (= 1001 (value c)))
      (mute! b)
      (is (= 101 (value c)))
      (unmute! b)
      (is (= 1001 (value c)))))

  (testing "destroy 1"
    (let [a (cell :a 100)
          b (formula (partial * 10) a)
          c (formula (partial + 1) b)]
      (destroy! b)
      (is (= :datacore.cells/destroyed (value b)))
      (is (nil? (value c)))
      (is (= :datacore.cells/unlinked (-> @global-cells :cells (get c) :sources-list first)))))

  (testing "destroy 2 - destruction is propagated"
    ;; can't test by looking putting side-effects in e's function,
    ;; because the function is not called one of the sources has no
    ;; value, value is set directly to ::no-value. We'd need effect
    ;; cells for that. Or should we be calling the function anyway?
    (let [safe-fn (fn [fun]
                    (fn [& args]
                      (try (apply fun args)
                           (catch Exception _ nil))))
          a       (cell :a 100)
          b       (formula (safe-fn +) a)
          c       (formula (safe-fn -) b)
          d       (formula (safe-fn *) c)
          e       (formula (safe-fn /) d)]
      (is (= -1/100 (value e)))
      (destroy! b)
      (is (nil? (-> @global-cells :cells (get e) :value)))
      (is (= :datacore.cells/destroyed (value b)))
      (is (nil? (value c)))
      (is (= :datacore.cells/unlinked (-> @global-cells :cells (get c) :sources-list first)))
      (is (nil? (value d)))
      (is (nil? (value e)))))

  (testing "destroy 3"
    (let [a (cell :a 100)
          b (formula (partial * 100) a)
          c (formula (partial * 10) a)
          d (formula (partial + 1) b c)]
      (destroy! c)
      (is (= 100 (value a)))
      (is (= 10000 (value b)))
      (is (= :datacore.cells/destroyed (value c)))
      (is (nil? (value d)))
      (is (= b (-> @global-cells :cells (get d) :sources-list first)))
      (is (= :datacore.cells/unlinked (-> @global-cells :cells (get d) :sources-list second)))))

  (testing "recover well from destruction"
    (let [a   (cell :a 100)
          b   (cell :b 200)
          c   (cell :c 300)
          sum (formula (fn [& args] (apply + (remove nil? args))) a b c)
          res (formula identity sum)]
      (is (= 600 (value res)))
      (destroy! b)
      (is (= 400 (-> @global-cells :cells (get res) :value))) ;; make sure a push happens
      (is (= 400 (value res)))))

  (testing "recover well from unlinking cells"
    (let [a   (cell :a 100)
          b   (cell :b 200)
          c   (cell :c 300)
          sum (formula (fn [& args] (apply + (remove nil? args))) a b c)
          res (formula identity sum)]
      (is (= 600 (value res)))
      (unlink! b sum)
      (is (= 400 (-> @global-cells :cells (get res) :value))) ;; make sure a push happens
      (is (= 400 (value res)))))

  (testing "recover well from unlinking slot"
    (let [a   (cell :a 100)
          b   (cell :b 200)
          c   (cell :c 300)
          sum (formula (fn [& args] (apply + (remove nil? args))) a b c)
          res (formula identity sum)]
      (is (= 600 (value res)))
      (unlink-slot! sum 1)
      (is (= 400 (-> @global-cells :cells (get res) :value))) ;; make sure a push happens
      (is (= 400 (value res)))))

  (testing "unlink twice"
    (let [a   (cell :a 100)
          b   (cell :b 200)
          c   (cell :c 300)
          sum (formula (fn [& args] (apply + (remove nil? args))) a b c)
          res (formula identity sum)]
      (is (= 600 (value res)))
      (unlink-slot! sum 1)
      (is (= 400 (-> @global-cells :cells (get res) :value))) ;; make sure a push happens
      (is (= 400 (value res)))
      (unlink-slot! sum 1)
      (is (= 400 (value res)))))

  (testing "relinking"
    (let [a     (cell :a 100)
          b     (cell :b 200)
          alt-b (cell :alt-b 1000)
          c     (cell :c 300)
          sum   (formula (fn [& args] (apply + (remove nil? args))) a b c)
          res   (formula identity sum)]
      (is (= 600 (value res)))
      (link-slot! alt-b sum 1)
      (is (= 1400 (-> @global-cells :cells (get res) :value))) ;; make sure a push happens
      (is (= 1400 (value res)))))

  (testing "relinking and unlinking and linking something else"
    (let [a     (cell :a 100)
          b     (cell :b 200)
          alt-b (cell :alt-b 1000)
          c     (cell :c 300)
          sum   (formula (fn [& args] (apply + (remove nil? args))) a b c)
          res   (formula identity sum)]
      (is (= 600 (value res)))
      (link-slot! alt-b sum 1)
      (is (= 1400 (-> @global-cells :cells (get res) :value))) ;; make sure a push happens
      (is (= 1400 (value res)))
      (unlink-slot! sum 0)
      (is (= 1300 (-> @global-cells :cells (get res) :value))) ;; make sure a push happens
      (is (= 1300 (value res)))
      (link-slot! b sum 0)
      (is (= 1500 (-> @global-cells :cells (get res) :value))) ;; make sure a push happens
      (is (= 1500 (value res)))))

  (testing "link into formula that didn't have any inputs"
    (let [a          (cell :a 100)
          b          (formula (partial + 1) a {:label :b})
          c          (formula (partial + 50) b {:label :c})
          d          (formula vector {:label :d})
          propagated (atom 0)]
      (add-watch! d :testing (fn [_ _ _] (core/swap! propagated inc)))
      (link! c d)
      (is (= 1 @propagated))
      (is (= 101 (value b)))
      (is (= 151 (value c)))
      (is (= [151] (value d)))))

  (testing "linear-move-up 1"
    (let [a (cell :a 100)
          b (formula (partial * 10) a {:label :b})
          c (formula (partial + 1) b {:label :c})
          d (formula (partial / 20) c {:label :d})]
      (is (= (->> 100 (* 10) (+ 1) (/ 20)) (value d)))
      ;;change graph from:
      ;; a->b->c->d
      ;; to:
      ;; a->c->b->d
      (linear-move-up! c)
      (is (= (->> 100 (+ 1) (* 10) (/ 20)) (value d)))))

  (testing "linear-move-up 2"
    (let [a (cell :a 100)
          b (formula (partial * 10) a {:label :b})
          c (formula (partial + 1) b {:label :c})]
      (is (thrown? Exception (linear-move-up! b)))))

  (testing "linear-move-up 3"
    (let [b (formula (partial * 10) :datacore.cells/unlinked {:label :b})
          c (formula (partial + 1) b {:label :c})]
      (is (thrown? Exception (linear-move-up! b)))))

  (testing "linear-move-down 1"
    (let [a (cell :a 100)
          b (formula (partial * 10) a {:label :b})
          c (formula (partial + 1) b {:label :c})
          d (formula (partial / 20) c {:label :d})]
      (is (= (->> 100 (* 10) (+ 1) (/ 20)) (value d)))
      ;;change graph from:
      ;; a->b->c->d
      ;; to:
      ;; a->c->b->d
      (linear-move-down! b)
      (is (= (->> 100 (+ 1) (* 10) (/ 20)) (value d)))))

  (testing "linear-move-down 2"
    (let [a (cell :a 100)
          b (formula (partial * 10) a {:label :b})
          c (formula (partial + 1) b {:label :c})]
      (is (= (->> 100 (* 10) (+ 1)) (value c)))
      (linear-move-down! b)
      (is (= (->> 100 (+ 1) (* 10)) (value b)))))

  (testing "linear-move-down 3"
    (let [a (cell :a 100)
          b (formula (partial * 10) a {:label :b})
          c (formula (partial + 1) b {:label :c})]
      (is (thrown? Exception (linear-move-down! c)))))

  (testing "linear-insert"
    (let [a (cell :a 100)
          b (formula (partial * 10) :datacore.cells/unlinked {:label :b})
          c (formula (partial + 1) a {:label :c})]
      (is (= 101 (value c)))
      (linear-insert! a b c)
      (is (= (->> 100 (* 10) (+ 1)) (-> @global-cells :cells (get c) :value)))
      (is (= (->> 100 (* 10) (+ 1)) (value c)))))

  (testing "swap-function 1"
    (let [a (cell :a 100)
          b (formula (partial * 10) a)]
      (is (= 1000 (value b)))
      (swap-function! b (fn [a b c] (+ a b c)))
      (is (= [a :datacore.cells/unlinked :datacore.cells/unlinked]
             (-> @global-cells :cells (get b) :sources-list)))
      (let [in2 (cell 200)
            in3 (cell 300)]
        (link-slot! in2 b 1)
        (link-slot! in3 b 2)
        (is (= (+ 100 200 300) (value b))))))

  (testing "swap-function 2"
    (let [a   (cell :a 10)
          b   (cell :b 20)
          c   (cell :c 30)
          d   (cell :d 40)
          res (formula (partial +) a b c d)]
      (is (= (+ 10 20 30 40) (value res)))
      (swap-function! res (fn [a b] (* a b)))
      (is (= [a b] (-> @global-cells :cells (get res) :sources-list)))
      (is (= (* 10 20) (-> @global-cells :cells (get res) :value)))
      (is (= (* 10 20) (value res)))
      (is (not (linked? c res)))
      (is (not (linked? d res))))))

(deftest test-watches
  (testing "watches 1 - watches work on input cells"
    (let [a     (cell 10)
          touch (atom false)]
      (add-watch! a :a (fn [_ _ new] (core/reset! touch new)))
      (is (false? @touch))
      (swap! a inc)
      (is (= 11 @touch))))

  (testing "watches 2 - watches work on formula cells"
    (let [a     (cell 10)
          b     (formula inc a)
          touch (atom false)]
      (add-watch! b :b (fn [_ _ new] (core/reset! touch new)))
      (is (false? @touch))
      (swap! a inc)
      (is (= 12 @touch))))

  (testing "watches 3 - watches work on multiple formulas"
    (let [a       (cell 10)
          b       (formula inc a)
          c       (formula dec a)
          touch-b (atom false)
          touch-c (atom false)]
      (add-watch! b :b (fn [_ _ new] (core/reset! touch-b new)))
      (add-watch! c :c (fn [_ _ new] (core/reset! touch-c new)))
      (is (false? @touch-b))
      (is (false? @touch-c))

      (swap! a inc)
      (is (= 12 @touch-b))
      (is (= 10 @touch-c))))

  (testing "watches 4 - watches are independent"
    (let [a       (cell 10)
          b       (formula inc a)
          c       (cell 100)
          d       (formula dec c)
          touch-b (atom false)
          touch-d (atom false)]
      (add-watch! b :b (fn [_ _ new] (core/reset! touch-b new)))
      (add-watch! d :d (fn [_ _ new] (core/reset! touch-d new)))
      (is (false? @touch-b))
      (is (false? @touch-d))

      (swap! a inc)
      (is (= 12 @touch-b))
      (is (false? @touch-d))

      (swap! c inc)
      (is (= 12 @touch-b))
      (is (= 100 @touch-d))))

  (testing "watches 5 - multiple watches on the same cell"
    (let [a  (cell 10)
          b  (formula (partial * 2) a)
          w1 (atom 0)
          w2 (atom 0)
          w3 (atom 0)]
      (add-watch! b :w1 (fn [_ _ _] (core/swap! w1 inc)))
      (add-watch! b :w2 (fn [_ _ _] (core/swap! w2 inc)))
      (add-watch! b :w3 (fn [_ _ _] (core/swap! w3 inc)))
      (swap! a inc)
      (is (= 1 @w1))
      (is (= 1 @w2))
      (is (= 1 @w3))))

  (testing "watches 6 - multiple watches on the same cell with reset"
    (let [a  (cell 10)
          b  (formula (partial * 2) a)
          w1 (atom 0)
          w2 (atom 0)
          w3 (atom 0)]
      (add-watch! b :w1 (fn [_ _ _] (core/swap! w1 inc)))
      (add-watch! b :w2 (fn [_ _ _] (core/swap! w2 inc)))
      (add-watch! b :w3 (fn [_ _ _] (core/swap! w3 inc)))
      (reset! a 9000)
      (is (= 1 @w1))
      (is (= 1 @w2))
      (is (= 1 @w3))))

  (testing "watches 7 - propagation happens once for cells with watches"
    (let [log (atom 0)
          a   (cell 10)
          b   (formula (fn [x] (core/swap! log inc) (* 2 x)) a)]
      (reset! a 9000)
      (is (= 1 @log))))

  (testing "watches 8 - swap-secretly!"
    (let [a          (cell 10)
          swap-count (atom 0)]
      (add-watch! a :a (fn [_ _ new] (core/swap! swap-count inc)))
      (swap! a inc)
      (is (= 1 @swap-count))
      (swap-secretly! a inc)
      (is (= 1 @swap-count))

      (swap! a inc)
      (is (= 2 @swap-count))
      (swap-secretly! a inc)
      (is (= 2 @swap-count))

      (swap-secretly! a inc)
      (is (= 2 @swap-count))
      (swap-secretly! a inc)
      (is (= 2 @swap-count))

      (swap! a inc)
      (is (= 3 @swap-count))
      (swap! a inc)
      (is (= 4 @swap-count)))))
