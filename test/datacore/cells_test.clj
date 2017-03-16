(ns datacore.cells-test
  (:refer-clojure :exclude [swap! reset!])
  (:require [datacore.cells :refer :all]
            [clojure.test :refer :all]
            [clojure.core :as core]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]))

(def global-cells @#'datacore.cells/global-cells)
(core/swap! global-cells (fn [_] (make-cells)))

(doseq [s (stest/instrumentable-syms)]
  (stest/instrument s))

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
      (is (= :datacore.cells/no-value (value c)))
      (is (= :datacore.cells/unlinked (-> @global-cells :cells (get c) :sources-list first)))))

  (testing "destroy 2 - destruction is propagated"
    ;; can't test by looking putting side-effects in e's function,
    ;; because the function is not called one of the sources has no
    ;; value, value is set directly to ::no-value. We'd need effect
    ;; cells for that. Or should we be calling the function anyway?
    (let [a   (cell :a 100)
          b   (formula (partial * 10) a)
          c   (formula (partial + 1) b)
          d   (formula (partial + 2) c)
          e   (formula (partial + 20) d)]
      (destroy! b)
      (is (= :datacore.cells/no-value (-> @global-cells :cells (get e) :value)))
      (is (= :datacore.cells/destroyed (value b)))
      (is (= :datacore.cells/no-value (value c)))
      (is (= :datacore.cells/unlinked (-> @global-cells :cells (get c) :sources-list first)))
      (is (= :datacore.cells/no-value (value d)))
      (is (= :datacore.cells/no-value (value e)))))

  (testing "destroy 3"
    (let [a (cell :a 100)
          b (formula (partial * 100) a)
          c (formula (partial * 10) a)
          d (formula (partial + 1) b c)]
      (destroy! c)
      (is (= 100 (value a)))
      (is (= 10000 (value b)))
      (is (= :datacore.cells/destroyed (value c)))
      (is (= :datacore.cells/no-value (value d)))
      (is (= b (-> @global-cells :cells (get d) :sources-list first)))
      (is (= :datacore.cells/unlinked (-> @global-cells :cells (get d) :sources-list second))))))
