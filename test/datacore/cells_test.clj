(ns datacore.cells-test
  (:refer-clojure :exclude [swap! reset!])
  (:require [datacore.cells :refer :all]
            [clojure.test :refer :all]
            [clojure.core :as core]))

(def cycles? @#'datacore.cells/cycles?)
(deftest test-cycles?
  (is (nil?  (cycles? {:a #{:b :c}} :a)))
  (is (true? (cycles? {:a #{:b :c} :b #{:a :z}} :a)))
  (is (true? (cycles? {:a #{:b :c} :b #{:d} :d #{:e} :e #{:a}} :a)))
  (is (true? (cycles? {:a #{:b :c} :b #{:d} :d #{:e} :e #{:b}} :a)))
  (is (true? (cycles? {:a #{:b :c} :b #{:d} :d #{:e} :e #{:b}} :d)))
  (is (nil?  (cycles? {:a #{:b :c} :b #{:d} :d #{:e} :e #{:f}} :a))))

(deftest test-construction
  (let [log (atom [])
        a   (cell 100)
        b   (cell 2)
        c   (formula
             (fn [b]
               (formula
                (fn [b] (core/swap! log conj :foo) (prn @b))
                b)
               (* 100 @b))
             b)]
    ))

(deftest test-propagation
  (testing "one level"
    (let [a (cell 100)
          b (cell 2)
          c (formula (partial * 2) a b)]
      (is (= 400 @c))

      (testing "- 1"
        (swap! a inc)
        (is (= 101 @a))
        (is (= 2 @b))
        (is (= (* 101 2 2) @c)))

      (testing "- 2"
        (swap! b inc)
        (is (= 101 @a))
        (is (= 3 @b))
        (is (= (* 101 3 2) @c)))))

  (testing "one level - no change"
    (let [a (cell 100)
          b (cell 2)
          c (formula (partial * 2) a b)]
      (is (= 400 @c))
      (swap! a identity)
      (is (= 100 @a))
      (is (= 2 @b))
      (is (= 400 @c))))

  (testing "two levels"
    (let [a (cell 100)
          b (cell 2)
          c (formula (partial * 2) a b)
          d (formula (partial * 10) c)]
      (is (= 400 @c))
      (is (= 4000 @d))

      (testing "- 1"
        (swap! a inc)
        (is (= 101 @a))
        (is (= 2 @b))
        (is (= (* 101 2 2) @c))
        (is (= (* 101 2 2 10) @d)))

      (testing "- 2"
        (swap! b inc)
        (is (= 101 @a))
        (is (= 3 @b))
        (is (= (* 101 3 2) @c))
        (is (= (* 101 3 2 10) @d)))))

  (testing "long chain 1"
    (let [chain (reduce (fn [chain _]
                          (conj chain (formula inc (last chain))))
                        [(cell 0)] (range 100))]
      (swap! (first chain) #(+ % 5))
      (doall (map-indexed (fn [i c] (is (= (+ i 5) @c))) chain))))

  (testing "long chain 2"
    (let [chain (reduce (fn [chain _]
                          (conj chain (formula inc (last chain))))
                        [(cell 0)] (range 100))
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
                       {:label :b}
                       a)
          c   (formula (fn [x]
                         (core/swap! log conj :c)
                         (+ x 20))
                       {:label :c}
                       a)
          d   (formula (fn [b c]
                         (core/swap! log conj :d)
                         (+ b c))
                       {:label :d}
                       b c)]
      (swap! a inc)
      (is (= [:b :c :d] @log))))

  (testing "muting"
    (let [a (cell :a 100)
          b (formula (partial * 10) a)
          c (formula (partial + 1) b)]
      (is (= 1001 @c))
      (mute! b)
      (is (= 101 @c))
      (unmute! b)
      (is (= 1001 @c))))

  (testing "destroying"
    (let [a (cell :a 100)
          b (formula (partial * 10) a)
          c (formula (partial + 1) b)]
      (destroy! b)
      (is (= :datacore.cells/destroyed @b))
      (is (= :datacore.cells/destroyed @c)))))

(comment
  (do
    (def a (cell :a 100))
    (def b (formula (partial * 10) a))
    (def c (formula (partial + 1) b))))
