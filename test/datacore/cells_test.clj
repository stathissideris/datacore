(ns datacore.cells-test
  (:refer-clojure :exclude [swap!])
  (:require [datacore.cells :refer :all]
            [clojure.test :refer :all]
            [clojure.core :as core]))

(deftest test-propagation
  (testing "one level"
    (let [a (cell 100)
          b (cell 2)
          c (cell= (* 2 @a @b))]
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
          c (cell= (* 2 @a @b))]
      (is (= 400 @c))
      (swap! a identity)
      (is (= 100 @a))
      (is (= 2 @b))
      (is (= 400 @c))))

  (testing "two levels"
    (let [a (cell 100)
          b (cell 2)
          c (cell= (* 2 @a @b))
          d (cell= (* 10 @c))]
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

  (testing "long chain"
    (let [chain (reduce (fn [chain _]
                          (conj chain (cell= (inc @(last chain)))))
                        [(cell 0)] (range 100))]
      (doall (map-indexed (fn [i c] (is (= i @c))) chain))
      (swap! (first chain) #(+ % 5))
      (doall (map-indexed (fn [i c] (is (= (+ i 5) @c))) chain))))

  (testing "long chain with eager propagation"
    (let [chain (reduce (fn [chain _]
                          (conj chain (cell= (inc @(last chain)))))
                        [(cell 0)] (range 100))
          touch (atom 0)
          chain (conj chain (cell= (core/swap! touch inc)
                                   @(last chain)))]
      @(last chain) ;; establish link
      (is (= 1 @touch))
      (swap! (first chain) #(+ % 5))
      (is (= 2 @touch))))

  (testing "eager propagation"
    (let [log (atom [])
          a   (cell 100)
          b   (cell=
               (core/swap! log conj :b)
               (+ @a 10))
          c   (cell=
               (core/swap! log conj :c)
               (+ @a 20))
          d   (cell=
               (core/swap! log conj :d)
               (+ @b @c))]
      @d ;;to establish link
      (is (= [:d :b :c] @log))
      (swap! a inc)
      (is (= [:d :b :c :b :c :d] @log)))))
