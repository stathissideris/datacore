(ns datacore.cells-test
  (:refer-clojure :exclude [swap!])
  (:require [datacore.cells :refer :all]
            [clojure.test :refer :all]))

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
                          (let [prev (last chain)]
                            (conj chain (cell= (inc @prev)))))
                        [(cell 0)] (range 100))]
      (doall (map-indexed (fn [i c] (is (= i @c))) chain))
      (swap! (first chain) #(+ % 5))
      (doall (map-indexed (fn [i c] (is (= (+ i 5) @c))) chain)))))
