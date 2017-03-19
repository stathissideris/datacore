(ns datacore.util-test
  (:require [datacore.util :refer :all]
            [clojure.test :refer :all]))

(deftest take-exactly-test
  (is (= [1 2] (take-exactly 2 [1 2 3] :x)))
  (is (= [1 2 3] (take-exactly 3 [1 2 3] :x)))
  (is (= [1 2 3 :x :x :x] (take-exactly 6 [1 2 3] :x))))

(deftest diff-test
  (is (= [[:delete 0]
          [:same 1]
          [:same 2]
          [:same 3]
          [:add :a]
          [:add :b]
          [:add :c]
          [:same 4]
          [:same 5]
          [:same 6]
          [:delete 7]
          [:delete 8]]
         (seq-diff [0 1 2 3 4 5 6 7 8] [1 2 3 :a :b :c 4 5 6])))
  (is (= [[:delete 0]
          [:same 1]
          [:same 2]
          [:same 3]
          [:edit :e :a]
          [:edit :f :b]
          [:edit :g :c]
          [:same 4]
          [:same 5]
          [:same 6]
          [:delete 7]
          [:delete 8]]
         (seq-diff [0 1 2 3 :e :f :g 4 5 6 7 8] [1 2 3 :a :b :c 4 5 6])))
  (is (= [[:delete 0]
          [:same 1]
          [:same 2]
          [:same 3]
          [:edit :e :a]
          [:edit :f :b]
          [:edit :g :c]
          [:same 4]
          [:same 5]
          [:add 10]
          [:same 6]
          [:delete 7]
          [:delete 8]]
         (seq-diff [0 1 2 3 :e :f :g 4 5 6 7 8] [1 2 3 :a :b :c 4 5 10 6]))))
