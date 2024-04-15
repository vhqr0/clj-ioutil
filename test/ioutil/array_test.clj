(ns ioutil.array-test
  (:require [clojure.test :refer :all]
            [ioutil.array :as array]
            [ioutil.bytes :as bytes]))

(deftest array-test
  (testing "copy"
    (are [x y] (= (let [a (byte-array 3)] (array/copy (byte-array x) a) (vec a)) y)
      [1 2 3] [1 2 3]
      [1 2] [1 2 0]
      [1 2 3 4] [1 2 3])
    (are [x xs y] (= (let [a (byte-array 3)] (array/copy (byte-array x) xs a 0) (vec a)) y)
      [1 2 3] 1 [2 3 0]
      [1 2 3 4] 1 [2 3 4]
      [1 2 3 4] 0 [1 2 3])
    (are [x xs n y] (= (let [a (byte-array 3)] (array/copy (byte-array x) xs a 0 n) (vec a)) y)
      [1 2 3] 0 2 [1 2 0]
      [1 2 3] 1 2 [2 3 0])
    (are [x y] (= (let [xx (byte-array x) a (byte-array [1 2 3])] (array/copy a xx) (vec xx)) y)
      0 []
      1 [1]
      2 [1 2]
      3 [1 2 3]
      4 [1 2 3 0])
    (are [x xs y] (= (let [xx (byte-array x) a (byte-array [1 2 3])] (array/copy a 0 xx xs) (vec xx)) y)
      1 1 [0]
      2 1 [0 1]
      3 1 [0 1 2]
      4 1 [0 1 2 3]
      5 1 [0 1 2 3 0]))
  (testing "fill"
    (are [v x] (= (vec (array/fill (byte-array 3) (byte v))) x)
      1 [1 1 1]
      2 [2 2 2])
    (are [s v x] (= (vec (array/fill (byte-array 3) s (byte v))) x)
      0 1 [1 1 1]
      1 1 [0 1 1]
      2 1 [0 0 1]
      3 1 [0 0 0]))
  (testing "sub"
    (are [x y] (= (vec (array/sub (byte-array x))) y)
      [] []
      [1 2 3] [1 2 3])
    (are [x s y] (= (vec (array/sub (byte-array x) s)) y)
      [1 2 3] 0 [1 2 3]
      [1 2 3] 1 [2 3]
      [1 2 3] 2 [3]
      [1 2 3] 3 [])
    (are [x s e y] (= (vec (array/sub (byte-array x) s e)) y)
      [1 2 3] 1 3 [2 3]
      [1 2 3] 2 3 [3]
      [1 2 3] 3 3 []))
  (testing "concat"
    (are [x y] (= (vec (apply array/concat (map byte-array x))) y)
      [] []
      [[]] []
      [[] []] []
      [[] [1]] [1]
      [[1] []] [1]
      [[1 2 3]] [1 2 3]
      [[1 2 3] [4 5 6]] [1 2 3 4 5 6]
      [[1 2 3] [4 5 6] [7 8 9]] [1 2 3 4 5 6 7 8 9]))
  (testing "compare"
    (are [p x y] (p (zero? (array/compare (byte-array x) (byte-array y))))
      true? [1 2 3] [1 2 3]
      false? [1 2 3] [2 3 4]
      false? [1 2] [1 2 3]
      false? [1 2 3 4] [1 2 3])
    (are [p x xs xe y ys ye] (p (zero? (array/compare (byte-array x) xs xe (byte-array y) ys ye)))
      true? [1 2 3] 0 3 [0 1 2 3 4] 1 4
      false? [1 2 3] 0 3 [0 1 2 3 4] 1 5
      false? [1 2 3] 1 3 [0 1 2 3 4] 1 3))
  (testing "index-of"
    (are [x i] (= (array/index-of (bytes/str->bytes x) (bytes/str->bytes "\r\n")) i)
      "hello" -1
      "hello\r\n" 5
      "hello\r\nworld" 5
      "hello\r\nworld\r\n" 5)
    (are [x i] (= (array/last-index-of (bytes/str->bytes x) (bytes/str->bytes "\r\n")) i)
      "hello" -1
      "hello\r\n" 5
      "hello\r\nworld" 5
      "hello\r\nworld\r\n" 12)
    (are [s i] (= (array/index-of (bytes/str->bytes "hello\r\nworld\r\n") (bytes/str->bytes "\r\n") s) i)
      5 5
      6 12
      12 12
      13 -1)
    (are [e i] (= (array/last-index-of (bytes/str->bytes "hello\r\nworld\r\n") (bytes/str->bytes "\r\n") 0 e) i)
      6 -1
      7 5
      13 5
      14 12)))
