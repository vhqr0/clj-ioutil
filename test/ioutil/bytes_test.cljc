(ns ioutil.bytes-test
  (:require [clojure.test :refer :all]
            [ioutil.clj.array :as arr]
            [ioutil.bytes :as b]))

(deftest array-test
  (testing "copy"
    (are [x y] (= (let [a (byte-array 3)] (arr/copy (byte-array x) a) (vec a)) y)
      [1 2 3] [1 2 3]
      [1 2] [1 2 0]
      [1 2 3 4] [1 2 3])
    (are [x xs y] (= (let [a (byte-array 3)] (arr/copy (byte-array x) xs a 0) (vec a)) y)
      [1 2 3] 1 [2 3 0]
      [1 2 3 4] 1 [2 3 4]
      [1 2 3 4] 0 [1 2 3])
    (are [x xs n y] (= (let [a (byte-array 3)] (arr/copy (byte-array x) xs a 0 n) (vec a)) y)
      [1 2 3] 0 2 [1 2 0]
      [1 2 3] 1 2 [2 3 0])
    (are [x y] (= (let [xx (byte-array x) a (byte-array [1 2 3])] (arr/copy a xx) (vec xx)) y)
      0 []
      1 [1]
      2 [1 2]
      3 [1 2 3]
      4 [1 2 3 0])
    (are [x xs y] (= (let [xx (byte-array x) a (byte-array [1 2 3])] (arr/copy a 0 xx xs) (vec xx)) y)
      1 1 [0]
      2 1 [0 1]
      3 1 [0 1 2]
      4 1 [0 1 2 3]
      5 1 [0 1 2 3 0]))
  (testing "fill"
    (are [v x] (= (vec (arr/fill (byte-array 3) (byte v))) x)
      1 [1 1 1]
      2 [2 2 2])
    (are [s v x] (= (vec (arr/fill (byte-array 3) s (byte v))) x)
      0 1 [1 1 1]
      1 1 [0 1 1]
      2 1 [0 0 1]
      3 1 [0 0 0]))
  (testing "sub"
    (are [x y] (= (vec (arr/sub (byte-array x))) y)
      [] []
      [1 2 3] [1 2 3])
    (are [x s y] (= (vec (arr/sub (byte-array x) s)) y)
      [1 2 3] 0 [1 2 3]
      [1 2 3] 1 [2 3]
      [1 2 3] 2 [3]
      [1 2 3] 3 [])
    (are [x s e y] (= (vec (arr/sub (byte-array x) s e)) y)
      [1 2 3] 1 3 [2 3]
      [1 2 3] 2 3 [3]
      [1 2 3] 3 3 []))
  (testing "concat"
    (are [x y] (= (vec (apply arr/concat (map byte-array x))) y)
      [] []
      [[]] []
      [[] []] []
      [[] [1]] [1]
      [[1] []] [1]
      [[1 2 3]] [1 2 3]
      [[1 2 3] [4 5 6]] [1 2 3 4 5 6]
      [[1 2 3] [4 5 6] [7 8 9]] [1 2 3 4 5 6 7 8 9]))
  (testing "compare"
    (are [p x y] (p (zero? (arr/compare (byte-array x) (byte-array y))))
      true? [1 2 3] [1 2 3]
      false? [1 2 3] [2 3 4]
      false? [1 2] [1 2 3]
      false? [1 2 3 4] [1 2 3])
    (are [p x xs xe y ys ye] (p (zero? (arr/compare (byte-array x) xs xe (byte-array y) ys ye)))
      true? [1 2 3] 0 3 [0 1 2 3 4] 1 4
      false? [1 2 3] 0 3 [0 1 2 3 4] 1 5
      false? [1 2 3] 1 3 [0 1 2 3 4] 1 3))
  (testing "index-of"
    (are [x i] (= (arr/index-of (b/str->bytes x) (b/str->bytes "\r\n")) i)
      "hello" nil
      "hello\r\n" 5
      "hello\r\nworld" 5
      "hello\r\nworld\r\n" 5)
    (are [x i] (= (arr/last-index-of (b/str->bytes x) (b/str->bytes "\r\n")) i)
      "hello" nil
      "hello\r\n" 5
      "hello\r\nworld" 5
      "hello\r\nworld\r\n" 12)
    (are [s i] (= (arr/index-of (b/str->bytes "hello\r\nworld\r\n") (b/str->bytes "\r\n") s) i)
      5 5
      6 12
      12 12
      13 nil)
    (are [e i] (= (arr/last-index-of (b/str->bytes "hello\r\nworld\r\n") (b/str->bytes "\r\n") 0 e) i)
      6 nil
      7 5
      13 5
      14 12)))

(deftest bytes-test
  (testing "seq"
    (are [x y] (= (b/bseq-unsigned (b/make-bytes x)) y)
      [] []
      [1 2 3] [1 2 3]
      [-3 -2 -1] [253 254 255]
      [253 254 255] [253 254 255]))
  (testing "hex"
    (are [x y] (= (b/bytes->hex (b/make-bytes x)) y)
      [] ""
      [0x00 0x00] "0000"
      [0x12 0x34 0x56] "123456"
      [0xab 0xcd 0xef] "abcdef")
    (are [x y] (= (b/bseq-unsigned (b/hex->bytes x)) y)
      "" []
      "0000" [0x00 0x00]
      "123456" [0x12 0x34 0x56]
      "abcdef" [0xab 0xcd 0xef]))
  (testing "int"
    (are [x y] (= (b/bytes->int (b/make-bytes x) :unsigned true) y)
      [255 255] 65535
      [255 254] 65534)
    (are [x n y] (= (b/bseq-unsigned (b/int->bytes x n :unsigned true)) y)
      65535 2 [255 255]
      65534 2 [255 254])))
