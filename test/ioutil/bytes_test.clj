(ns ioutil.bytes-test
  (:require [clojure.test :refer :all]
            [ioutil.bytes :as b]))

(deftest bytes-test
  (testing "sub"
    (are [x y] (= (vec (b/sub (b/bmake x))) y)
      [] []
      [1 2 3] [1 2 3])
    (are [x s y] (= (vec (b/sub (b/bmake x) s)) y)
      [1 2 3] 0 [1 2 3]
      [1 2 3] 1 [2 3]
      [1 2 3] 2 [3]
      [1 2 3] 3 [])
    (are [x s e y] (= (vec (b/sub (b/bmake x) s e)) y)
      [1 2 3] 1 3 [2 3]
      [1 2 3] 2 3 [3]
      [1 2 3] 3 3 []))
  (testing "concat"
    (are [x y] (= (vec (apply b/concat (map b/bmake x))) y)
      [] []
      [[]] []
      [[] []] []
      [[] [1]] [1]
      [[1] []] [1]
      [[1 2 3]] [1 2 3]
      [[1 2 3] [4 5 6]] [1 2 3 4 5 6]
      [[1 2 3] [4 5 6] [7 8 9]] [1 2 3 4 5 6 7 8 9]))
  (testing "compare"
    (are [p x y] (p (zero? (b/compare (b/bmake x) (b/bmake y))))
      true? [1 2 3] [1 2 3]
      false? [1 2 3] [2 3 4]
      false? [1 2] [1 2 3]
      false? [1 2 3 4] [1 2 3])
    (are [p x xs xe y ys ye] (p (zero? (b/compare (b/bmake x) xs xe (b/bmake y) ys ye)))
      true? [1 2 3] 0 3 [0 1 2 3 4] 1 4
      false? [1 2 3] 0 3 [0 1 2 3 4] 1 5
      false? [1 2 3] 1 3 [0 1 2 3 4] 1 3))
  (testing "index-of"
    (are [x i] (= (b/index-of (b/str->bytes x) (b/str->bytes "\r\n")) i)
      "hello" nil
      "hello\r\n" 5
      "hello\r\nworld" 5
      "hello\r\nworld\r\n" 5)
    (are [x i] (= (b/last-index-of (b/str->bytes x) (b/str->bytes "\r\n")) i)
      "hello" nil
      "hello\r\n" 5
      "hello\r\nworld" 5
      "hello\r\nworld\r\n" 12)
    (are [s i] (= (b/index-of (b/str->bytes "hello\r\nworld\r\n") (b/str->bytes "\r\n") s) i)
      5 5
      6 12
      12 12
      13 nil)
    (are [e i] (= (b/last-index-of (b/str->bytes "hello\r\nworld\r\n") (b/str->bytes "\r\n") 0 e) i)
      6 nil
      7 5
      13 5
      14 12))
  (testing "seq"
    (are [x y] (= (b/bseq-unsigned (b/bmake x)) y)
      [] []
      [1 2 3] [1 2 3]
      [-3 -2 -1] [253 254 255]
      [253 254 255] [253 254 255]))
  (testing "hex"
    (are [x y] (= (b/bytes->hex (b/bmake x)) y)
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
    (are [x y] (= (b/bytes->int (b/bmake x) :unsigned true) y)
      [255 255] 65535
      [255 254] 65534)
    (are [x n y] (= (b/bseq-unsigned (b/int->bytes x n :unsigned true)) y)
      65535 2 [255 255]
      65534 2 [255 254])))
