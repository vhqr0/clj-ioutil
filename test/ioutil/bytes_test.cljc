(ns ioutil.bytes-test
  (:require [clojure.test :refer :all]
            [ioutil.bytes :as bytes]))

(deftest bytes-test
  (testing "seq"
    (are [x y] (= (mapv #(bit-and 0xff %) (bytes/bytes->seq (bytes/make-bytes x))) y)
      [] []
      [1 2 3] [1 2 3]
      [-3 -2 -1] [253 254 255]
      [253 254 255] [253 254 255]))
  (testing "hex"
    (are [x y] (= (bytes/bytes->hex (bytes/make-bytes x)) y)
      [] ""
      [0x00 0x00] "0000"
      [0x12 0x34 0x56] "123456"
      [0xab 0xcd 0xef] "abcdef")
    (are [x y] (= (mapv #(bit-and 0xff %) (bytes/bytes->seq (bytes/hex->bytes x))) y)
      "" []
      "0000" [0x00 0x00]
      "123456" [0x12 0x34 0x56]
      "abcdef" [0xab 0xcd 0xef]))
  (testing "int"
    (are [x y] (= (bytes/bytes->int (bytes/make-bytes x)) y)
      [] 0
      [255 255] 65535
      [255 254] 65534
      [0xab 0xcd 0xef] 0xabcdef)
    (are [x n y] (= (mapv #(bit-and 0xff %) (bytes/bytes->seq (bytes/int->bytes x n))) y)
      0 0 []
      65535 2 [255 255]
      65534 2 [255 254]
      0xabcdef 3 [0xab 0xcd 0xef]))
  (testing "sint"
    (are [x y] (= (bytes/bytes->sint (bytes/make-bytes x)) y)
      [] 0
      [255] -1
      [254] -2)
    (are [x n y] (= (mapv #(bit-and 0xff %) (bytes/bytes->seq (bytes/sint->bytes x n))) y)
      0 0 []
      -1 1 [255]
      -2 1 [254]))
  (testing "reader"
    (with-test
      (def reader (volatile! (bytes/make-reader (bytes/str->bytes "hello\nworld\n"))))
      (is (= (bytes/bytes->str (bytes/vupdate! reader bytes/read 2)) "he"))
      (is (= (bytes/bytes->str (bytes/vupdate! reader bytes/read-line "\n" true)) "llo\n"))
      (is (not (bytes/eof? @reader)))
      (is (= (bytes/bytes->str (bytes/vupdate! reader bytes/read-line "\n")) "world"))
      (is (bytes/eof? @reader))))
  (testing "writer"
    (with-test
      (def writer (volatile! (bytes/make-writer)))
      (is (= (do
               (vswap! writer bytes/write (bytes/str->bytes "hello\n"))
               (vswap! writer bytes/write (bytes/str->bytes "world\n"))
               (bytes/bytes->str (bytes/flush @writer)))
             "hello\nworld\n")))))
