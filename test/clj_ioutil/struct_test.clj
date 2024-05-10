(ns clj-ioutil.struct-test
  (:require [clojure.test :refer :all]
            [clj-ioutil.bytes :as b]
            [clj-ioutil.struct :as st]))

(deftest struct-test
  (testing "bytes"
    (with-test
      (def short1 [:bytes 2 :spec st/u16 :many true])
      (are [x y] (= (b/bseq-unsigned @(st/struct->bytes [x] short1)) y)
        255 [0 255]
        256 [1 0]
        65535 [255 255]
        65534 [255 254])
      (are [x y] (= @(st/bytes->struct (b/bmake x) short1) [y])
        [0 255] 255
        [1 0] 256
        [255 255] 65535
        [255 254] 65534))
    (with-test
      (def bytes2 [:bytes 2 :spec st/u8 :many true])
      (are [x y] (= (b/bseq-unsigned @(st/struct->bytes x bytes2)) y)
        [0 255] [0 255]
        [1 0] [1 0]
        [255 255] [255 255]
        [255 254] [255 254])
      (are [x y] (= @(st/bytes->struct (b/bmake x) bytes2) y)
        [0 255] [0 255]
        [1 0] [1 0]
        [255 255] [255 255]
        [255 254] [255 254])))
  (testing "int"
    (with-test
      (def uint16 st/u16)
      (are [x y] (= (b/bseq-unsigned @(st/struct->bytes x uint16)) y)
        255 [0 255]
        256 [1 0]
        65535 [255 255]
        65534 [255 254])
      (are [x y] (= @(st/bytes->struct (b/bmake x) uint16) y)
        [0 255] 255
        [1 0] 256
        [255 255] 65535
        [255 254] 65534)))
  (testing "bits"
    (with-test
      (def bits224 (st/make-bits-struct [2 2 4]))
      (are [x y] (= (b/bseq-unsigned @(st/struct->bytes x bits224)) y)
        [0x2 0x1 0x6] [0x96]
        [0x1 0x2 0x9] [0x69])
      (are [x y] (= @(st/bytes->struct (b/bmake x) bits224) y)
        [0x96] [0x2 0x1 0x6]
        [0x69] [0x1 0x2 0x9])))
  (testing "line"
    (with-test
      (def crlf-line st/crlf-line)
      (are [x y] (= (b/bytes->str @(st/struct->bytes x crlf-line)) y)
        "" "\r\n"
        "hello" "hello\r\n")
      (are [x y] (= @(st/bytes->struct (b/str->bytes x) crlf-line) y)
        "\r\n" ""
        "hello\r\n" "hello"))))
