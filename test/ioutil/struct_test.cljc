(ns ioutil.struct-test
  (:require [clojure.test :refer :all]
            [ioutil.bytes :as bytes]
            [ioutil.struct :as struct]))

(deftest struct-test
  (testing "bytes"
    (with-test
      (def short1 (struct/make-struct [:bytes :len 2 :spec [:int :len 2]]))
      (are [x y] (= (mapv #(bit-and 0xff %) (bytes/bytes->seq (struct/struct->bytes short1 x))) y)
        255 [0 255]
        256 [1 0]
        65535 [255 255]
        65534 [255 254])
      (are [x y] (= (struct/bytes->struct short1 (bytes/make-bytes x)) y)
        [0 255] 255
        [1 0] 256
        [255 255] 65535
        [255 254] 65534))
    (with-test
      (def bytes2 (struct/make-struct [:bytes :len 2 :spec [:int :len 1] :many true]))
      (are [x y] (= (mapv #(bit-and 0xff %) (bytes/bytes->seq (struct/struct->bytes bytes2 x))) y)
        [0 255] [0 255]
        [1 0] [1 0]
        [255 255] [255 255]
        [255 254] [255 254])
      (are [x y] (= (struct/bytes->struct bytes2 (bytes/make-bytes x)) y)
        [0 255] [0 255]
        [1 0] [1 0]
        [255 255] [255 255]
        [255 254] [255 254])))
  (testing "int"
    (with-test
      (def uint16 (struct/make-struct [:int :len 2]))
      (are [x y] (= (mapv #(bit-and 0xff %) (bytes/bytes->seq (struct/struct->bytes uint16 x))) y)
        255 [0 255]
        256 [1 0]
        65535 [255 255]
        65534 [255 254])
      (are [x y] (= (struct/bytes->struct uint16 (bytes/make-bytes x)) y)
        [0 255] 255
        [1 0] 256
        [255 255] 65535
        [255 254] 65534)))
  (testing "bits"
    (with-test
      (def bits224 (struct/make-struct [:bits :lens [2 2 4]]))
      (are [x y] (= (mapv #(bit-and 0xff %) (bytes/bytes->seq (struct/struct->bytes bits224 x))) y)
        [0x2 0x1 0x6] [0x96]
        [0x1 0x2 0x9] [0x69])
      (are [x y] (= (struct/bytes->struct bits224 (bytes/make-bytes x)) y)
        [0x96] [0x2 0x1 0x6]
        [0x69] [0x1 0x2 0x9])))
  (testing "line"
    (with-test
      (def crlf-line (struct/make-struct [:line :end "\r\n"]))
      (are [x y] (= (bytes/bytes->str (struct/struct->bytes crlf-line x)) y)
        "" "\r\n"
        "hello" "hello\r\n")
      (are [x y] (= (struct/bytes->struct crlf-line (bytes/str->bytes x)) y)
        "\r\n" ""
        "hello\r\n" "hello"))))
