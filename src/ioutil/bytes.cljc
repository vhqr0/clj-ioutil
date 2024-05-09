(ns ioutil.bytes
  (:refer-clojure :exclude [rand-int concat compare read read-line flush -peek -flush -write])
  (:require [ioutil.bytes.impl :as impl]
            [promesa.core :as p]
            [promesa.exec.csp :as csp]))

;;; impl

(def btype
  "Type of bytes."
  impl/btype)

(def bmake
  "[num-or-seq]
  Make bytes, `byte-array` like."
  impl/bmake)

(def blength
  "[b]
  Get length of bytes, `alength` like."
  impl/blength)

(def bempty?
  "[b]
  Test empty of bytes, `empty` like."
  impl/bempty?)

;;;; getter as int8 or uint8

(def bget
  "[b i]
  Get byte (int8 or uint8, depend on platform) at index of bytes, `aget` like."
  impl/bget)

(def bseq
  "[b]
  Get byte (int8 or uint8, depend on platform) seq of bytes, `seq`like."
  impl/bseq)

(def brseq
  "[b]
  get byte (int8 or uint8, depend on platform) reversed seq of bytes, `rseq` like."
  impl/brseq)

;;;; getter as uint8

(def bget-unsigned
  "[b i]
  Like `bget`, force uint8."
  impl/bget-unsigned)

(def bseq-unsigned
  "[b]
  Like `bseq`, force uint8."
  impl/bseq-unsigned)

(def brseq-unsigned
  "[b]
  Like `brseq`, force uint8."
  impl/brseq-unsigned)

;;;; array like

(def sub
  "[b]
  [b s]
  [b s e]
  Sub bytes."
  impl/sub)

(def concat
  "[& bs]
  Concat seq of bytes or [bytes [start [end]]]."
  impl/concat)

(def compare
  "[x y]
  [x xs y ys]
  [x xs xe y ys ye]
  Compare bytes."
  impl/compare)

(def equals?
  "[x y]
  [x xs y ys]
  [x xs xe y ys ye]
  Test equals of bytes."
  impl/equals?)

(def index-of
  "[h n]
  [h n s]
  [h n s e]
  Find needle in haystack."
  impl/index-of)

(def last-index-of
  "Reverse version of `index-of`."
  impl/last-index-of)

;;;; codec

(def bytes->hex
  "[b]
  Convert bytes to hex string."
  impl/bytes->hex)

(def hex->bytes
  "[s]
  Parse hex string."
  impl/hex->bytes)

(def bytes->base64
  "[b]
  [b alphabet]
  Convert bytes to base64 string. The optional arg specify a base64
  alphabet in keyword, and atleast support :default and :urlsafe."
  impl/bytes->base64)

(def base64->bytes
  "[s]
  [s alphabet]
  Parse base64 string. The optional args see `bytes->base64`."
  impl/base64->bytes)

;;;; str utils

(def bytes->str
  "[b]
  [b encoding]
  Decode bytes to string. The optional arg specify encoding of string."
  impl/bytes->str)

(def str->bytes
  "[s]
  Encode string to bytes. The optional arg see `bytes->str`."
  impl/str->bytes)

(def str->int
  "[s & {:keys [radix unsigned]}]
  Parse int string."
  impl/str->int)

(def str->float
  "[s]
  Parse float string."
  impl/str->float)

(def str->uuid
  "[s]
  Parse uuid string."
  impl/str->uuid)

;;;; num utils

(def bytes->int
  "[b & {:keys [little unsigned]}]
  Convert bytes to int."
  impl/bytes->int)

(def int->bytes
  "[i n & {:keys [little unsigned]}]
  Convert n bytes int to bytes."
  impl/int->bytes)

(def bytes->float
  "[b & {:keys [little]}]
  Convert bytes to float."
  impl/bytes->float)

(def float->bytes
  "[f n & {:keys [little]}]
  Convert n bytes float to bytes."
  impl/float->bytes)

(def bytes->uuid
  "[b]
  Convert bytes to uuid."
  impl/bytes->uuid)

(def uuid->bytes
  "[u]
  Convert uuid to bytes."
  impl/uuid->bytes)

;;;; rand

(def rand-bytes
  "[n]
  Get random n bytes."
  impl/rand-bytes)

(def rand-int
  "[n & {:keys [unsigned]}]
  Get random n bytes int."
  impl/rand-int)

(def rand-float
  "[n]
  Get random n bytes float."
  impl/rand-float)

(def rand-uuid
  "[]
  Get random uuid."
  impl/rand-uuid)

;;;; bits utils

(def int->bits
  "[i offsets masks]
  Split bits from int."
  impl/int->bits)

(def bits->int
  "[bits offsets]
  Join bits to int."
  impl/bits->int)

(def bytes->bits
  "[b offsets masks]
  Split bits from bytes."
  impl/bytes->bits)

(def bits->bytes
  "[bits offsets n]
  Join bits to bytes."
  impl/bits->bytes)

;;; io protocols

(defprotocol IDetachable
  (-detach [this]
    "Detach low level resource from reader/writer/stream."))

(defprotocol ICloseable
  (-close [this]
    "Close all related resources of reader/writer/stream."))

(defprotocol IBytesReader
  (-peek [this]
    "Peek buffer of reader, return (data, pos).")
  (-seek [this pos]
    "Reset pos of buffer, return new reader.")
  (-peek-more [this]
    "Peek more bytes than last peek from reader, return p/let-able
    new reader, and (data, pos) or nil if no more bytes to peek."))

(defprotocol IBytesWriter
  (-shutdown [this]
    "Shutdown writing, return p/let-able new writer.")
  (-flush [this]
    "Flush buffered bytes, return p/let-able new writer.")
  (-write [this b]
    "Write bytes to buffer, return p/let-able new writer."))

;;; io utils

(defn want-read-error []
  (ex-info "want read error" {:type ::want-read}))

(defn want-write-error []
  (ex-info "want write error" {:type ::want-write}))

(defn detach [detachable]
  (-detach detachable))

(defn close [closeable]
  (-close closeable))

(def ^:dynamic *peek-threshold* 65536)

(defn peek-until
  ([reader pred] (peek-until reader pred *peek-threshold*))
  ([reader pred threshold]
   (p/loop [r [reader (-peek reader)]]
     (let [[reader r] r]
       (if r
         (let [[data pos] r]
           (if-let [r (pred data pos)]
             [reader r]
             (p/recur
              (if (< (- (blength data) pos) threshold)
                (-peek-more reader)
                (p/rejected (want-write-error))))))
         (p/rejected (want-read-error)))))))

(defn read
  ([reader]
   (let [[data pos] (-peek reader)]
     (if (< pos (blength data))
       [(-seek reader (blength data)) (sub data pos)]
       (p/let [[reader r] (-peek-more reader)]
         (if-let [[data pos] r]
           [(-seek reader (blength data)) (sub data pos)]
           [reader nil])))))
  ([reader n]
   (letfn [(pred [data pos]
             (let [i (+ pos n)]
               (when (<= i (blength data))
                 [data pos i])))]
     (p/let [[reader [data pos i]] (peek-until reader pred)]
       [(-seek reader i) (sub data pos i)]))))

(defn read-all [reader]
  (p/loop [reader reader bs []]
    (p/let [[reader b] (read reader)]
      (if-not b
        [reader (apply concat bs)]
        (p/recur reader (conj bs b))))))

(defn read-eof [reader]
  (let [[data pos] (-peek reader)]
    (if (< pos (blength data))
      [reader false]
      (p/let [[reader r] (-peek-more reader)]
        [reader (nil? r)]))))

(defn read-line
  [reader & {:keys [end encoding] :or {end "\r\n"}}]
  (let [end (if-not (string? end)
              end
              (if-not encoding
                (str->bytes end)
                (str->bytes end encoding)))]
    (letfn [(pred [data pos]
              (when-let [i (index-of data end pos)]
                [data pos i]))]
      (p/let [[reader [data pos i]] (peek-until reader pred)]
        [(-seek reader (+ i (blength end)))
         (let [b (sub data pos i)]
           (if-not encoding
             (bytes->str b)
             (bytes->str b encoding)))]))))

(defn shutdown [writer]
  (-shutdown writer))

(defn flush [writer]
  (-flush writer))

(defn write [writer b]
  (if (bempty? b)
    writer
    (-write writer b)))

(defn write-line
  [writer line & {:keys [end encoding] :or {end "\r\n"}}]
  (let [line (str line end)
        b (if-not encoding
            (str->bytes line)
            (str->bytes line encoding))]
    (write writer b)))

;;; bytes io

(comment
  (extend-type #?(:clj (Class/forName "[B")
                  :cljs js/ArrayBuffer)
    IDetachable
    (-detach [this]
      this)
    IBytesReader
    (-peek [this]
      [this 0])
    (-seek [this pos]
      (sub this pos))
    (-peek-more [this]
      [this nil])
    IBytesWriter
    (-shutdown [this]
      this)
    (-flush [this]
      this)
    (-write [this b]
      (concat this b))))

;;; buffered bytes io

(defrecord reader [data pos])

(extend-type reader
  IDetachable
  (-detach [this]
    (let [{:keys [data pos]} this]
      (sub data pos)))
  IBytesReader
  (-peek [this]
    (let [{:keys [data pos]} this]
      [data pos]))
  (-seek [this pos]
    (->reader (:data this) pos))
  (-peek-more [this]
    [this nil]))

(defrecord writer [data ring])

(extend-type writer
  IDetachable
  (-detach [this]
    (:data this))
  IBytesWriter
  (-shutdown [this]
    (->writer (:data this) nil))
  (-flush [this]
    (let [{:keys [data ring]} this]
      (if (or (not ring) (empty? ring))
        this
        (->writer (apply concat data ring) []))))
  (-write [this b]
    (let [{:keys [data ring]} this]
      (assert ring)
      (->writer data (conj ring b)))))

(defn make-reader
  ([] (make-reader (bmake 0)))
  ([data] (->reader data 0)))

(defn make-writer
  ([] (make-writer (bmake 0)))
  ([data] (->writer data [])))

;;; chan io

(defrecord chan-reader [data pos chan])

(extend-type chan-reader
  IDetachable
  (-detach [this]
    (:chan this))
  ICloseable
  (-close [this]
    (csp/close! (:chan this)))
  IBytesReader
  (-peek [this]
    (let [{:keys [data pos]} this]
      [data pos]))
  (-seek [this pos]
    (let [{:keys [data chan]} this]
      (->chan-reader data pos chan)))
  (-peek-more [this]
    (let [{:keys [data pos chan]} this]
      (p/let [b (csp/take chan)]
        (if-not b
          [this nil]
          (let [data (concat [data pos] b)]
            [(->chan-reader data 0 chan) [data 0]]))))))

(defrecord chan-writer [chan ring])

(extend-type chan-writer
  IDetachable
  (-detach [this]
    (:chan this))
  ICloseable
  (-close [this]
    (csp/close! (:chan this)))
  IBytesWriter
  (-shutdown [this]
    (let [{:keys [chan]} this]
      (csp/close! chan)
      (->chan-writer chan nil)))
  (-flush [this]
    (let [{:keys [chan ring]} this]
      (if (or (not ring) (empty? ring))
        this
        (let [b (apply concat ring)]
          (if (bempty? b)
            this
            (p/let [ok (csp/put chan b)]
              (if ok
                (->chan-writer chan [])
                (p/rejected (want-write-error)))))))))
  (-write [this b]
    (let [{:keys [chan ring]} this]
      (assert ring)
      (->chan-writer chan (conj ring b)))))

(defn make-chan-reader [chan]
  (->chan-reader (bmake 0) 0 chan))

(defn make-chan-writer [chan]
  (->chan-writer chan []))

;;; stream

(defrecord stream [resource close-chan in-chan out-chan]
  IDetachable
  (-detach [this]
    (:resource this))
  ICloseable
  (-close [this]
    (csp/close! (:close-chan this))))

(defrecord error-stream [resource close-chan in-chan out-chan err-chan]
  IDetachable
  (-detach [this]
    (:resource this))
  ICloseable
  (-close [this]
    (csp/close! (:close-chan this))))

(defn make-stream-reader [stream]
  (make-chan-reader (:in-chan stream)))

(defn make-stream-writer [stream]
  (make-chan-writer (:out-chan stream)))

(defn make-stream-error-reader [stream]
  (make-chan-reader (:err-chan stream)))
