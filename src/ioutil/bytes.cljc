(ns ioutil.bytes
  (:refer-clojure :exclude [rand-int concat compare read read-line])
  (:require [ioutil.bytes.impl :as impl]
            [promesa.core :as p]
            [promesa.exec.csp :as csp]))

;;; impl

(def bmake impl/bmake)
(def btype impl/btype)
(def blength impl/blength)
(def bempty? impl/bempty?)
(def bget impl/bget)
(def bseq impl/bseq)
(def brseq impl/brseq)
(def bget-unsigned impl/bget-unsigned)
(def bseq-unsigned impl/bseq-unsigned)
(def brseq-unsigned impl/brseq-unsigned)
(def sub impl/sub)
(def concat impl/concat)
(def equals? impl/equals?)
(def compare impl/compare)
(def index-of impl/index-of)
(def last-index-of impl/last-index-of)
(def rand-bytes impl/rand-bytes)
(def rand-int impl/rand-int)
(def rand-float impl/rand-float)
(def rand-uuid impl/rand-uuid)
(def bytes->str impl/bytes->str)
(def str->bytes impl/str->bytes)
(def str->int impl/str->int)
(def str->float impl/str->float)
(def str->uuid impl/str->uuid)
(def bytes->hex impl/bytes->hex)
(def hex->bytes impl/hex->bytes)
(def bytes->base64 impl/bytes->base64)
(def base64->bytes impl/base64->bytes)
(def bytes->int impl/bytes->int)
(def int->bytes impl/int->bytes)
(def bytes->float impl/bytes->float)
(def float->bytes impl/float->bytes)
(def bytes->uuid impl/bytes->uuid)
(def uuid->bytes impl/uuid->bytes)
(def int->bits impl/int->bits)
(def bits->int impl/bits->int)
(def bytes->bits impl/bytes->bits)
(def bits->bytes impl/bits->bytes)

;;; protocols

(defprotocol IDetachable
  (-detach [this]
    "Detach low level resource from reader/writer."))

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
  (-write [this b]
    "Write bytes to buffer, return p/let-able new writer.")
  (-flush [this]
    "Flush buffered bytes, return p/let-able new writer."))

;;; utils

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

(defn read-line
  ([reader & {:keys [end keepend charset]
              :or {end "\r\n" keepend false}}]
   (let [end (if-not (string? end)
               end
               (if-not charset
                 (str->bytes end)
                 (str->bytes end charset)))]
     (letfn [(pred [data pos]
               (when-let [i (index-of data end pos)]
                 [data pos i]))]
       (p/let [[reader [data pos i]] (peek-until reader pred)]
         (let [j (+ i (blength end))]
           [(-seek reader j)
            (let [b (sub data pos (if-not keepend i j))]
              (if-not charset
                (bytes->str b)
                (bytes->str b charset)))]))))))

(defn read-eof [reader]
  (let [[data pos] (-peek reader)]
    (if (< pos (blength data))
      [reader false]
      (p/let [[reader r] (-peek-more reader)]
        [reader (nil? r)]))))

(defn write
  ([writer] (-flush writer))
  ([writer b]
   (if (bempty? b)
     writer
     (-write writer b))))

;;; bytes

(extend-type @#'btype
  IDetachable
  (-detach [this] this)
  IBytesReader
  (-peek [this]
    [this 0])
  (-seek [this pos]
    (sub this pos))
  (-peek-more [this]
    [this nil])
  IBytesWriter
  (-write [this b]
    (concat this b))
  (-flush [this]
    this))

;;; buffer

(defrecord reader [data pos]
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

(defrecord writer [ring]
  IDetachable
  (-detach [this]
    (let [{:keys [ring]} this]
      (if (empty? ring)
        (bmake 0)
        (first ring))))
  IBytesWriter
  (-write [this b]
    (let [{:keys [ring]} this]
      (->writer (conj ring b))))
  (-flush [this]
    (let [{:keys [ring]} this]
      (->writer [(apply concat ring)]))))

(defn make-reader
  ([] (make-reader (bmake 0)))
  ([data] (make-reader data 0))
  ([data pos] (->reader data pos)))

(defn make-writer
  ([] (->writer []))
  ([data] (->writer [data])))

;;; chan

(defrecord chan-reader [data pos chan]
  IDetachable
  (-detach [this]
    (let [{:keys [data pos chan]} this]
      [(sub data pos) chan]))
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

(defrecord chan-writer [chan ring]
  IDetachable
  (-detach [this]
    (:chan this))
  ICloseable
  (-close [this]
    (csp/close! (:chan this)))
  IBytesWriter
  (-write [this b]
    (let [{:keys [chan ring]} this]
      (->chan-writer chan (conj ring b))))
  (-flush [this]
    (let [{:keys [chan ring]} this
          b (apply concat ring)]
      (if (bempty? b)
        this
        (p/let [ok (csp/put chan b)]
          (if ok
            (->chan-writer chan [])
            (p/rejected (want-write-error))))))))

(defn make-chan-reader [chan]
  (->chan-reader (bmake 0) 0 chan))

(defn make-chan-writer [chan]
  (->chan-writer chan []))

;;; stream

(defrecord stream [resource close-chan in-chan out-chan]
  ICloseable
  (-close [this]
    (csp/close! (:close-chan this))))

(defrecord error-stream [resource close-chan in-chan out-chan err-chan]
  ICloseable
  (-close [this]
    (csp/close! (:close-chan this))))

(defn make-stream-reader [stream]
  (make-chan-reader (:in-chan stream)))

(defn make-stream-writer [stream]
  (make-chan-writer (:out-chan stream)))

(defn make-stream-error-reader [stream]
  (make-chan-reader (:err-chan stream)))
