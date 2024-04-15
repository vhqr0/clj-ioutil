(ns ioutil.bytes
  (:refer-clojure :exclude [concat compare read read-line flush])
  (:require [clojure.edn :as edn]
            [ioutil.array :as array]))

(def make-bytes byte-array)
(def blength alength)
(def bget aget)

(def rand-bytes array/rand-byte-array)
(def bytes->str array/byte-array->str)
(def str->bytes array/str->byte-array)

(def sub array/sub)
(def concat array/concat)
(def compare array/compare)
(def index-of array/index-of)
(def last-index-of array/last-index-of)

;;; errors

(defn want-read-error []
  (ex-info "bytes want read error" {:type ::want-read}))

(defn want-write-error []
  (ex-info "bytes want write error" {:type ::want-write}))

(defn want-read-error? [e]
  (= (:type (ex-data e)) ::want-read))

(defn want-write-error? [e]
  (= (:type (ex-data e)) ::want-write))

;;; seq

(defn bytes->seq [b]
  (->> (range (blength b)) (map #(bget b %))))

;;; hex

(def ^:private nibble->hex {0x0 \0 0x1 \1 0x2 \2 0x3 \3 0x4 \4
                            0x5 \5 0x6 \6 0x7 \7 0x8 \8 0x9 \9
                            0xa \a 0xb \b 0xc \c 0xd \d 0xe \e 0xf \f})

(def ^:private hex->nibble {\0 0x0 \1 0x1 \2 0x2 \3 0x3 \4 0x4
                            \5 0x5 \6 0x6 \7 0x7 \8 0x8 \9 0x9
                            \a 0xa \b 0xb \c 0xc \d 0xd \e 0xe \f 0xf
                            \A 0xa \B 0xb \C 0xc \D 0xd \E 0xe \F 0xf})

(defn- byte->nibble [i]
  [(bit-shift-right (bit-and i 0xf0) 4) (bit-and i 0xf)])

(defn- nibble->byte [l r]
  {:pre [(int? l) (int? r)]}
  (+ (bit-shift-left l 4) r))

(defn byte->hex [i]
  (let [[l r] (byte->nibble i)]
    (str (nibble->hex l) (nibble->hex r))))

(defn hex->byte
  ([h] (let [[l r] (seq h)] (hex->byte l r)))
  ([l r] (nibble->byte (hex->nibble l) (hex->nibble r))))

(defn bytes->hex [b]
  (->> (bytes->seq b)
       (map byte->hex)
       (apply str)))

(defn hex->bytes [h]
  (->> (partition-all 2 h)
       (map hex->byte)
       make-bytes))

;;; int

(defn seq->int [s]
  (reduce #(+ (bit-shift-left %1 8) (bit-and %2 0xff)) 0 s))

(defn int->seq [i]
  (lazy-seq (cons (bit-and i 0xff) (int->seq (bit-shift-right i 8)))))

(defn bytes->int
  ([b] (bytes->int b :big))
  ([b order] (seq->int
              (condp = order
                :big    (bytes->seq b)
                :little (reverse (bytes->seq b))))))

(defn int->bytes
  ([i n] (int->bytes i n :big))
  ([i n order]
   (let [s (take n (int->seq i))]
     (condp = order
       :big    (make-bytes (reverse s))
       :little (make-bytes s)))))

(defn str->int [s]
  {:post [(int? %)]}
  (if (empty? s)
    0
    (edn/read-string s)))

(defn hex->int [h]
  {:post [(int? %)]}
  (if (empty? h)
    0
    (edn/read-string (str "0x" h))))

;;; sint

(defn int->sint [i n]
  (let [m (bit-shift-left 1 (dec (bit-shift-left n 3)))]
    (if (< i m)
      i
      (- i (bit-shift-left m 1)))))

(defn sint->int [i n]
  (if (>= i 0)
    i
    (+ i (bit-shift-left 1 (bit-shift-left n 3)))))

(defn bytes->sint
  ([b] (bytes->sint b :big))
  ([b order] (-> (bytes->int b order) (int->sint (blength b)))))

(defn sint->bytes
  ([i n] (sint->bytes i n :big))
  ([i n order] (-> (sint->int i n) (int->bytes n order))))

;;; bits

(defn bits-lens->offsets [lens]
  (->> (reverse lens) (reductions + 0) butlast reverse vec))

(defn bits-lens->masks [lens]
  (mapv #(dec (bit-shift-left % 1)) lens))

(defn bits-lens->bytes-lens [lens]
  (let [n (reduce + lens)]
    (assert (zero? (mod n 8)))
    (bit-shift-right n 3)))

(defn int->bits [i offsets masks]
  (mapv #(bit-and (bit-shift-right i %1) %2) offsets masks))

(defn bits->int [bits offsets]
  (reduce + (map bit-shift-left bits offsets)))

(defn bytes->bits
  ([b offsets masks] (bytes->bits b offsets masks :big))
  ([b offsets masks order] (-> (bytes->int b order) (int->bits offsets masks))))

(defn bits->bytes
  ([bits offsets n] (bits->bytes offsets bits n :big))
  ([bits offsets n order] (-> (bits->int bits offsets) (int->bytes n order))))

;;; reader

(defrecord reader [data pos])

(defn make-reader [data]
  (->reader data 0))

(defn read
  ([{:keys [data pos]}]
   [(->reader data (blength data)) (sub data pos)])
  ([{:keys [data pos]} n]
   (let [npos (+ pos n)]
     (if (>= (blength data) npos)
       [(->reader data npos) (sub data pos npos)]
       (throw (want-read-error))))))

(defn read-line
  ([reader] (read-line reader "\n"))
  ([reader end] (read-line reader end false))
  ([{:keys [data pos]} end keepend]
   (let [end (if (string? end) (str->bytes end) end)
         i (index-of data end pos)]
     (if (neg? i)
       (throw (want-read-error))
       (let [npos (+ i (blength end))]
         [(->reader data npos)
          (sub data pos (if keepend npos i))])))))

(defn eof? [{:keys [data pos]}]
  (>= pos (blength data)))

(defn update! [a f & args]
  (let [[na r] (apply f @a args)]
    (reset! a na)
    r))

(defn vupdate! [v f & args]
  (let [[nv r] (apply f @v args)]
    (vreset! v nv)
    r))

;;; writer

(defn make-writer
  ([] (list))
  ([b] (list b)))

(defn write [writer b]
  (if (zero? (blength b))
    writer
    (cons b writer)))

(defn flush [writer]
  (->> (remove (comp zero? blength) writer)
       reverse
       (apply concat)))
