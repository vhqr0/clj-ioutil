(ns ioutil.bytes.impl
  (:refer-clojure :exclude [bytes? rand-int concat compare])
  (:require [clojure.core :as c]
            [ioutil.bytes.util :as u]
            [ioutil.clj.array :as a])
  (:import java.nio.ByteBuffer
           java.util.UUID
           java.util.HexFormat
           java.util.Base64))

(def make-bytes byte-array)
(def bytes? c/bytes?)

(def btype (type (byte-array 0)))
(def bcast bytes)

(def blength alength)
(def bempty? (comp zero? alength))

(def bget aget)
(def bseq seq)

(defn brseq [b]
  (map #(aget b %) (range (dec (alength b)) -1 -1)))

(def bget-unsigned #(bit-and 0xff (bget %1 %2)))

(defn bseq-unsigned [b] (map #(bit-and 0xff %) (bseq b)))
(defn brseq-unsigned [b] (map #(bit-and 0xff %) (brseq b)))

(defn sub
  ([b] b)
  ([b s] (if (zero? s) b (a/sub b s)))
  ([b s e] (a/sub b s e)))

(defn concat [& bs]
  (let [bs (remove bempty? bs)]
    (cond (empty? bs) (byte-array 0)
          (empty? (rest bs)) (first bs)
          :else (apply a/concat bs))))

(def equals? a/equals?)
(def compare a/compare)
(def index-of a/index-of)
(def last-index-of a/last-index-of)

;;; num utils

(defn cast-int [i n]
  (case n
    1 (byte  i)
    2 (short i)
    4 (int   i)
    8 (long  i)))

(defn cast-float [f n]
  (case n
    4 (float  n)
    8 (double n)))

(defn int->uint
  "Convert n bytes signed int to long with unsigned int value."
  ([i n] (int->uint (cast-int i n)))
  ([i] (condp instance? i
         java.lang.Byte    (bit-and 0xff       i)
         java.lang.Short   (bit-and 0xffff     i)
         java.lang.Integer (bit-and 0xffffffff i))))

(defn uint->int
  "Convert long with unsigned value to n bytes signed int."
  [i n]
  (case n
    1 (unchecked-byte  i)
    2 (unchecked-short i)
    4 (unchecked-int   i)))

;;; rand

(def ^:dynamic *random* (java.util.Random.))

(defn rand-bytes
  "Get random n bytes."
  [n] (let [b (byte-array n)] (.nextBytes *random* b) b))

(defn rand-int
  "Get random n bytes int."
  ([] (.nextLong *random*))
  ([n]
   (case n
     1 (unchecked-byte (rand-int))
     2 (unchecked-short (rand-int))
     4 (.nextInt *random*)
     8 (.nextLong *random*)))
  ([n & {:keys [unsigned]
         :or {unsigned false}}]
   (if-not unsigned
     (rand-int n)
     (-> (rand-int n) int->uint))))

(defn rand-float
  "Get random n bytes float."
  ([] (.nextDouble *random*))
  ([n] (case n
         4 (.nextFloat *random*)
         8 (.nextDouble *random*))))

(defn rand-uuid
  "Get random uuid."
  [] (UUID/randomUUID))

;;; str

(defn bytes->str
  "Decode bytes to string."
  ([b] (String. b))
  ([b charset] (String. b charset)))

(defn str->bytes
  "Encode string to bytes."
  ([s] (.getBytes s))
  ([s charset] (.getBytes s charset)))

(defn str->int
  "Parse int string."
  ([s] (Long/parseLong s))
  ([s & {:keys [radix unsigned]
         :or {unsigned false}}]
   (if-not unsigned
     (if-not radix
       (Long/parseLong s)
       (Long/parseLong s radix))
     (if-not radix
       (Long/parseUnsignedLong s)
       (Long/parseUnsignedLong s radix)))))

(defn str->float
  "Parse float string."
  [s] (Double/parseDouble s))

(defn str->uuid
  "Parse uuid string."
  [s] (UUID/fromString s))

;;; codec

(defn bytes->hex
  "Convert bytes to hex string."
  [b] (-> (HexFormat/of) (.formatHex b)))

(defn hex->bytes
  "Parse hex string."
  [s] (-> (HexFormat/of) (.parseHex s)))

(def ^:dynamic *base64-encoder-factory*
  {:default (fn [] (Base64/getEncoder))
   :url     (fn [] (Base64/getUrlEncoder))
   :mime    (fn [] (Base64/getMimeEncoder))})

(def ^:dynamic *base64-decoder-factory*
  {:default (fn [] (Base64/getDecoder))
   :url     (fn [] (Base64/getUrlDecoder))
   :mime    (fn [] (Base64/getMimeDecoder))})

(defn bytes->base64
  "Convert bytes to base64 string.
  The optional encoder can specify a base64 variant (:default :url :mime)."
  ([b] (bytes->base64 b :default))
  ([b encoder]
   (-> ((*base64-encoder-factory* encoder))
       (.encode b)
       bytes->str)))

(defn base64->bytes
  "Parse base64 string."
  ([s] (base64->bytes s :default))
  ([s decoder]
   (-> ((*base64-decoder-factory* decoder))
       (.decode s))))

;;; num

(defn bytes->int
  "Convert bytes to int."
  ([b]
   (case (alength b)
     1 (aget b 0)
     2 (-> (ByteBuffer/wrap b) (.getShort))
     4 (-> (ByteBuffer/wrap b) (.getInt))
     8 (-> (ByteBuffer/wrap b) (.getLong))))
  ([b & {:keys [little unsigned]
         :or {little false unsigned false}}]
   (let [b (if-not little
             b
             (byte-array (reverse b)))]
     (if-not unsigned
       (bytes->int b)
       (-> (bytes->int b) int->uint)))))

(defn int->bytes
  "Convert n bytes int to bytes."
  ([i]
   (condp instance? i
     java.lang.Byte (byte-array [i])
     java.lang.Short   (let [b (byte-array 2)] (-> (ByteBuffer/wrap b) (.putShort i)) b)
     java.lang.Integer (let [b (byte-array 4)] (-> (ByteBuffer/wrap b) (.putInt   i)) b)
     java.lang.Long    (let [b (byte-array 8)] (-> (ByteBuffer/wrap b) (.putLong  i)) b)))
  ([i n] (int->bytes (cast-int i n)))
  ([i n & {:keys [little unsigned]
           :or {little false unsigned false}}]
   (let [b (if-not unsigned
             (int->bytes i n)
             (-> (uint->int i n) int->bytes))]
     (if-not little
       b
       (byte-array (reverse b))))))

(defn bytes->float
  "Convert bytes to float."
  [b] (case (alength b)
        4 (-> (ByteBuffer/wrap b) (.getFloat))
        8 (-> (ByteBuffer/wrap b) (.getDouble))))

(defn float->bytes
  "Convert n bytes float to bytes."
  ([f] (condp instance? f
         java.lang.Float  (let [b (byte-array 4)] (-> (ByteBuffer/wrap b) (.putFloat  f)) b)
         java.lang.Double (let [b (byte-array 8)] (-> (ByteBuffer/wrap b) (.putDouble f)) b)))
  ([f n] (cast-float f n)))

(defn bytes->uuid
  "Convert bytes to uuid."
  [b] {:pos [(= (alength b) 16)]}
  (let [bb (ByteBuffer/wrap b)
        l (.getLong bb)
        r (.getLong bb)]
    (UUID. l r)))

(defn uuid->bytes
  "Convert uuid to bytes."
  [u] (let [b (byte-array 16)
            bb (ByteBuffer/wrap b)
            l (.getMostSignificantBits u)
            r (.getLeastSignificantBits u)]
        (.putLong bb l)
        (.putLong bb r)
        b))

;;; bits

(def int->bits u/int->bits)
(def bits->int u/bits->int)

(defn bytes->bits [b offsets masks]
  (-> (bytes->int b :unsigned true) (int->bits offsets masks)))

(defn bits->bytes [bits offsets n]
  (-> (bits->int bits offsets) (int->bytes n :unsigned true)))
