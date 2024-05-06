(ns ioutil.bytes.impl
  (:refer-clojure :exclude [rand-int concat compare])
  (:require [clojure.core :as c]
            [ioutil.bytes.util :as u])
  (:import java.nio.ByteBuffer
           java.nio.charset.Charset
           java.util.Arrays
           java.util.Random
           java.util.UUID
           java.util.HexFormat
           [java.util Base64 Base64$Encoder Base64$Decoder]))

(def bmake byte-array)
(def btype (Class/forName "[B"))

(defn blength [b] (alength (bytes b)))
(defn bempty? [b] (zero? (alength (bytes b))))

(defn bget [b i] (aget (bytes b) i))

(def bseq seq)

(defn brseq [b]
  (map (partial bget b) (range (dec (blength b)) -1 -1)))

(defn- byte->uint [i] (bit-and 0xff (byte i)))
(defn bget-unsigned [b i] (byte->uint (aget (bytes b) i)))
(defn bseq-unsigned [b] (map byte->uint (bseq b)))
(defn brseq-unsigned [b] (map byte->uint (brseq b)))

;;; array

(defn pure-sub
  "Sub bytes, pure (always return new bytes)."
  ([b]
   (if-not (instance? ByteBuffer b)
     (aclone (bytes b))
     (let [^ByteBuffer bb b]
       (pure-sub (.array bb) (.position bb) (.limit bb)))))
  ([b s]
   (pure-sub b s (blength b)))
  ([b s e]
   (Arrays/copyOfRange (bytes b) (int s) (int e))))

(defn sub
  "Sub array, impure (reuse origin array if possible)."
  ([b]
   (if-not (instance? ByteBuffer b)
     b
     (let [^ByteBuffer bb b]
       (sub (.array bb) (.position bb) (.limit bb)))))
  ([b s]
   (if (zero? s)
     b
     (pure-sub b s (blength b))))
  ([b s e]
   (if (and (zero? s) (= e (blength b)))
     b
     (pure-sub b s e))))

(defn- pure-concat-1
  ([]
   (bmake 0))
  ([b]
   (let [[b s e n] b]
     (pure-sub b s e)))
  ([b1 b2]
   (let [[b1 s1 e1 n1] b1
         [b2 s2 e2 n2] b2
         nb (bmake (+ n1 n2))]
     (System/arraycopy b1 s1 nb 0 n1)
     (System/arraycopy b2 s2 nb n1 n2)
     nb))
  ([b1 b2 & bs]
   (let [bs (cons b1 (cons b2 bs))
         is (reductions + (map #(% 3) bs))
         nb (bmake (last is))]
     (doseq [[[b s e n] i] (zipmap bs (cons 0 (butlast is)))]
       (System/arraycopy b s nb i n))
     nb)))

(defn- concat-1
  ([]
   (bmake 0))
  ([b]
   (let [[b s e n] b]
     (sub b s e)))
  ([b & bs]
   (apply pure-concat-1 b bs)))

(defn- concat-> [bs]
  (letfn [(vector-> [[b s e]]
            (let [s (or s 0)
                  e (or e (blength b))]
              [b s e (- e s)]))
          (buffer-> [bb]
            (let [^ByteBuffer bb bb
                  b (.array bb)
                  s (.position bb)
                  e (.limit bb)]
              [b s e (- e s)]))]
    (->> bs
         (map
          #(cond (vector? %) (vector-> %)
                 (instance? ByteBuffer %) (buffer-> %)
                 :else (let [e (blength %)] [% 0 e e])))
         (remove #(zero? (% 3))))))

(defn pure-concat
  "Concat array, pure (always return new array)."
  [& bs] (apply pure-concat-1 (concat-> bs)))

(defn concat
  "Concat array, impure (reuse origin array if possible)."
  [& bs] (apply concat-1 (concat-> bs)))

(defn equals?
  "Test equiv of arrays x and y."
  ([x y] (Arrays/equals (bytes x) (bytes y)))
  ([x xs y ys] (equals? x xs (blength x) y ys (blength y)))
  ([x xs xe y ys ye] (Arrays/equals (bytes x) (int xs) (int xe) (bytes y) (int ys) (int ye))))

(defn compare
  "Compare arrays x and y."
  ([x y] (Arrays/compare (bytes x) (bytes y)))
  ([x xs y ys] (compare x xs (blength x) y ys (blength y)))
  ([x xs xe y ys ye] (Arrays/compare (bytes x) (int xs) (int xe) (bytes y) (int ys) (int ye))))

(defn index-of
  ([h n] (index-of h n 0))
  ([h n s] (index-of h n s (blength h)))
  ([h n s e] (u/index-of h n s e blength equals?)))

(defn last-index-of
  ([h n] (last-index-of h n 0))
  ([h n s] (last-index-of h n s (blength h)))
  ([h n s e] (u/index-of h n s e blength equals? :last true)))

;;; num utils

(defn cast-int [i n]
  (case (long n)
    1 (byte  i)
    2 (short i)
    4 (int   i)
    8 (long  i)))

(defn cast-float [f n]
  (case (long n)
    4 (float  n)
    8 (double n)))

(defn int->uint
  "Convert n bytes signed int to long with unsigned int value."
  ([i]
   (condp instance? i
     Byte    (bit-and 0xff       i)
     Short   (bit-and 0xffff     i)
     Integer (bit-and 0xffffffff i)))
  ([i n]
   (int->uint (cast-int i n))))

(defn uint->int
  "Convert long with unsigned value to n bytes signed int."
  [i n]
  (case (long n)
    1 (unchecked-byte  i)
    2 (unchecked-short i)
    4 (unchecked-int   i)))

;;; rand

(def ^:dynamic ^Random *random* (Random.))

(defn rand-bytes
  "Get random n bytes."
  [n]
  (let [b (bmake n)]
    (.nextBytes *random* b) b))

(defn rand-int
  "Get random n bytes int."
  ([]
   (.nextLong *random*))
  ([n]
   (case (long n)
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
  ([]
   (.nextDouble *random*))
  ([n]
   (case (long n)
     4 (.nextFloat *random*)
     8 (.nextDouble *random*))))

(defn rand-uuid
  "Get random uuid."
  [] (UUID/randomUUID))

;;; str

(defn bytes->str
  "Decode bytes to string."
  ([^bytes b]
   (String. b))
  ([^bytes b charset]
   (if (instance? Charset charset)
     (String. b ^Charset charset)
     (String. b ^String charset))))

(defn str->bytes
  "Encode string to bytes."
  ([^String s]
   (.getBytes s))
  ([^String s charset]
   (if (instance? Charset charset)
     (.getBytes s ^Charset charset)
     (.getBytes s ^String charset))))

(defn str->int
  "Parse int string."
  ([^String s]
   (Long/parseLong s))
  ([^String s & {:keys [radix unsigned]
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
  [^String s] (Double/parseDouble s))

(defn str->uuid
  "Parse uuid string."
  [^String s] (UUID/fromString s))

;;; codec

(defn bytes->hex
  "Convert bytes to hex string."
  [^bytes b] (.formatHex (HexFormat/of) b))

(defn hex->bytes
  "Parse hex string."
  [^String s] (.parseHex (HexFormat/of) s))

(defn- make-base64-encoder [encoder]
  (if (instance? Base64$Encoder encoder)
    encoder
    (case encoder
      :default (Base64/getEncoder)
      :url     (Base64/getUrlEncoder)
      :mime    (Base64/getMimeEncoder))))

(defn- make-base64-decoder [decoder]
  (if (instance? Base64$Decoder decoder)
    decoder
    (case decoder
      :default (Base64/getDecoder)
      :url     (Base64/getUrlDecoder)
      :mime    (Base64/getMimeDecoder))))

(defn bytes->base64
  "Convert bytes to base64 string.
  The optional encoder can specify a base64 variant (:default :url :mime)."
  ([^bytes b]
   (bytes->base64 b :default))
  ([^bytes b encoder]
   (let [^Base64$Encoder encoder (make-base64-encoder encoder)]
     (bytes->str (.encode encoder b)))))

(defn base64->bytes
  "Parse base64 string."
  ([^String s]
   (base64->bytes s :default))
  ([^String s decoder]
   (let [^Base64$Decoder decoder (make-base64-decoder decoder)]
     (.decode decoder s))))

;;; num

(defn bytes->int
  "Convert bytes to int."
  ([^bytes b]
   (case (long (blength b))
     1 (bget b 0)
     2 (.getShort (ByteBuffer/wrap b))
     4 (.getInt   (ByteBuffer/wrap b))
     8 (.getLong  (ByteBuffer/wrap b))))
  ([^bytes b & {:keys [little unsigned]
                :or {little false unsigned false}}]
   (let [b (if-not little
             b
             (bmake (reverse b)))]
     (if-not unsigned
       (bytes->int b)
       (-> (bytes->int b) int->uint)))))

(defn int->bytes
  "Convert n bytes int to bytes."
  ([i]
   (condp instance? i
     Byte (bmake [i])
     Short   (let [b (bmake 2)] (.putShort (ByteBuffer/wrap b) i) b)
     Integer (let [b (bmake 4)] (.putInt   (ByteBuffer/wrap b) i) b)
     Long    (let [b (bmake 8)] (.putLong  (ByteBuffer/wrap b) i) b)))
  ([i n]
   (int->bytes (cast-int i n)))
  ([i n & {:keys [little unsigned]
           :or {little false unsigned false}}]
   (let [b (if-not unsigned
             (int->bytes i n)
             (-> (uint->int i n) int->bytes))]
     (if-not little
       b
       (bmake (reverse b))))))

(defn bytes->float
  "Convert bytes to float."
  [^bytes b]
  (case (long (blength b))
    4 (.getFloat  (ByteBuffer/wrap b))
    8 (.getDouble (ByteBuffer/wrap b))))

(defn float->bytes
  "Convert n bytes float to bytes."
  ([f]
   (condp instance? f
     Float  (let [b (bmake 4)] (.putFloat  (ByteBuffer/wrap b) f) b)
     Double (let [b (bmake 8)] (.putDouble (ByteBuffer/wrap b) f) b)))
  ([f n]
   (cast-float f n)))

(defn bytes->uuid
  "Convert bytes to uuid."
  [^bytes b] {:pos [(= (alength b) 16)]}
  (let [bb (ByteBuffer/wrap b)
        l (.getLong bb)
        r (.getLong bb)]
    (UUID. l r)))

(defn uuid->bytes
  "Convert uuid to bytes."
  [^UUID u]
  (let [b (bmake 16)
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
