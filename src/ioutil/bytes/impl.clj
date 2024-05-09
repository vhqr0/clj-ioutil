(ns ioutil.bytes.impl
  (:refer-clojure :exclude [rand-int concat compare])
  (:require [ioutil.bytes.util :as u])
  (:import java.nio.ByteBuffer
           java.nio.charset.Charset
           java.util.Arrays
           java.util.Random
           java.util.UUID
           java.util.HexFormat
           [java.util Base64 Base64$Encoder Base64$Decoder]
           [java.net URLEncoder URLDecoder]))

(def btype (Class/forName "[B"))
(def bmake byte-array)

(defn blength [b] (alength (bytes b)))
(defn bempty? [b] (zero? (alength (bytes b))))

;;; getter as int8

(defn bget [b i] (aget (bytes b) i))

(def bseq seq)

(defn brseq [b]
  (map (partial bget b) (range (dec (blength b)) -1 -1)))

;;; getter as uint8

(defn- byte->uint [i] (bit-and 0xff (byte i)))
(defn bget-unsigned [b i] (byte->uint (aget (bytes b) i)))
(defn bseq-unsigned [b] (map byte->uint (bseq b)))
(defn brseq-unsigned [b] (map byte->uint (brseq b)))

;;; array like

;; Impl notes: pure sub/connect designed for reusable byte array, such
;; as an input stream buffer; sub/concat also support ByteBuffer.

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
  "Sub bytes, impure (reuse origin bytes if possible)."
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
  "Concat seq of [bytes start end length]."
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

(defn- concat->
  "Coerce seq of bytes, [bytes [start [end]]] or BytesBuffer to seq
  of [bytes start end length], remove empty bytes."
  [bs]
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
  "Concat bytes, pure (always return new bytes)."
  [& bs] (apply pure-concat-1 (concat-> bs)))

(defn concat
  "Concat bytes, impure (reuse origin bytes if possible)."
  [& bs] (apply concat-1 (concat-> bs)))

(defn compare
  ([x y] (Arrays/compare (bytes x) (bytes y)))
  ([x xs y ys] (compare x xs (blength x) y ys (blength y)))
  ([x xs xe y ys ye] (Arrays/compare (bytes x) (int xs) (int xe) (bytes y) (int ys) (int ye))))

(defn equals?
  ([x y] (Arrays/equals (bytes x) (bytes y)))
  ([x xs y ys] (equals? x xs (blength x) y ys (blength y)))
  ([x xs xe y ys ye] (Arrays/equals (bytes x) (int xs) (int xe) (bytes y) (int ys) (int ye))))

(defn index-of
  ([h n] (index-of h n 0))
  ([h n s] (index-of h n s (blength h)))
  ([h n s e] (u/index-of h n s e blength equals?)))

(defn last-index-of
  ([h n] (last-index-of h n 0))
  ([h n s] (last-index-of h n s (blength h)))
  ([h n s e] (u/index-of h n s e blength equals? :last true)))

;;; codec

;; Impl notes: base64 also support alphabet :mime; base64 alphabet
;; also support java Base64Encoder/Decoder objects.

(defn bytes->hex
  [^bytes b] (.formatHex (HexFormat/of) b))

(defn hex->bytes
  [^String s] (.parseHex (HexFormat/of) s))

(defn- make-base64-encoder [alphabet]
  (if (instance? Base64$Encoder alphabet)
    alphabet
    (case alphabet
      :default (Base64/getEncoder)
      :urlsafe (Base64/getUrlEncoder)
      :mime    (Base64/getMimeEncoder))))

(defn- make-base64-decoder [alphabet]
  (if (instance? Base64$Decoder alphabet)
    alphabet
    (case alphabet
      :default (Base64/getDecoder)
      :urlsafe (Base64/getUrlDecoder)
      :mime    (Base64/getMimeDecoder))))

(defn bytes->base64
  ([^bytes b]
   (bytes->base64 b :default))
  ([^bytes b alphabet]
   (let [^Base64$Encoder encoder (make-base64-encoder alphabet)]
     (String. (.encode encoder b)))))

(defn base64->bytes
  ([^String s]
   (base64->bytes s :default))
  ([^String s alphabet]
   (let [^Base64$Decoder decoder (make-base64-decoder alphabet)]
     (.decode decoder s))))

;;; str utils

;; Impl notes: encoding also support java Charset objects; urlencoded
;; also support optional encoding.

(defn bytes->str
  ([^bytes b]
   (String. b))
  ([^bytes b encoding]
   (if (instance? Charset encoding)
     (String. b ^Charset encoding)
     (String. b ^String encoding))))

(defn str->bytes
  ([^String s]
   (.getBytes s))
  ([^String s encoding]
   (if (instance? Charset encoding)
     (.getBytes s ^Charset encoding)
     (.getBytes s ^String encoding))))

(defn str->urlencoded
  ([s]
   (str->urlencoded s (Charset/defaultCharset)))
  ([^String s encoding]
   (if (instance? Charset encoding)
     (URLEncoder/encode s ^Charset encoding)
     (URLEncoder/encode s ^String encoding))))

(defn urlencoded->str
  ([u]
   (urlencoded->str u (Charset/defaultCharset)))
  ([^String u encoding]
   (if (instance? Charset encoding)
     (URLDecoder/decode u ^Charset encoding)
     (URLDecoder/decode u ^String encoding))))

(defn str->int
  ([^String s]
   (Long/parseLong s))
  ([^String s & {:keys [radix unsigned] :or {unsigned false}}]
   (if-not unsigned
     (if-not radix
       (Long/parseLong s)
       (Long/parseLong s radix))
     (if-not radix
       (Long/parseUnsignedLong s)
       (Long/parseUnsignedLong s radix)))))

(defn str->float [^String s]
  (Double/parseDouble s))

(defn str->uuid [^String s]
  (UUID/fromString s))

;;; num utils

;; Impl notes: supported signed int length are 1, 2, 4 and 8;
;; supported unsigned int length are 1, 2 and 4; supported float
;; length are 4 and 8; int/float->bytes support type awared 1 arg;
;; return of bytes->int/float is type awared.

(defn cast-int
  "Cast any length int to n bytes int."
  [i n]
  (case (long n)
    1 (byte  i)
    2 (short i)
    4 (int   i)
    8 (long  i)))

(defn cast-float
  "Cast any length float to n bytes float."
  [f n]
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

(defn bytes->int
  ([^bytes b]
   (case (long (blength b))
     1 (bget b 0)
     2 (.getShort (ByteBuffer/wrap b))
     4 (.getInt   (ByteBuffer/wrap b))
     8 (.getLong  (ByteBuffer/wrap b))))
  ([^bytes b & {:keys [little unsigned] :or {little false unsigned false}}]
   (let [b (if-not little
             b
             (bmake (reverse b)))]
     (if-not unsigned
       (bytes->int b)
       (-> (bytes->int b) int->uint)))))

(defn int->bytes
  ([i]
   (condp instance? i
     Byte (bmake [i])
     Short   (let [b (bmake 2)] (.putShort (ByteBuffer/wrap b) i) b)
     Integer (let [b (bmake 4)] (.putInt   (ByteBuffer/wrap b) i) b)
     Long    (let [b (bmake 8)] (.putLong  (ByteBuffer/wrap b) i) b)))
  ([i n]
   (int->bytes (cast-int i n)))
  ([i n & {:keys [little unsigned] :or {little false unsigned false}}]
   (let [b (if-not unsigned
             (int->bytes i n)
             (-> (uint->int i n) int->bytes))]
     (if-not little
       b
       (bmake (reverse b))))))

(defn bytes->float
  ([^bytes b]
   (case (long (blength b))
     4 (.getFloat  (ByteBuffer/wrap b))
     8 (.getDouble (ByteBuffer/wrap b))))
  ([^bytes b & {:keys [little] :or {little false}}]
   (let [b (if-not little
             b
             (bmake (reverse b)))]
     (bytes->float b))))

(defn float->bytes
  ([f]
   (condp instance? f
     Float  (let [b (bmake 4)] (.putFloat  (ByteBuffer/wrap b) f) b)
     Double (let [b (bmake 8)] (.putDouble (ByteBuffer/wrap b) f) b)))
  ([f n]
   (float->bytes (cast-float f n)))
  ([f n & {:keys [little] :or {little false}}]
   (let [b (float->bytes f n)]
     (if-not little
       b
       (bmake (reverse b))))))

(defn bytes->uuid [^bytes b]
  (let [bb (ByteBuffer/wrap b)
        l (.getLong bb)
        r (.getLong bb)]
    (UUID. l r)))

(defn uuid->bytes [^UUID u]
  (let [b (bmake 16)
        bb (ByteBuffer/wrap b)
        l (.getMostSignificantBits u)
        r (.getLeastSignificantBits u)]
    (.putLong bb l)
    (.putLong bb r)
    b))

;;; rand

;; Impl notes: rand-int/float support 0 arg, return long/double by
;; default.

(def ^:dynamic ^Random *random* (Random.))

(defn rand-bytes [n]
  (let [b (bmake n)]
    (.nextBytes *random* b) b))

(defn rand-int
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
  ([]
   (.nextDouble *random*))
  ([n]
   (case (long n)
     4 (.nextFloat *random*)
     8 (.nextDouble *random*))))

(defn rand-uuid []
  (UUID/randomUUID))

;;; bits utils

(def int->bits u/int->bits)
(def bits->int u/bits->int)

(defn bytes->bits [b offsets masks]
  (-> (bytes->int b :unsigned true) (int->bits offsets masks)))

(defn bits->bytes [bits offsets n]
  (-> (bits->int bits offsets) (int->bytes n :unsigned true)))
