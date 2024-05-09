(ns ioutil.bytes.impl
  (:refer-clojure :exclude [rand-int concat compare])
  (:require [cljs.core :as c]
            [clojure.string :as str]
            [goog.crypt.base64 :as base64]
            [ioutil.bytes.util :as u]))

(def btype js/ArrayBuffer)

(defn bmake [x]
  (if (int? x)
    (js/ArrayBuffer. x)
    (.-buffer (js/Int8Array.from x))))

(defn blength [b]
  (.-byteLength b))

(def bempty? (comp zero? blength))

;;; getter as int8

(defn bget [b i]
  (aget (js/Int8Array. b) i))

(defn bseq [b]
  (seq (js/Int8Array. b)))

(defn brseq [b]
  (map (partial bget b) (range (dec (blength b)) -1 -1)))

;;; getter as uint8

(defn bget-unsigned [b i]
  (aget (js/Uint8Array. b) i))

(defn bseq-unsigned [b]
  (seq (js/Uint8Array. b)))

(defn brseq-unsigned [b]
  (map (partial bget-unsigned b) (range (dec (blength b)) -1 -1)))

;;; array like

(defn sub
  ([x] (.slice x))
  ([x s] (.slice x s))
  ([x s e] (.slice x s e)))

(defn- concat-1
  ([]
   (bmake 0))
  ([b]
   b)
  ([b & bs]
   (let [bs (cons b bs)
         is (reductions + (map blength bs))
         nb (bmake (last is))
         na (js/Int8Array. nb)]
     (doseq [[b i] (zipmap bs (cons 0 (butlast is)))]
       (.set na (js/Int8Array. b) i))
     nb)))

(defn concat [& bs]
  (->> (map #(if (vector? %) (apply sub %) %) bs)
       (remove bempty?)
       (apply concat-1)))

(defn compare
  ([x y]
   (let [c (c/compare (blength x) (blength y))]
     (if-not (zero? c)
       c
       (loop [x (bseq-unsigned x) y (bseq-unsigned y)]
         (if (empty? x)
           0
           (let [c (c/compare (first x) (first y))]
             (if-not (zero? c)
               c
               (recur (rest x) (rest y)))))))))
  ([x xs y ys]
   (compare (sub x xs) (sub y ys)))
  ([x xs xe y ys ye]
   (compare (sub x xs xe) (sub y ys ye))))

(def equals? (comp zero? compare))

(defn index-of
  ([h n] (index-of h n 0))
  ([h n s] (index-of h n s (blength h)))
  ([h n s e] (u/index-of h n s e blength equals?)))

(defn last-index-of
  ([h n] (last-index-of h n 0))
  ([h n s] (last-index-of h n s (blength h)))
  ([h n s e] (u/index-of h n s e blength equals? :last true)))

;;; codec

;; Impl notes: base64 alphabet is ignored, goog.crypt.base64 support
;; mix of all variants.

(defn bytes->hex [b]
  (->> (bseq-unsigned b)
       (map #(.padStart (.toString % 16) 2 0))
       (apply str)))

(defn hex->bytes [s]
  (->> (partition 2 s)
       (map #(js/parseInt (apply str %) 16))
       bmake))

(def base64-alphabet
  {:default base64/Alphabet.DEFAULT
   :urlsafe base64/Alphabet.WEBSAFE})

(defn bytes->base64
  ([b] (base64/encodeByteArray (js/Uint8Array. b)))
  ([b alphabet] (base64/encodeByteArray (js/Uint8Array. b) (base64-alphabet alphabet))))

(defn base64->bytes
  ([s] (.-buffer (base64/decodeStringToUint8Array s)))
  ([s alphabet] (base64->bytes s)))

;;; str utils

;; Impl notes: TextEncoder with encoding is not a standard, but some
;; browsers support this (exclude Chrome), see below.
;;
;; [https://developer.mozilla.org/en-US/docs/Web/API/TextDecoder/encoding]
;; [https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder/encoding]

(defn bytes->str
  ([b]
   (.decode (js/TextDecoder.) b))
  ([b encoding]
   (let [decoder (js/TextDecoder. encoding)]
     (assert (= (.-encoding decoder) encoding))
     (.decode decoder b))))

(defn str->bytes
  ([s]
   (.-buffer (.encode (js/TextEncoder.) s)))
  ([s encoding]
   (let [encoder (js/TextEncoder. encoding)]
     (assert (= (.-encoding encoder) encoding))
     (.-buffer (.encode encoder s)))))

(defn str->int
  ([s]
   (js/parseInt s))
  ([s & {:keys [radix unsigned] :or {unsigned false}}]
   (let [i (if-not radix
             (js/parseInt s)
             (js/parseInt s radix))]
     (when unsigned
       (assert (>= i 0)))
     i)))

(defn str->float [s]
  (js/parseFloat s))

(def str->uuid uuid)

;;; num utils

(defn bytes->int
  [b & {:keys [little unsigned] :or {little false unsigned false}}]
  (if-not unsigned
    (case (blength b)
      1 (bget b 0)
      2 (.getInt16 (js/DataView. b) 0 little)
      4 (.getInt32 (js/DataView. b) 0 little)
      8 (.getBigInt64 (js/DataView. b) 0 little))
    (case (blength b)
      1 (bget-unsigned b 0)
      2 (.getUint16 (js/DataView. b) 0 little)
      4 (.getUint32 (js/DataView. b) 0 little)
      8 (.getBigUint64 (js/DataView. b) 0 little))))

(defn int->bytes
  [i n & {:keys [little unsigned] :or {little false unsigned false}}]
  (if-not unsigned
    (case n
      1 (bmake [i])
      2 (let [b (bmake n)] (.setInt16 (js/DataView. b) 0 i little) b)
      4 (let [b (bmake n)] (.setInt32 (js/DataView. b) 0 i little) b)
      8 (let [b (bmake n)] (.setBigInt64 (js/DataView. b) 0 i little) b))
    (case n
      1 (bmake [i])
      2 (let [b (bmake n)] (.setUint16 (js/DataView. b) 0 i little) b)
      4 (let [b (bmake n)] (.setUint32 (js/DataView. b) 0 i little) b)
      8 (let [b (bmake n)] (.setBigUint64 (js/DataView. b) 0 i little) b))))

(defn bytes->float [b & {:keys [little] :or {little false}}]
  (case (blength b)
    4 (.getFloat32 (js/DataView. b) 0 little)
    8 (.getFloat64 (js/DataView. b) 0 little)))

(defn float->bytes [f n & {:keys [little] :or {little false}}]
  (case n
    4 (let [b (bmake n)] (.setFloat32 (js/DataView. b) 0 f little) b)
    8 (let [b (bmake n)] (.setFloat64 (js/DataView. b) 0 f little) b)))

(defn bytes->uuid [b]
  (->> [[0 4] [4 6] [6 8] [8 10] [10 16]]
       (map #(bytes->hex (apply sub b %)))
       (interpose \-)
       (apply str)
       str->uuid))

(defn uuid->bytes [u]
  (->> (str/split (str u) #"-")
       (map hex->bytes)
       (apply concat)))

;;; rand

(defn rand-bytes [n]
  (bmake (repeatedly n #(c/rand-int 256))))

(defn rand-int [n & {:keys [unsigned] :or {unsigned false}}]
  (bytes->int (rand-bytes n) :unsigned unsigned))

(defn rand-float [n]
  (bytes->float (rand-bytes n)))

(def rand-uuid random-uuid)

;;; bits utils

(def int->bits u/int->bits)
(def bits->int u/bits->int)

(defn bytes->bits [b offsets masks]
  (-> (bytes->int b :unsigned true) (int->bits offsets masks)))

(defn bits->bytes [bits offsets n]
  (-> (bits->int bits offsets) (int->bytes n :unsigned true)))
