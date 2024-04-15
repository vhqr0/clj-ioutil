(ns ioutil.array
  (:refer-clojure :exclude [concat compare]))

(def ^:private rand-seed (java.util.Random.))

(defn rand-byte-array [n]
  (let [a (byte-array n)]
    (.nextBytes rand-seed a)
    a))

(defn byte-array->str
  ([a] (byte-array->str a "utf-8"))
  ([a encoding] (String. a encoding)))

(defn str->byte-array
  ([s] (str->byte-array s "utf-8"))
  ([s encoding] (.getBytes s encoding)))

(defn copy
  "Copy n items between arrays x and y, return n."
  ([x y] (copy x 0 y 0))
  ([x xs y ys] (copy x xs y ys (min (- (alength x) xs) (- (alength y) ys))))
  ([x xs y ys n] (do (System/arraycopy x xs y ys n) n)))

(defn fill
  "Fill array x with v, return x."
  ([x v] (fill x 0 v))
  ([x s v] (fill x s (alength x) v))
  ([x s e v] (do (java.util.Arrays/fill x s e v) x)))

(defn sub
  "Sub array."
  ([x] (sub x 0))
  ([x s] (sub x s (alength x)))
  ([x s e] (java.util.Arrays/copyOfRange x s e)))

(def ^:dynamic *empty-value* (byte-array 0))

(defn concat
  "Concat arrays."
  ([] *empty-value*)
  ([x] (java.util.Arrays/copyOf x (alength x)))
  ([x y] (let [xc (alength x)
               yc (alength y)
               a (java.util.Arrays/copyOf x (+ xc yc))]
           (copy y 0 a xc yc)
           a))
  ([x y & xs]
   (let [xs (cons y xs)
         a (java.util.Arrays/copyOf x (reduce + (map alength (cons x xs))))]
     (loop [xs xs i (alength x)]
       (if (empty? xs)
         a
         (let [x (first xs)]
           (recur (rest xs) (+ i (copy x 0 a i (alength x))))))))))

(defn compare
  "Compare arrays x and y."
  ([x y] (compare x 0 y 0))
  ([x xs y ys] (compare x xs (alength x) y ys (alength y)))
  ([x xs xe y ys ye] (java.util.Arrays/compare x xs xe y ys ye)))

(defn- first-index-of [h n s]
  (let [c (alength n)]
    (loop [s s]
      (if (empty? s)
        -1
        (let [i (first s)]
          (if (zero? (compare h i (+ i c) n 0 c))
            i
            (recur (rest s))))))))

(defn index-of
  "Find needle n in haystack h."
  ([h n] (index-of h n 0))
  ([h n s] (index-of h n s (alength h)))
  ([h n s e] (first-index-of h n (range s (inc (- e (alength n)))))))

(defn last-index-of
  "Reverse version of `index-of`."
  ([h n] (last-index-of h n 0))
  ([h n s] (last-index-of h n s (alength h)))
  ([h n s e] (first-index-of h n (range (- e (alength n)) (dec s) -1))))
