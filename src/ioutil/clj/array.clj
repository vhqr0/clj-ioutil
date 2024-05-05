(ns ioutil.clj.array
  (:refer-clojure :exclude [concat compare])
  (:require [ioutil.bytes.util :as u])
  (:import java.util.Arrays
           java.nio.Buffer))

(defn copy!
  "Copy n items between arrays x and y, return n."
  ([x y] (copy! x 0 y 0))
  ([x xs y ys] (copy! x xs y ys (min (- (alength x) xs) (- (alength y) ys))))
  ([x xs y ys n] (do (System/arraycopy x xs y ys n) n)))

(defn fill!
  "Fill array x with v."
  ([x v] (Arrays/fill x v))
  ([x s v] (fill! x s (alength x) v))
  ([x s e v] (Arrays/fill x s e v)))

(defn sub
  "Sub array, pure (always return new array)."
  ([x]
   (if-not (instance? Buffer x)
     (aclone x)
     (sub (.array x) (.position x) (.limit x))))
  ([x s]
   (sub x s (alength x)))
  ([x s e]
   (Arrays/copyOfRange x s e)))

(defn sub!
  "Sub array, impure (reuse origin array if possible)."
  ([x]
   (if-not (instance? Buffer x)
     x
     (sub! (.array x) (.position x) (.limit x))))
  ([x s]
   (if (zero? s)
     x
     (sub x s)))
  ([x s e]
   (if (and (zero? s) (= e (alength x)))
     x
     (sub x s e))))

(defn- concat-1
  "Concat arrays in form of [array start end length]."
  ([])
  ([x]
   (let [[x s e n] x]
     (sub x s e)))
  ([x y]
   (let [[x xs xe xn] x
         [y ys ye yn] y
         a (Arrays/copyOfRange x xs (+ xe yn))]
     (System/arraycopy y ys a xn yn)
     a))
  ([x y & xs]
   (let [xs (cons y xs)
         is (reductions + (map #(% 3) (cons x xs)))
         a (let [[x s e n] x]
             (Arrays/copyOfRange x s (+ s (last is))))]
     (doseq [[[x s e n] i] (zipmap xs (butlast is))]
       (System/arraycopy x s a i n))
     a)))

(defn- concat-1!
  ([])
  ([x]
   (let [[x s e n] x]
     (sub! x s e)))
  ([x & xs]
   (apply concat-1 x xs)))

(defn- concat-> [xs]
  (->> xs
       (map
        #(cond (vector? %) (let [[x s e] %
                                 s (or s 0)
                                 e (or e (alength x))]
                             [x s e (- e s)])
               (instance? Buffer %) (let [x (.array %)
                                          s (.position %)
                                          e (.limit %)]
                                      [x s e (- e s)])
               :else (let [e (alength %)]
                       [% 0 e e])))
       (remove #(zero? (% 3)))))

(defn concat
  "Concat array, pure (always return new array)."
  [& xs] (->> (concat-> xs) (apply concat-1)))

(defn concat!
  "Concat array, impure (reuse origin array if possible)."
  [& xs] (->> (concat-> xs) (apply concat-1!)))

(defn equals?
  "Test equiv of arrays x and y."
  ([x y] (Arrays/equals x y))
  ([x xs y ys] (equals? x xs (alength x) y ys (alength y)))
  ([x xs xe y ys ye] (Arrays/equals x xs xe y ys ye)))

(defn compare
  "Compare arrays x and y."
  ([x y] (Arrays/compare x y))
  ([x xs y ys] (compare x xs (alength x) y ys (alength y)))
  ([x xs xe y ys ye] (Arrays/compare x xs xe y ys ye)))

(defn index-of
  ([h n] (index-of h n 0))
  ([h n s] (index-of h n s (alength h)))
  ([h n s e] (u/index-of h n s e alength equals?)))

(defn last-index-of
  ([h n] (last-index-of h n 0))
  ([h n s] (last-index-of h n s (alength h)))
  ([h n s e] (u/index-of h n s e alength equals? :last true)))
