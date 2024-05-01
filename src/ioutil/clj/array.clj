(ns ioutil.clj.array
  (:refer-clojure :exclude [concat compare])
  (:require [ioutil.bytes.util :as u])
  (:import java.util.Arrays))

(defn copy
  "Copy n items between arrays x and y, return n."
  ([x y] (copy x 0 y 0))
  ([x xs y ys] (copy x xs y ys (min (- (alength x) xs) (- (alength y) ys))))
  ([x xs y ys n] (do (System/arraycopy x xs y ys n) n)))

(defn fill
  "Fill array x with v, return x."
  ([x v] (doto x (Arrays/fill v)))
  ([x s v] (fill x s (alength x) v))
  ([x s e v] (doto x (Arrays/fill s e v))))

(defn sub
  "Sub array."
  ([x] (aclone x))
  ([x s] (sub x s (alength x)))
  ([x s e] (Arrays/copyOfRange x s e)))

(def ^:dynamic *empty-array* nil)

(defn concat
  "Concat arrays."
  ([] *empty-array*)
  ([x] (aclone x))
  ([x y] (let [a (Arrays/copyOf x (+ (alength x) (alength y)))]
           (System/arraycopy y 0 a (alength x) (alength y))
           a))
  ([x y & xs]
   (let [xs (cons y xs)
         is (reductions + (map alength (cons x xs)))
         a (Arrays/copyOf x (last is))]
     (doseq [[x i] (zipmap xs (butlast is))]
       (System/arraycopy x 0 a i (alength x)))
     a)))

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
