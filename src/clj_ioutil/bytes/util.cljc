(ns clj-ioutil.bytes.util)

;;; index of

(defn index-of
  "Find needle in haystack.

  h: haystack
  n: needle
  s: start of haystack
  e: end of haystack
  (length-fn x) -> int: get length of haystack and needle
  (equals-pred x xs xe y ys ye) -> bool:  compare range of haystack and needle
  last: optional flag to control find direction
  "
  [h n s e length-fn equals-pred
   & {:keys [last] :or {last false}}]
  (let [c (length-fn n)
        r (range s (inc (- e c)))
        pred (fn [i] (when (equals-pred h i (+ i c) n 0 c) i))]
    (some pred (if-not last r (reverse r)))))

;;; bits utils

(defn bits-lens->bytes-lens [lens]
  (let [n (reduce + lens)]
    (assert (zero? (mod n 8)))
    (bit-shift-right n 3)))

(defn bits-lens->offsets [lens]
  (->> (reverse lens) (reductions + 0) butlast reverse vec))

(defn bits-lens->masks [lens]
  (mapv #(dec (bit-shift-left % 1)) lens))

(defn int->bits [i offsets masks]
  (mapv #(bit-and (bit-shift-right i %1) %2) offsets masks))

(defn bits->int [bits offsets]
  (reduce + (map bit-shift-left bits offsets)))

;;; assoc nested

(defn- nested-key-type [k]
  (cond (keyword? k) :keyword
        (vector? k) :vector
        (map? k) :map))

(defmulti assoc-nested (fn [m k v] (nested-key-type k)))
(defmulti get-nested (fn [m k] (nested-key-type k)))

;; keyword
;; (assoc-nested {} :a 1) => {:a 1}
;; (get-nested {:a 1} :a) => 1

(defmethod assoc-nested :keyword [m k v]
  (assoc m k v))

(defmethod get-nested :keyword [m k]
  (get m k))

;; vector
;; (assoc-nested {} [:a :b] [1 2]) => {:a 1 :b 2}
;; (get-nested {:a 1 :b 2} [:a :b]) => [1 2]

(defmethod assoc-nested :vector [m k v]
  (reduce
   (fn [m [k v]]
     (assoc-nested m k v))
   m
   (zipmap k v)))

(defmethod get-nested :vector [m k]
  (mapv #(get-nested m %) k))

;; map
;; (assoc-nested {} {:aa :a :bb :b} {:aa 1 :bb 2}) => {:a 1 :b 2}
;; (get-nested {:a 1 :b 2} {:aa :a :bb :b}) => {:aa 1 :bb 2}

(defmethod assoc-nested :map [m k v]
  (let [ks (keys k)
        k (mapv k ks)
        v (mapv v ks)]
    (assoc-nested m k v)))

(defmethod get-nested :map [m k]
  (let [ks (keys k)
        k (mapv k ks)
        v (get-nested m k)]
    (zipmap ks v)))
