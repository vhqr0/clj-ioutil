(ns ioutil.stream
  (:require [ioutil.bytes :as bytes]
            [ioutil.struct :as struct]))

;; source: seq of bytes
;; putfn: (b)->nil, ()->nil

;;; read

(defrecord ring [reader source])

(defn make-ring [source]
  (->ring (bytes/make-reader (bytes/make-bytes 0)) source))

(defn ring->source [ring]
  (let [{:keys [reader source]} ring
        {:keys [data pos]} reader]
    (if (= pos (bytes/blength data))
      source
      (cons (bytes/sub data pos) source))))

(defn trim
  ([ring threshold]
   (if (<= (get-in ring [:reader :pos]) threshold)
     ring
     (trim ring)))
  ([ring]
   (let [{:keys [reader source]} ring
         {:keys [data pos]} reader]
     (if (zero? pos)
       ring
       (->ring (bytes/make-reader (bytes/sub data pos)) source)))))

(defn pull
  ([ring threshold]
   (if (>= (get-in ring [:reader :pos]) threshold)
     (throw (bytes/want-write-error))
     (pull ring)))
  ([ring]
   (let [{:keys [reader source]} ring
         {:keys [data pos]} reader]
     (loop [source source]
       (if (empty? source)
         (throw (bytes/want-read-error))
         (let [b (first source)]
           (if (zero? (bytes/blength b))
             (recur (rest source))
             (->ring
              (bytes/make-reader
               (if (= pos (bytes/blength data))
                 b
                 (bytes/concat (bytes/sub data pos) b)))
              (rest source)))))))))

(def ^:dynamic *read-threshold* (bit-shift-left 1 20))

(defn read
  ([ring readfn] (read ring readfn *read-threshold*))
  ([ring readfn threshold]
   (loop [ring ring]
     (let [{:keys [reader source]} ring
           res (volatile! nil)
           exc (volatile! nil)]
       (try
         (vreset! res (readfn reader))
         (catch Exception e
           (vreset! exc e)))
       (let [e @exc]
         (cond (nil? e) (let [[reader res] @res] [(->ring reader source) res])
               (bytes/want-read-error? e) (recur (pull ring threshold))
               :else (throw e)))))))

(defn read-struct
  ([ring struct] (read-struct ring struct *read-threshold*))
  ([ring struct threshold] (read ring (:readfn struct) threshold)))

;;; write

(defn write [putfn writefn it]
  (putfn (struct/data->bytes writefn it)))

(defn write-struct [putfn struct it]
  (putfn (struct/struct->bytes struct it)))
