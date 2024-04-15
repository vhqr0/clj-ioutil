(ns proto.addr
  (:require [clojure.string :as str]
            [ioutil.bytes :as bytes]
            [ioutil.struct :as struct]))

(defn bytes->mac [b]
  (if (= (bytes/blength b) 6)
    (->> (bytes/bytes->seq b)
         (map bytes/byte->hex)
         (interpose \:)
         (apply str))
    (throw (struct/validation-error))))

(defn mac->bytes [a]
  (let [ss (str/split a #":")]
    (if (= (count ss) 6)
      (->> (map bytes/hex->byte ss) bytes/make-bytes)
      (throw (struct/validation-error)))))

(defn bytes->ipv4 [b]
  (if (= (bytes/blength b) 4)
    (->> (bytes/bytes->seq b)
         (map #(bit-and 0xff %))
         (interpose \.)
         (apply str))
    (throw (struct/validation-error))))

(defn ipv4->bytes [a]
  (let [ss (str/split a #"\.")]
    (if (= (count ss) 4)
      (->> (map bytes/str->int ss) bytes/make-bytes)
      (throw (struct/validation-error)))))

(defn- find-longest-zero-ipv6-segs [ss]
  {:pre [(= (count ss) 8)]}
  (let [zeros (->> (map-indexed #(vector %1 (zero? (bytes/bytes->int %2))) ss)
                   (partition-by second)
                   (filter #(second (first %)))
                   (mapv #(vector (first (first %)) (first (last %)))))]
    (if (empty? zeros)
      [-1 -1]
      (apply max-key #(- (second %) (first %)) (reverse zeros)))))

(defn- bytes->ipv6-seg [b]
  {:pre [(= (count b) 2)]}
  (let [s (->> (bytes/bytes->hex b) (drop-while #(= % \0)))]
    (if (empty? s) "0" (apply str s))))

(defn- ipv6-seg->bytes [s]
  (-> (bytes/hex->int s)
      (bytes/int->bytes 2)))

(defn- join-ipv6-segs [ss]
  (->> (map bytes->ipv6-seg ss)
       (interpose \:)
       (apply str)))

(defn bytes->ipv6 [b]
  (if (= (bytes/blength b) 16)
    (let [ss (map #(bytes/sub b % (+ % 2)) (range 0 16 2))
          [s e] (find-longest-zero-ipv6-segs ss)]
      (if (neg? s)
        (join-ipv6-segs ss)
        (str (join-ipv6-segs (take s ss)) \: \: (join-ipv6-segs (drop (inc e) ss)))))
    (throw (struct/validation-error))))

(defn ipv6->bytes [a]
  (if-let [[l r] (str/split a #"::")]
    (if r
      (let [l (str/split l #":")
            r (str/split r #":")
            c (- 8 (count l) (count r))]
        (if (pos? c)
          (let [l (map ipv6-seg->bytes l)
                r (map ipv6-seg->bytes r)
                c (bytes/make-bytes (bit-shift-left c 1))]
            (apply bytes/concat (concat l [c] r)))
          (throw (struct/validation-error))))
      (let [ss (str/split a #":")]
        (if (= (count ss) 8)
          (->> (map ipv6-seg->bytes ss) (apply bytes/concat))
          (throw (struct/validation-error)))))))

(struct/defstruct mac
  [:bytes
   :len 6
   :to (bytes->mac it)
   :from (mac->bytes it)])

(struct/defstruct ipv4
  [:bytes
   :len 4
   :to (bytes->ipv4 it)
   :from (ipv4->bytes it)])

(struct/defstruct ipv6
  [:bytes
   :len 16
   :to (bytes->ipv6 it)
   :from (ipv6->bytes it)])
