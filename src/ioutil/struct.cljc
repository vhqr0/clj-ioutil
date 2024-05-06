(ns ioutil.struct
  (:require [promesa.core :as p]
            [ioutil.bytes :as b]
            [ioutil.bytes.util :as u]))

(defmulti read-struct-by-type (fn [reader type args] type))
(defmulti write-struct-by-type (fn [writer type args it] type))

(defn- spec-> [spec]
  (cond (keyword? spec) [spec ()]
        (and (vector? spec) (keyword? (first spec))) [(first spec) (rest spec)]))

(defn read-struct [reader spec]
  (let [[type args] (spec-> spec)]
    (read-struct-by-type reader type args)))

(defn read-one-struct [reader spec]
  (p/let [[reader it] (read-struct reader spec)
          [reader eof] (b/read-eof reader)]
    (assert eof)
    [reader it]))

(defn read-many-struct [reader spec]
  (p/loop [r (b/read-eof reader) them []]
    (let [[reader eof] r]
      (if eof
        [reader them]
        (p/let [[reader it] (read-struct reader spec)]
          (p/recur (b/read-eof reader) (conj them it)))))))

(defn write-struct [writer spec it]
  (let [[type args] (spec-> spec)]
    (write-struct-by-type writer type args it)))

(defn write-many-struct [writer spec them]
  (p/loop [writer writer them them]
    (if (empty? them)
      writer
      (p/let [writer (write-struct writer spec (first them))]
        (p/recur writer (rest them))))))

(defn bytes->struct [b spec]
  (let [reader (b/make-reader b)]
    (p/let [[reader it] (read-one-struct reader spec)]
      it)))

(defn bytes->many-struct [b spec]
  (let [reader (b/make-reader b)]
    (p/let [[reader them] (read-many-struct reader spec)]
      them)))

(defn struct->bytes [it spec]
  (let [writer (b/make-writer)]
    (p/let [writer (write-struct writer spec it)
            writer (b/flush writer)]
      (b/detach writer))))

(defn many-struct->bytes [them spec]
  (let [writer (b/make-writer)]
    (p/let [writer (write-many-struct writer spec them)
            writer (b/flush writer)]
      (b/detach writer))))

;;; assert

;; assertion wrapper
;; pos signed short example: [:assert pos? [:int 2]]

(defmethod read-struct-by-type :assert [reader type args]
  (let [[pred spec] args]
    (p/let [[reader it] (read-struct reader spec)]
      (assert (pred it))
      [reader it])))

(defmethod write-struct-by-type :assert [writer type args it]
  (let [[pred spec] args]
    (p/do
      (assert (pred it))
      (write-struct writer spec it))))

;;; map

;; mapping wrapper
;; 4 char string example: [:map [bytes->str str->bytes] [:bytes 4]]

(defmethod read-struct-by-type :map [reader type args]
  (let [[[rf wf] spec] args]
    (p/let [[reader it] (read-struct reader spec)]
      [reader (rf it)])))

(defmethod write-struct-by-type :map [writer type args it]
  (let [[[rf wf] spec] args]
    (p/do
      (write-struct writer spec (wf it)))))

;;; take

;; take n (or custom pred) structs
;; 2 short example: [:take 2 [:int 2]]
;; http header example: [:take #(= (last %) "") :line]

(defn- take-pred-> [pred]
  (if (integer? pred) #(= (count %) pred) pred))

(defmethod read-struct-by-type :take [reader type args]
  (let [[pred spec] args
        pred (take-pred-> pred)]
    (p/loop [reader reader them []]
      (if (pred them)
        [reader them]
        (p/let [[reader it] (read-struct reader spec)]
          (p/recur reader (conj them it)))))))

(defmethod write-struct-by-type :take [writer type args them]
  (let [[pred spec] args
        pred (take-pred-> pred)]
    (p/do
      (assert (pred them))
      (write-many-struct writer spec them))))

;;; conj

;; conj multi structs
;; 2 short example: [:conj [:int 2] [:int 2]]

(defmethod read-struct-by-type :conj [reader type args]
  (let [specs args]
    (p/loop [reader reader specs specs them []]
      (if (empty? specs)
        [reader them]
        (p/let [[reader it] (read-struct reader (first specs))]
          (p/recur reader (rest specs) (conj them it)))))))

(defmethod write-struct-by-type :conj [writer type args them]
  (let [specs args]
    (p/loop [writer writer specs specs them them]
      (if (empty? specs)
        writer
        (p/let [writer (write-struct writer (first specs) (first them))]
          (p/recur writer (rest specs) (rest them)))))))

;;; assoc

;; assoc multi structs by ioutil.bytes.util/assoc-nested
;; spec maybe a fn that accept a map and return the final spec
;; varlen exmaple: [:assoc :len [:int 1] :data (fn [m] [:bytes (:len m)])]
;; varlen-2 example: [:assoc :len [:int 1] [:data1 :data2] (fn [m] [:take 2 [:bytes (:len m)]])]

(defmethod read-struct-by-type :assoc [reader type args]
  (let [specs (partition 2 args)]
    (p/loop [reader reader specs specs them {}]
      (if (empty? specs)
        [reader them]
        (let [[k spec] (first specs)
              spec (if (fn? spec) (spec them) spec)]
          (p/let [[reader it] (read-struct reader spec)]
            (p/recur reader (rest specs) (u/assoc-nested them k it))))))))

(defmethod write-struct-by-type :assoc [writer type args them]
  (let [specs (partition 2 args)]
    (p/loop [writer writer specs specs]
      (if (empty? specs)
        writer
        (let [[k spec] (first specs)
              spec (if (fn? spec) (spec them) spec)]
          (p/let [writer (write-struct writer spec (u/get-nested them k))]
            (p/recur writer (rest specs))))))))

;;; dissoc

;; helper for assoc, dissoc some const field from map
;; socks5 reply example: [:dissoc {:ver 5} [:assoc :ver [:int 1] :meth [:int 1]]]

(defmethod read-struct-by-type :dissoc [reader type args]
  (let [[kvs spec] args]
    (p/let [[reader them] (read-struct reader spec)]
      [reader (loop [kvs kvs them them]
                (if (empty? kvs)
                  them
                  (let [[k v] (first them)]
                    (when v
                      (assert (= (them k) v)))
                    (recur (rest kvs) (dissoc them k)))))])))

(defmethod write-struct-by-type :dissoc [writer type args them]
  (let [[kvs spec] args]
    (p/do
      (write-struct writer spec (merge them kvs)))))

;;; juxt

;; helper for assoc, convert between vector and map
;; 2 short example: [:juxt [:a :b] [:assoc :a [:int 2] :b [:int 2]]]

(defmethod read-struct-by-type :juxt [reader type args]
  (let [[ks spec] args]
    (p/let [[reader them] (read-struct reader spec)]
      [reader (mapv them ks)])))

(defmethod write-struct-by-type :juxt [writer type args them]
  (let [[ks spec] args]
    (p/do
      (write-struct writer spec (zipmap ks them)))))

;;; const

(defmethod read-struct-by-type :const [reader type args]
  (let [[v] args]
    [reader v]))

(defmethod write-struct-by-type :const [writer type args it]
  (let [[v] args]
    (p/do
      (when v
        (assert (= it v)))
      writer)))

;;; bytes

(defmethod read-struct-by-type :bytes [reader type args]
  (let [[n & {:keys [spec many] :or {many false}}] args]
    (p/let [[reader b] (b/read reader n)]
      (if-not spec
        [reader b]
        (p/let [it (if-not many
                     (bytes->struct b spec)
                     (bytes->many-struct b spec))]
          [reader it])))))

(defmethod write-struct-by-type :bytes [writer type args it]
  (let [[n & {:keys [spec many] :or {many false}}] args]
    (p/let [b (if-not spec
                it
                (if-not many
                  (struct->bytes it spec)
                  (many-struct->bytes it spec)))]

      (assert (= (b/blength b) n))
      (b/write writer b))))

;;; int

(defmethod read-struct-by-type :int [reader type args]
  (let [[n & args] args]
    (p/let [[reader b] (b/read reader n)]
      [reader (apply b/bytes->int b args)])))

(defmethod write-struct-by-type :int [writer type args it]
  (let [[n & args] args]
    (p/do
      (b/write writer (apply b/int->bytes it n args)))))

(def i8  [:int 1])
(def i16 [:int 2])
(def i32 [:int 4])
(def i64 [:int 8])

(def u8  [:int 1 :unsigned true])
(def u16 [:int 2 :unsigned true])
(def u32 [:int 4 :unsigned true])

;;; line

(defmethod read-struct-by-type :line [reader type args]
  (p/do
    (apply b/read-line reader args)))

(defmethod write-struct-by-type :line [writer type args it]
  (p/do
    (apply b/write-line writer it args)))

(def crlf-line :line)
(def cr-line [:line :end "\r"])
(def lf-line [:line :end "\n"])

;;; bits

(defn make-bits-struct [lens]
  (let [n (u/bits-lens->bytes-lens lens)
        offsets (u/bits-lens->offsets lens)
        masks (u/bits-lens->masks lens)]
    [:bits n offsets masks]))

(defmethod read-struct-by-type :bits [reader type args]
  (let [[n offsets masks] args]
    (p/let [[reader b] (b/read reader n)]
      [reader (b/bytes->bits b offsets masks)])))

(defmethod write-struct-by-type :bits [writer type args it]
  (let [[n offsets masks] args]
    (p/do
      (b/write writer (b/bits->bytes it offsets n)))))

;;; tests

(comment
  (do
    (def http-header
      [:conj
       crlf-line
       [:map [#(->> (butlast %)
                    (map (fn [it] (clojure.string/split it #"\s*:\s*" 2)))
                    (into {}))
              #(-> (map (fn [[k v]] (str k \: \space v)) %)
                    (concat [""]))]
        [:take #(= (last %) "")
         crlf-line]]])
    (-> @(struct->bytes ["GET / HTTP/1.1" {"Test" "hello"}] http-header)
        b/bytes->str
        prn)
    (-> @(bytes->struct (b/str->bytes "HTTP/1.1 200 OK]\r\nTest: hello\r\n\r\n") http-header)
        prn)))
