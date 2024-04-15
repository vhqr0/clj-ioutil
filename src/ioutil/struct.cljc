(ns ioutil.struct
  (:refer-clojure :exclude [defstruct])
  (:require [clojure.string :as str]
            [ioutil.bytes :as bytes]))

;; readfn: bytes/reader->(bytes/reader, it)
;; writefn: (bytes/writer, it)->bytes/writer
(defrecord st [readfn writefn])

(defn validation-error []
  (ex-info "struct validation error" {:type ::validation}))

(defn validation-error? [e]
  (= (:type (ex-data e)) ::validation))

;;; spec

;; simple: [:int :len 1]
;; complex: [[a [:int :len 1]] [b [:int :len 1]]]
;; record: U8

(defn- spec-type [spec]
  (cond (not (vector? spec))    :record
        (keyword? (first spec)) :simple
        (every? vector? spec)   :complex))

(defmulti spec->read-form spec-type)
(defmulti spec->write-form spec-type)

;;; make read/write form

;; simple spec:
;; (defstruct U8 [:int :len 1])

(defmulti simple-spec->read-form (fn [type kvs] type))
(defmulti simple-spec->write-form (fn [type kvs] type))

(defmulti simple-read-form-wrap (fn [type read-form kvs] type))
(defmulti simple-write-form-wrap (fn [type write-form kvs] type))

(defmethod simple-read-form-wrap :transform [_ read-form kvs]
  (if-let [to-form (:to kvs)]
    `(let [~'it ~read-form]
       ~to-form)
    read-form))

(defmethod simple-write-form-wrap :transform [_ write-form kvs]
  (if-let [from-form (:from kvs)]
    `(let [~'it ~from-form]
       ~write-form)
    write-form))

(defmethod simple-read-form-wrap :validate [_ read-form kvs]
  (if-let [to-validate-form (or (:validate kvs) (:to-validate kvs))]
    `(let [~'it ~read-form]
       (if ~to-validate-form
         ~'it
         (throw (validation-error))))
    read-form))

(defmethod simple-write-form-wrap :validate [_ write-form kvs]
  (if-let [from-validate-form (or (:validate kvs) (:from-validate kvs))]
    `(if ~from-validate-form
       ~write-form
       (throw (validation-error)))
    write-form))

(def simple-form-wrappers [:transform :validate])

(defn simple-read-form-> [read-form kvs]
  (reduce
   #(simple-read-form-wrap %2 %1 kvs)
   read-form
   simple-form-wrappers))

(defn simple-write-form-> [write-form kvs]
  (reduce
   #(simple-write-form-wrap %2 %1 kvs)
   write-form
   (reverse simple-form-wrappers)))

(defn- simple-spec-> [spec]
  (let [[type & kvs] spec]
    [type (into {} (map vec (partition-all 2 kvs)))]))

(defmethod spec->read-form :simple [spec]
  (let [[type kvs] (simple-spec-> spec)]
    (-> (simple-spec->read-form type kvs)
        (simple-read-form-> kvs))))

(defmethod spec->write-form :simple [spec]
  (let [[type kvs] (simple-spec-> spec)]
    (-> (simple-spec->write-form type kvs)
        (simple-write-form-> kvs))))

;; complex spec:
;; (defstruct XXX
;;   [[a [:int :len 1]]
;;    [b [:int :len 1]]])

(defn- complex-spec-> [spec spec->form]
  (mapv
   #(let [[l spec] %]
      [l (spec->form spec)])
   spec))

(defmethod spec->read-form :complex [spec]
  (let [specs (complex-spec-> spec spec->read-form)]
    `(let [~@(mapcat
              #(let [[l read-form] %]
                 `(~l ~read-form))
              specs)]
       [~@(flatten (map first specs))])))

(defmethod spec->write-form :complex [spec]
  (let [specs (complex-spec-> spec spec->write-form)]
    `(let [[~@(flatten (map first specs))] ~'it]
       ~@(map
          #(let [[l write-form] %]
             `(let [~'it ~l] ~write-form))
          specs))))

;; record spec:
;; (defstruct XXX U8)
;; (defstruct XXX (make-struct [:int :len 1]))

(defmethod spec->read-form :record [spec]
  `(bytes/vupdate! ~'reader (:readfn ~spec)))

(defmethod spec->write-form :record [spec]
  `(vswap! ~'writer (:writefn ~spec) ~'it))

;;; make readfn/writefn/struct

(defmacro make-readfn [spec]
  `(fn [~'reader]
     (let [~'reader (volatile! ~'reader)
           ~'it ~(spec->read-form spec)]
       [@~'reader ~'it])))

(defmacro make-writefn [spec]
  `(fn [~'writer ~'it]
     (let [~'writer (volatile! ~'writer)]
       ~(spec->write-form spec)
       @~'writer)))

(defmacro make-struct [spec]
  `(->st
    (make-readfn ~spec)
    (make-writefn ~spec)))

(defmacro defstruct [name spec]
  `(def ~name (make-struct ~spec)))

(defn read-struct [reader struct]
  ((:readfn struct) reader))

(defn write-struct [writer struct it]
  ((:writefn struct) writer it))

;;; read/write one/many

(defn read-one [reader readfn]
  (try
    (let [[reader it] (readfn reader)]
      (if (bytes/eof? reader)
        it
        (throw (bytes/want-write-error))))
    (catch Exception e
      (throw (if (bytes/want-read-error? e) (validation-error) e)))))

(defn read-many [reader readfn]
  (try
    (loop [reader reader them []]
      (if (bytes/eof? reader)
        them
        (let [[reader it] (readfn reader)]
          (recur reader (conj them it)))))
    (catch Exception e
      (throw (if (bytes/want-read-error? e) (validation-error) e)))))

(defn write-many [writer writefn them]
  (reduce writefn writer them))

(defn bytes->data [readfn b]
  (-> (bytes/make-reader b) (read-one readfn)))

(defn data->bytes [writefn it]
  (-> (bytes/make-writer) (writefn it) bytes/flush))

(defn bytes->many-data [readfn b]
  (-> (bytes/make-reader b) (read-many readfn)))

(defn many-data->bytes [writefn them]
  (-> (bytes/make-writer) (write-many writefn them) bytes/flush))

(defn bytes->struct [struct b]
  (bytes->data (:readfn struct) b))

(defn struct->bytes [struct it]
  (data->bytes (:writefn struct) it))

(defn bytes->many-struct [struct b]
  (bytes->many-data (:readfn struct) b))

(defn many-struct->bytes [struct them]
  (many-data->bytes (:writefn struct) them))

;;; simple structs

;; struct

(defn- struct-spec-> [kvs spec->form]
  (let [{:keys [spec]} kvs]
    (spec->form spec)))

(defmethod simple-spec->read-form :struct [_ kvs]
  (struct-spec-> kvs spec->read-form))

(defmethod simple-spec->write-form :struct [_ kvs]
  (struct-spec-> kvs spec->write-form))

;; const

(defn- const-spec-> [kvs]
  (let [{:keys [val]} kvs]
    val))

(defn ensure-equal [x y]
  (when-not (= x y)
    (throw (validation-error))))

(defmethod simple-spec->read-form :const [_ kvs]
  (const-spec-> kvs))

(defmethod simple-spec->write-form :const [_ kvs]
  (let [val (const-spec-> kvs)]
    `(ensure-equal ~'it ~val)))

;; if

(defn- if-spec-> [kvs spec->form]
  (let [{:keys [test then else]} kvs]
    [test (spec->form then) (spec->form else)]))

(defn- if-spec->form [kvs spec->form]
  (let [[test then else] (if-spec-> kvs spec->form)]
    `(if ~test ~then ~else)))

(defmethod simple-spec->read-form :if [_ kvs]
  (if-spec->form kvs spec->read-form))

(defmethod simple-spec->write-form :if [_ kvs]
  (if-spec->form kvs spec->write-form))

;; cond

(defn- cond-tests-> [tests spec->form]
  (mapcat
   #(let [[test spec] %]
      `(~test ~(spec->form spec)))
   (partition-all 2 tests)))

(defn- cond-spec-> [kvs spec->form]
  (let [{:keys [tests]} kvs]
    (cond-tests-> tests)))

(defn- cond-spec->form [kvs spec->form]
  (let [tests (cond-spec-> kvs spec->form)]
    `(cond ~@tests :else (throw (validation-error)))))

(defmethod simple-spec->read-form :cond [_ kvs]
  (cond-spec->form kvs spec->read-form))

(defmethod simple-spec->write-form :cond [_ kvs]
  (cond-spec->form kvs spec->write-form))

;; case

(defn- case-spec-> [kvs spec->form]
  (let [{[v & tests] :tests} kvs]
    [v (cond-tests-> tests spec->form)]))

(defn- case-spec->form [kvs spec->form]
  (let [[v tests] (case-spec-> kvs spec->form)]
    `(case ~v ~@tests)))

(defmethod simple-spec->read-form :case [_ kvs]
  (case-spec->form kvs spec->read-form))

(defmethod simple-spec->write-form :case [_ kvs]
  (case-spec->form kvs spec->write-form))

;; condp

(defn- condp-spec-> [kvs spec->form]
  (let [{[v p & tests] :tests} kvs]
    [v p (cond-tests-> tests spec->form)]))

(defn- condp-spec->form [kvs spec->form]
  (let [[v p tests] (condp-spec-> kvs spec->form)]
    `(condp ~v ~p ~@tests)))

(defmethod simple-spec->read-form :condp [_ kvs]
  (condp-spec->form kvs spec->read-form))

(defmethod simple-spec->write-form :condp [_ kvs]
  (condp-spec->form kvs spec->write-form))

;; list

(defn- list-spec-> [kvs spec->form]
  (let [{:keys [len spec]} kvs]
    [len (spec->form spec)]))

(defn ensure-list-len [list len]
  (if (= (count list) len)
    list
    (throw (validation-error))))

(defmethod simple-spec->read-form :list [_ kvs]
  (let [[len read-form] (list-spec-> kvs spec->read-form)]
    `(mapv #(~read-form) (range ~len))))

(defmethod simple-spec->write-form :list [_ kvs]
  (let [[len write-form] (list-spec-> kvs spec->write-form)]
    `(let [~'them (ensure-list-len ~'it ~len)]
       (doseq [~'it ~'them] ~write-form))))

;; while

(defn- while-spec-> [kvs spec->form]
  (let [{:keys [test spec]} kvs]
    [test (spec->form spec)]))

(defmethod simple-spec->read-form :while [_ kvs]
  (let [[test read-form] (while-spec-> kvs spec->read-form)]
    `(loop [~'them []]
       (let [~'it ~read-form]
         (if ~test
           (conj ~'them ~'it)
           (recur (conj ~'them ~'it)))))))

(defmethod simple-spec->write-form :while [_ kvs]
  (let [[test write-form] (while-spec-> kvs spec->write-form)]
    `(do
       (let [[~'them ~'it] [(butlast ~'it) (last ~'it)]]
         (when-not ~test
           (throw (validation-error))))
       (let [~'them ~'it] (doseq [~'it ~'them] ~write-form)))))

;; bytes

(defn- bytes-spec-> [kvs]
  (let [{:keys [len spec many] :or {many false}} kvs]
    [len spec many]))

(defmethod simple-spec->read-form :bytes [_ kvs]
  (let [[len spec many] (bytes-spec-> kvs)
        read-form `(bytes/vupdate! ~'reader bytes/read ~len)]
    (if spec
      `(~(if many `bytes->many-data `bytes->data)
        (make-readfn ~spec) ~read-form)
      read-form)))

(defmethod simple-spec->write-form :bytes [_ kvs]
  (let [[len spec many] (bytes-spec-> kvs)
        write-form `(vswap! ~'writer bytes/write ~'it)]
    (if spec
      `(let [~'it (~(if many `many-data->bytes `data->bytes)
                   (make-writefn ~spec) ~'it)]
         ~write-form)
      write-form)))

;; int

(defn- int-spec-> [kvs]
  (let [{:keys [len order signed] :or {order :big signed false}} kvs]
    [len order signed]))

(defmethod simple-spec->read-form :int [_ kvs]
  (let [[len order signed] (int-spec-> kvs)]
    `(-> (bytes/vupdate! ~'reader bytes/read ~len)
         (~(if signed 'bytes/bytes->sint 'bytes/bytes->int) ~order))))

(defmethod simple-spec->write-form :int [_ kvs]
  (let [[len order signed] (int-spec-> kvs)]
    `(->> (~(if 'bytes/sint->bytes 'bytes/int->bytes) ~'it ~len ~order)
          (vswap! ~'writer bytes/write))))

;; bits

(defn- bits-spec-> [kvs]
  (let [{:keys [lens order] :or {order :big}} kvs]
    [lens order]))

(defmethod simple-spec->read-form :bits [_ kvs]
  (let [[lens order] (bits-spec-> kvs)
        bytes-lens (bytes/bits-lens->bytes-lens lens)
        offsets (bytes/bits-lens->offsets lens)
        masks (bytes/bits-lens->masks lens)]
    `(-> (bytes/vupdate! ~'reader bytes/read ~bytes-lens)
         (bytes/bytes->bits ~offsets ~masks ~order))))

(defmethod simple-spec->write-form :bits [_ kvs]
  (let [[lens order] (bits-spec-> kvs)
        bytes-lens (bytes/bits-lens->bytes-lens lens)
        offsets (bytes/bits-lens->offsets lens)]
    `(->> (bytes/bits->bytes ~'it ~offsets ~bytes-lens ~order)
          (vswap! ~'writer bytes/write))))

;; line

(defn- line-spec-> [kvs]
  (let [{:keys [end keepend encoding] :or {end "\n" keepend false encoding "utf-8"}} kvs]
    [end keepend encoding]))

(defn ensure-line-end [line end]
  (if (str/ends-with? line end)
    line
    (throw (validation-error))))

(defmethod simple-spec->read-form :line [_ kvs]
  (let [[end keepend encoding] (line-spec-> kvs)]
    `(-> (bytes/vupdate! ~'reader bytes/read-line ~end ~keepend)
         (bytes/bytes->str ~encoding))))

(defmethod simple-spec->write-form :line [_ kvs]
  (let [[end keepend encoding] (line-spec-> kvs)]
    `(->> (bytes/str->bytes
           ~(if keepend
              `(ensure-line-end ~'it ~end)
              `(str ~'it ~end))
           ~encoding)
          (vswap! ~'writer bytes/write))))

;; vlen

(defn- vlen-spec-> [kvs type spec->form]
  (let [{:keys [len to-vlen from-vlen]} kvs
        vlen (gensym) v (gensym)]
    (spec->form
     `[:struct
       :spec [[~vlen [:int :len ~len :to ~to-vlen :from ~from-vlen]]
              [~v [~type :len ~vlen ~@(apply concat (dissoc kvs :len :to-vlen :from-vlen))]]]
       :to (let [[~vlen ~v] ~'it] ~v)
       :from [(count ~'it) ~'it]])))

(defmethod simple-spec->read-form :vlist [_ kvs]
  (vlen-spec-> kvs :list spec->read-form))

(defmethod simple-spec->write-form :vlist [_ kvs]
  (vlen-spec-> kvs :list spec->write-form))

(defmethod simple-spec->read-form :vbytes [_ kvs]
  (vlen-spec-> kvs :bytes spec->read-form))

(defmethod simple-spec->write-form :vbytes [_ kvs]
  (vlen-spec-> kvs :bytes spec->write-form))
