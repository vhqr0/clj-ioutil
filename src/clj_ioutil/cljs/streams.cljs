(ns clj-ioutil.cljs.streams
  (:require [promesa.core :as p]
            [promesa.exec.csp :as csp]
            [clj-ioutil.bytes :as b]))

(defn readable-stream->chan
  ([rs cch]
   (readable-stream->chan rs cch (csp/chan)))
  ([rs cch ch]
   (p/vthread
    (let [reader (.getReader rs)]
      (-> (p/loop [r (.read reader)]
            (if (.-done r)
              (csp/close! ch)
              (p/let [ok (csp/put ch (.-buffer (.-value r)))]
                (assert ok)
                (p/recur (.read reader)))))
          (p/catch #(csp/close! cch %)))))
   ch))

(defn writable-stream->chan
  ([ws cch]
   (writable-stream->chan ws cch (csp/chan :buf 1 :xf (remove b/bempty?))))
  ([ws cch ch]
   (p/vthread
    (let [writer (.getWriter ws)]
      (-> (p/loop [b (csp/take ch)]
            (if-not b
              (.close writer)
              (p/do
                (.-ready writer)
                (.write writer b)
                (p/recur (csp/take ch)))))
          (p/catch #(csp/close! cch %)))))
   ch))

(defn readable-stream->read-stream [rs]
  (let [cch (csp/chan)
        ich (readable-stream->chan rs cch)]
    (b/create-stream cch ich nil #(.cancel rs))))

(defn writable-stream->write-stream [ws]
  (let [cch (csp/chan)
        och (writable-stream->chan ws cch)]
    (b/create-stream cch nil och #(.abort ws))))

(extend-protocol b/IStreamFactory
  js/ReadableStream
  (b/stream [this] (readable-stream->read-stream this))
  js/WritableStream
  (b/stream [this] (writable-stream->write-stream this)))
