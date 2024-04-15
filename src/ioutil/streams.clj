(ns ioutil.streams
  (:require [ioutil.array :as array])
  (:import java.io.FileInputStream
           java.io.FileOutputStream
           java.net.Socket
           javax.net.ssl.SSLSocketFactory))

(def ^:dynamic *input-buffer-size* 8192)

(defn input-stream->source
  ([input-stream] (input-stream->source input-stream *input-buffer-size*))
  ([input-stream buffer-size]
   (let [buffer (byte-array buffer-size)]
     (letfn [(input-seq []
               (lazy-seq
                (let [n (.read input-stream buffer 0 (alength buffer))]
                  (if (pos? n)
                    (cons (array/sub buffer 0 n) (input-seq))
                    (do (.close input-stream) ())))))]
       (input-seq)))))

(defn output-stream->putfn [output-stream]
  (fn
    ([] (doto output-stream (.flush) (.close)))
    ([b] (.write output-stream b 0 (alength b)))))

;;; file

(defn- make-file-input-stream [path]
  (let [input-stream (FileInputStream. path)]
    [(input-stream->source input-stream)
     (fn [] (doto input-stream (.close)))]))

(defn- make-file-output-stream [path]
  (let [output-stream (FileOutputStream. path)]
    [() (output-stream->putfn output-stream)]))

(defn make-file-stream
  ([path] (make-file-stream path :input))
  ([path mode]
   (case mode
     :input (make-file-input-stream path)
     :output (make-file-output-stream path))))

;;; socket

(defn make-tcp-socket [host port]
  (Socket. host port))

(defn make-ssl-socket
  ([host port] (-> (make-tcp-socket host port) (make-ssl-socket host port)))
  ([socket host port] (-> (SSLSocketFactory/getDefault)
                          (.createSocket socket host port true))))

(defn socket->stream [socket]
  (let [input-stream (.getInputStream socket)
        output-stream (.getOutputStream socket)]
    [(input-stream->source input-stream) (output-stream->putfn output-stream)]))

(defn make-tcp-stream
  ([host port] (make-tcp-stream (make-tcp-socket host port)))
  ([socket] (socket->stream socket)))

(defn make-ssl-stream
  ([host port] (make-ssl-stream (make-ssl-socket host port)))
  ([socket host port] (make-ssl-stream (make-ssl-socket socket host port)))
  ([socket] (socket->stream socket)))
