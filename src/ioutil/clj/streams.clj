(ns ioutil.clj.streams
  (:require [promesa.core :as p]
            [promesa.exec.csp :as csp]
            [ioutil.bytes :as b])
  (:import [java.io InputStream OutputStream File FileInputStream FileOutputStream]
           [java.net SocketAddress InetAddress InetSocketAddress
            Proxy Proxy$Type ProxySelector URI Socket]
           javax.net.ssl.SSLSocketFactory))

(defn go-close [close-chan chans callback]
  (p/vthread
   (p/let [_ (csp/take close-chan)]
     (csp/close! close-chan)
     (doseq [chan chans]
       (csp/close! chan))
     (callback))))

;;; io

(def ^:dynamic *input-buffer-size* 8192)

(defn input-stream->readfn
  ([input-stream]
   (input-stream->readfn input-stream (b/make-bytes *input-buffer-size*)))
  ([input-stream buffer]
   (fn []
     (let [n (.read input-stream buffer)]
       (assert (pos? n))
       (b/sub buffer 0 n)))))

(defn output-stream->writefn [output-stream]
  (fn [b]
    (.write output-stream b)
    (.flush output-stream)))

(defn input-stream->chan
  [input-stream close-chan]
  (let [chan (csp/chan)
        read (input-stream->readfn input-stream)]
    (p/vthread
     (-> (p/loop [b (p/vthread (read))]
           (p/let [ok (csp/put chan b)]
             (assert ok)
             (p/recur (p/vthread (read)))))
         (p/catch #(csp/close! close-chan %))))
    chan))

(defn output-stream->chan
  [output-stream close-chan]
  (let [chan (csp/chan 1 (remove b/bempty?))
        write (output-stream->writefn output-stream)]
    (p/vthread
     (-> (p/loop [b (csp/take chan)]
           (assert b)
           (when-not (b/bempty? b)
             (write b))
           (p/recur (csp/take chan)))
         (p/catch #(csp/close! close-chan %))))
    chan))

;;; file

(defn make-file
  ([file]
   (if (instance? File file)
     file
     (if-not (vector? file)
       (File. file)
       (apply make-file file))))
  ([]
   (File. "."))
  ([parent child]
   (File. parent child)))

(defn make-input-file
  ([file]
   (if (instance? InputStream file)
     file
     (if-not (vector? file)
       (FileInputStream. file)
       (apply make-input-file file)))))

(defn make-output-file
  ([file]
   (if (instance? OutputStream file)
     file
     (if-not (vector? file)
       (FileOutputStream. file)
       (apply make-output-file file))))
  ([file & {:keys [append] :or {append false}}]
   (FileOutputStream. file append)))

(defn make-file-input-stream [file]
  (p/vthread
   (let [input-stream (make-input-file file)
         close-chan (csp/chan)
         in-chan (input-stream->chan input-stream close-chan)
         out-chan (csp/chan)]
     (go-close close-chan [in-chan out-chan] #(.close input-stream))
     (b/->stream close-chan in-chan out-chan))))

(defn make-file-output-stream [file]
  (p/vthread
   (let [output-stream (make-output-file file)
         close-chan (csp/chan)
         in-chan (csp/chan)
         out-chan (output-stream->chan output-stream close-chan)]
     (go-close close-chan [in-chan out-chan] #(.close output-stream))
     (b/->stream close-chan in-chan out-chan))))

;;; process

(defn make-process
  ([process]
   (if (instance? Process process)
     process
     (if-not (vector? process)
       (-> (Runtime/getRuntime) (.exec process))
       (apply make-process process))))
  ([command env]
   (-> (Runtime/getRuntime) (.exec command env)))
  ([command env dir]
   (-> (Runtime/getRuntime) (.exec command env dir))))

(defrecord process [process close-chan in-chan out-chan err-chan])

(defn make-process-stream
  ([process input-stream output-stream error-stream]
   (let [close-chan (csp/chan)
         in-chan (input-stream->chan input-stream close-chan)
         out-chan (output-stream->chan output-stream close-chan)
         err-chan (output-stream->chan error-stream close-chan)]
     (go-close close-chan [in-chan out-chan err-chan] #(.destroy process))
     (->process process close-chan in-chan out-chan err-chan)))
  ([process]
   (p/vthread
    (let [process (make-process process)
          input-stream (.getInputStream process)
          output-stream (.getOutputStream process)
          error-stream (.getErrorStream process)]
      (make-process-stream process input-stream output-stream error-stream)))))

;;; socket

(defn make-address
  ([address]
   (if (instance? InetAddress address)
     address
     (if-not (vector? address)
       (InetAddress/getByName address)
       (apply make-address address))))
  ([] (InetAddress/getLocalHost)))

(defn make-socket-address
  ([address]
   (if (instance? SocketAddress address)
     address
     (if-not (vector? address)
       (InetSocketAddress. address)
       (apply make-socket-address address))))
  ([address port]
   (InetSocketAddress. address port)))

(def make-proxy-type
  {:direct Proxy$Type/DIRECT
   :http Proxy$Type/HTTP
   :socks Proxy$Type/SOCKS})

(defn make-proxy
  ([proxy host port]
   (if (= proxy :default)
     (-> (ProxySelector/getDefault)
         (.select (URI. "http" "" host port "" "" ""))
         rand-nth)
     (let [[type address] proxy]
       (Proxy. (make-proxy-type type) (make-socket-address address))))))

(defn make-socket
  ([socket]
   (if (instance? Socket socket)
     socket
     (do
       (assert (vector? socket))
       (apply make-socket socket))))
  ([host port]
   (Socket. host port))
  ([host port & {:keys [timeout proxy ssl] :or {ssl false}}]
   (let [socket (if-not proxy
                  (Socket.)
                  (Socket. (make-proxy proxy host port)))]
     (if-not timeout
       (.connect socket (make-socket-address host port))
       (.connect socket (make-socket-address host port) timeout))
     (if-not ssl
       socket
       (-> (SSLSocketFactory/getDefault)
           (.createSocket socket host port true))))))

(defn make-socket-stream
  ([socket input-stream output-stream]
   (let [close-chan (csp/chan)
         in-chan (input-stream->chan input-stream close-chan)
         out-chan (output-stream->chan output-stream close-chan)]
     (go-close close-chan [in-chan out-chan] #(.close socket))
     (b/->stream close-chan in-chan out-chan)))
  ([socket]
   (p/vthread
    (let [socket (make-socket socket)
          input-stream (.getInputStream socket)
          output-stream (.getOutputStream socket)]
      (make-socket-stream socket input-stream output-stream)))))
