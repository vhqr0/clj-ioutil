(ns clj-ioutil.bytes.clj.streams
  (:require [clojure.java.io :as io]
            [promesa.core :as p]
            [promesa.exec.csp :as csp]
            [clj-ioutil.bytes :as b])
  (:import java.nio.ByteBuffer
           java.util.Arrays
           java.time.Duration
           [java.io InputStream OutputStream]
           [java.net SocketAddress InetAddress InetSocketAddress Proxy Proxy$Type Socket]
           javax.net.ssl.SSLSocketFactory))

(defn ^Duration duration-time [time]
  (if (instance? Duration time)
    time
    (Duration/ofMillis time)))

(defn ^long int-time [time]
  (if-not (instance? Duration time)
    time
    (.toMillis ^Duration time)))

;;; io

(def ^:dynamic *input-buffer-size* 8192)

(defn input-stream->readfn
  ([is]
   (input-stream->readfn is (b/bmake *input-buffer-size*)))
  ([^InputStream is ^bytes buffer]
   (fn []
     (let [n (.read is buffer)]
       (when (pos? n)
         (Arrays/copyOf buffer n))))))

(defn output-stream->writefn [^OutputStream os]
  (fn
    ([]
     (.close os))
    ([^bytes b]
     (.write os b)
     (.flush os))))

(defn input-stream->chan
  ([is cch]
   (input-stream->chan is cch (csp/chan)))
  ([is cch ch]
   (let [read (input-stream->readfn is)]
     (p/vthread
      (-> (p/loop [b (p/vthread (read))]
            (if-not b
              (csp/close! ch)
              (p/let [ok (csp/put ch b)]
                (assert ok)
                (p/recur (p/vthread (read))))))
          (p/catch #(csp/close! cch %))))
     ch)))

(defn output-stream->chan
  ([os cch]
   (output-stream->chan os cch (csp/chan :buf 1 :xf (remove b/bempty?))))
  ([os cch ch]
   (let [write (output-stream->writefn os)]
     (p/vthread
      (-> (p/loop [b (csp/take ch)]
            (if-not b
              (write)
              (do
                (write b)
                (p/recur (csp/take ch)))))
          (p/catch #(csp/close! cch %))))
     ch)))

(defn input-stream->read-stream [^InputStream is]
  (let [cch (csp/chan)
        ich (input-stream->chan is cch)]
    (b/create-stream cch ich nil #(.close is))))

(defn output-stream->write-stream [^OutputStream os]
  (let [cch (csp/chan)
        och (output-stream->chan os cch)]
    (b/create-stream cch nil och #(.close os))))

(extend-protocol b/IStreamFactory
  InputStream
  (b/stream [this] (input-stream->read-stream this))
  OutputStream
  (b/stream [this] (output-stream->write-stream this)))

(defn read-stream [x & opts]
  (p/vthread
   (input-stream->read-stream (apply io/input-stream x opts))))

(defn write-stream [x & opts]
  (p/vthread
   (output-stream->write-stream (apply io/output-stream x opts))))

(comment
  (do
    (def f ".gitignore")
    @(p/let [s (read-stream (io/file f))
             r (b/stream-reader s)
             [r it] (b/read-all r)]
       (println (b/bytes->str it))))
  (do
    (def u "http://bing.com")
    @(p/let [s (read-stream (io/as-url u))
             r (b/stream-reader s)
             [r it] (b/read r)]
       (println (b/bytes->str it))))
  (do
    (def f "target/test.txt")
    @(p/let [s (write-stream (io/file f))
             w (b/stream-writer s)
             w (b/write w (b/str->bytes "hello\n"))
             w (b/flush w)
             w (b/shutdown w)])))

;;; socket

(defn ^InetAddress inet-address
  ([]
   (InetAddress/getLocalHost))
  ([addr]
   (if (bytes? addr)
     (InetAddress/getByAddress ^bytes addr)
     (InetAddress/getByName ^String addr))))

(defn ^SocketAddress socket-address
  ([]
   (InetSocketAddress. 0))
  ([^long port]
   (InetSocketAddress. port))
  ([host ^long port]
   (if (instance? InetAddress host)
     (InetSocketAddress. ^InetAddress host port)
     (InetSocketAddress. ^String host port))))

(def socket-proxy-type
  {:direct Proxy$Type/DIRECT
   :http   Proxy$Type/HTTP
   :socks  Proxy$Type/SOCKS})

(defn ^Proxy socket-proxy
  ([]
   Proxy/NO_PROXY)
  ([type addr]
   (Proxy. (socket-proxy-type type) addr)))

(comment
  (do
    (println (str (inet-address "localhost")))
    (println (str (inet-address "bing.com")))
    (println (str (inet-address "127.0.0.1")))
    (println (str (socket-address 80)))
    (println (str (socket-address "bing.com" 80)))
    (println (str (socket-proxy)))
    (println (str (socket-proxy :socks (socket-address 1080))))
    (println (str (socket-proxy :http (socket-address 8080))))))

(def ^:dynamic *socket-connect-timeout* (Duration/ofSeconds 5))

(defn ^Socket socket
  ([^SocketAddress addr]
   (let [^Socket sock (Socket.)]
     (.connect sock addr)
     sock))
  ([addr &
    {:keys [timeout proxy ssl]
     :or {timeout *socket-connect-timeout* ssl false}}]
   (let [^InetSocketAddress addr addr
         ^Socket sock (if-not proxy (Socket.) (Socket. proxy))]
     (if-not timeout
       (.connect sock addr)
       (.connect sock addr (int-time timeout)))
     (if-not ssl
       sock
       (let [^SSLSocketFactory f (SSLSocketFactory/getDefault)]
         (.createSocket f sock (.getHostName addr) (.getPort addr) true))))))

(defn socket->stream [^Socket sock]
  (let [is (.getInputStream sock)
        os (.getOutputStream sock)
        cch (csp/chan)
        ich (input-stream->chan is cch)
        och (output-stream->chan os cch)]
    (b/create-stream cch ich och #(.close sock))))

(extend-protocol b/IStreamFactory
  Socket
  (b/stream [this] (socket->stream this))
  SocketAddress
  (b/stream [this] (p/vthread (socket->stream (socket this)))))

(defn socket-stream [addr & opts]
  (p/vthread
    (socket->stream (apply socket addr opts))))

(comment
  (do
    (def h "bing.com")
    (def s @(socket-stream (socket-address h 80)))
    @(p/let [w (b/stream-writer s)
             w (b/write w (b/str->bytes (str "GET / HTTP/1.1\r\nHost: " h "\r\n\r\n")))
             w (b/flush w)])
    @(p/let [r (b/stream-reader s)
             [r it] (b/read r)]
       (println (b/bytes->str it)))))
