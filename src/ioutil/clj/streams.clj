(ns ioutil.clj.streams
  (:require [promesa.core :as p]
            [promesa.exec :as pe]
            [promesa.exec.csp :as csp]
            [ioutil.bytes :as b])
  (:import java.time.Duration
           [java.io
            InputStream OutputStream File FileInputStream FileOutputStream]
           [java.net
            SocketAddress InetAddress InetSocketAddress
            Proxy Proxy$Type ProxySelector URI Socket]
           javax.net.ssl.SSLSocketFactory
           [java.net.http
            HttpClient HttpClient$Builder HttpClient$Version HttpClient$Redirect
            HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]))

(defn go-close [close-chan chans callback]
  (p/vthread
   (p/let [_ (csp/take close-chan)]
     (csp/close! close-chan)
     (doseq [chan chans]
       (csp/close! chan))
     (callback))))

(defn make-duration-time [time]
  (if (instance? Duration time)
    time
    (Duration/ofMillis time)))

(defn make-int-time [time]
  (if (instance? Duration time)
    (.toMillis time)
    time))

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
         in-chan (input-stream->chan input-stream close-chan)]
     (go-close close-chan [in-chan] #(.close input-stream))
     (b/->stream close-chan in-chan nil))))

(defn make-file-output-stream [file]
  (p/vthread
   (let [output-stream (make-output-file file)
         close-chan (csp/chan)
         out-chan (output-stream->chan output-stream close-chan)]
     (go-close close-chan [out-chan] #(.close output-stream))
     (b/->stream close-chan nil out-chan))))

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

(defrecord process [process close-chan in-chan out-chan err-chan]
  b/ICloseable
  (-close [this]
    (csp/close! (:close-chan this))))

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

(defn make-process-error-reader [process]
  (b/make-chan-reader (:err-chan process)))

;;; address

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

(def proxy-type
  {:direct Proxy$Type/DIRECT
   :http   Proxy$Type/HTTP
   :socks  Proxy$Type/SOCKS})

(defn make-proxy
  [proxy host port]
  (if (= proxy :default)
    (-> (ProxySelector/getDefault)
        (.select (URI. "http" "" host port "" "" ""))
        rand-nth)
    (let [[type address] proxy]
      (Proxy. (proxy-type type) (make-socket-address address)))))

(defn make-proxy-selector
  [proxy]
  (condp = proxy
    :no-proxy HttpClient$Builder/NO_PROXY
    :default  (ProxySelector/getDefault)
    (ProxySelector/of (make-socket-address proxy))))

(defn make-uri
  ([uri]
   (if (instance? URI uri)
     uri
     (if-not (vector? uri)
       (URI. uri)
       (apply make-uri uri))))
  ([scheme & {:keys [user host port path query fragment]
              :or {user "" host "" port 80 path "" query "" fragment ""}}]
   (URI. scheme user host port path query fragment)))

;;; socket

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
       (.connect socket (make-socket-address host port) (make-int-time timeout)))
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

;;; http

(def ^:dynamic *connect-timeout* (Duration/ofSeconds 2))
(def ^:dynamic *request-timeout* (Duration/ofSeconds 5))

(def http-version
  {:http1.1 HttpClient$Version/HTTP_1_1
   :http2   HttpClient$Version/HTTP_2})

(def http-method
  {:get     "GET"
   :head    "HEAD"
   :post    "POST"
   :put     "PUT"
   :delete  "DELETE"
   :options "OPTIONS"
   :trace   "TRACE"
   :patch   "PATCH"})

(def http-redirect
  {:default HttpClient$Redirect/NORMAL
   :always  HttpClient$Redirect/ALWAYS
   :never   HttpClient$Redirect/NEVER})

(defn make-http-body [body]
  (cond (string? body) (HttpRequest$BodyPublishers/ofString body)
        (bytes? body) (HttpRequest$BodyPublishers/ofByteArray body)
        :else (HttpRequest$BodyPublishers/noBody)))

(defn make-http-client
  [& {:keys [executor proxy timeout version redirect]
      :or {executor pe/default-vthread-executor
           timeout *connect-timeout*}}]
  (-> (cond-> (HttpClient/newBuilder)
        executor (.executor @executor)
        proxy    (.proxy (make-proxy-selector proxy))
        timeout  (.connectTimeout (make-duration-time timeout))
        version  (.version (http-version version))
        redirect (.followRedirects (http-redirect redirect)))
      (.build)))

(defn make-http-request
  [uri & {:keys [method body headers timeout version]
          :or {method :get timeout *request-timeout*}}]
  (let [builder (-> (HttpRequest/newBuilder)
                    (.uri (make-uri uri))
                    (.method (http-method method) (make-http-body body)))
        builder (if-not headers
                  builder
                  (reduce
                   (fn [builder [k v]] (.setHeader builder k v))
                   builder headers))]
    (-> (cond-> builder
          timeout (.timeout (make-duration-time timeout))
          version (.version (http-version version)))
        (.build))))

(defmulti http-send (fn [rtype client request] rtype))

(defmethod http-send :discard [rtype client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/discarding)))

(defmethod http-send :str [rtype client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofString)))

(defmethod http-send :bytes [rtype client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofByteArray)))

(defmethod http-send :input-stream [rtype client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofInputStream)))
