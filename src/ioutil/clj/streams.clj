(ns ioutil.clj.streams
  (:require [promesa.core :as p]
            [promesa.exec :as pe]
            [promesa.exec.csp :as csp]
            [ioutil.bytes :as b])
  (:import java.time.Duration
           java.nio.ByteBuffer
           [java.io
            InputStream OutputStream File FileInputStream FileOutputStream]
           [java.net
            SocketAddress InetAddress InetSocketAddress
            Proxy Proxy$Type ProxySelector URI Socket]
           javax.net.ssl.SSLSocketFactory
           [java.net.http
            HttpClient HttpClient$Builder HttpClient$Version HttpClient$Redirect
            HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers
            WebSocket WebSocket$Listener]))

(defn go-close [close-chan chans callback]
  (p/vthread
   (-> (csp/take close-chan)
       (p/finally
         (fn [_ _]
           (csp/close! close-chan)
           (doseq [chan chans]
             (csp/close! chan))
           (callback))))))

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
       (when (pos? n)
         (b/sub buffer 0 n))))))

(defn output-stream->writefn [output-stream]
  (fn [b]
    (.write output-stream b)
    (.flush output-stream)))

(defn input-stream->chan
  ([input-stream close-chan]
   (input-stream->chan input-stream close-chan (csp/chan)))
  ([input-stream close-chan chan]
   (let [read (input-stream->readfn input-stream)]
     (p/vthread
      (-> (p/loop [b (p/vthread (read))]
            (if-not b
              (csp/close! chan)
              (p/let [ok (csp/put chan b)]
                (assert ok)
                (p/recur (p/vthread (read))))))
          (p/catch #(csp/close! close-chan %))))
     chan)))

(defn output-stream->chan
  ([output-stream close-chan]
   (output-stream->chan output-stream close-chan
                        (csp/chan :buf 1 :xf (remove b/bempty?))))
  ([output-stream close-chan chan]
   (let [write (output-stream->writefn output-stream)]
     (p/vthread
      (-> (p/loop [b (csp/take chan)]
            (when b
              (when-not (b/bempty? b)
                (write b))
              (p/recur (csp/take chan))))
          (p/catch #(csp/close! close-chan %))))
     chan)))

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

(defn make-file-input-stream
  ([file]
   (if (instance? InputStream file)
     file
     (if-not (vector? file)
       (FileInputStream. file)
       (apply make-file-input-stream file)))))

(defn make-file-output-stream
  ([file]
   (if (instance? OutputStream file)
     file
     (if-not (vector? file)
       (FileOutputStream. file)
       (apply make-file-output-stream file))))
  ([file & {:keys [append] :or {append false}}]
   (FileOutputStream. file append)))

(defn make-file-read-stream [file]
  (p/vthread
   (let [input-stream (make-file-input-stream file)
         close-chan (csp/chan)
         in-chan (input-stream->chan input-stream close-chan)]
     (go-close close-chan [in-chan] #(.close input-stream))
     (b/->stream input-stream close-chan in-chan nil))))

(defn make-file-write-stream [file]
  (p/vthread
   (let [output-stream (make-file-output-stream file)
         close-chan (csp/chan)
         out-chan (output-stream->chan output-stream close-chan)]
     (go-close close-chan [out-chan] #(.close output-stream))
     (b/->stream output-stream close-chan nil out-chan))))

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

(defn make-process-stream
  ([process input-stream output-stream error-stream]
   (let [close-chan (csp/chan)
         in-chan (input-stream->chan input-stream close-chan)
         out-chan (output-stream->chan output-stream close-chan)
         err-chan (output-stream->chan error-stream close-chan)]
     (go-close close-chan [in-chan out-chan err-chan] #(.destroy process))
     (b/->error-stream process close-chan in-chan out-chan err-chan)))
  ([process]
   (p/vthread
    (let [process (make-process process)
          input-stream (.getInputStream process)
          output-stream (.getOutputStream process)
          error-stream (.getErrorStream process)]
      (make-process-stream process input-stream output-stream error-stream)))))

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
     (b/->stream socket close-chan in-chan out-chan)))
  ([socket]
   (p/vthread
    (let [socket (make-socket socket)
          input-stream (.getInputStream socket)
          output-stream (.getOutputStream socket)]
      (make-socket-stream socket input-stream output-stream)))))

;;; http

(def ^:dynamic *http-connect-timeout* (Duration/ofSeconds 2))
(def ^:dynamic *http-request-timeout* (Duration/ofSeconds 5))

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
           timeout *http-connect-timeout*}}]
  (-> (cond-> (HttpClient/newBuilder)
        executor (.executor @executor)
        proxy    (.proxy (make-proxy-selector proxy))
        timeout  (.connectTimeout (make-duration-time timeout))
        version  (.version (http-version version))
        redirect (.followRedirects (http-redirect redirect)))
      (.build)))

(defn- http-builder-add-headers [builder headers]
  (reduce
   (fn [builder [k v]] (.header builder k v))
   builder headers))

(defn make-http-request
  [uri & {:keys [method body headers timeout version]
          :or {method :get timeout *http-request-timeout*}}]
  (-> (cond-> (HttpRequest/newBuilder)
        headers (http-builder-add-headers headers)
        timeout (.timeout (make-duration-time timeout))
        version (.version (http-version version)))
      (.method (http-method method) (make-http-body body))
      (.uri (make-uri uri))
      (.build)))

(defmulti http-send (fn [rtype client request] rtype))

(defmethod http-send :discard [rtype client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/discarding)))

(defmethod http-send :text [rtype client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofString)))

(defmethod http-send :bin [rtype client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofByteArray)))

(defmethod http-send :stream [rtype client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofInputStream)))

(defn make-http-read-stream [client request]
  (p/let [response (http-send :stream client request)]
    (let [input-stream (.body response)
          close-chan (csp/chan)
          in-chan (input-stream->chan input-stream close-chan)]
      (go-close close-chan [in-chan] #(.close input-stream))
      (b/->stream response close-chan in-chan nil))))

;;; websocket

(def ^:dynamic *websocket-connect-timeout* (Duration/ofSeconds 5))

(defn- websocket-builder-add-subprotocols [builder subprotocols]
  (.subprotocols builder (first subprotocols)
                 (into-array java.lang.String (rest subprotocols))))

(defn make-websocket
  [client uri listener & {:keys [headers subprotocols timeout]
                          :or {timeout *websocket-connect-timeout*}}]
  (-> (cond-> (.newWebSocketBuilder client)
        headers (http-builder-add-headers headers)
        subprotocols (websocket-builder-add-subprotocols subprotocols)
        timeout (.connectTimeout (make-duration-time timeout)))
      (.buildAsync (make-uri uri) listener)))

(defn- copy-byte-buffer [bb]
  (let [b (b/make-bytes (.remaining bb))]
    (.get bb b)
    b))

(def ^:dynamic *websocket-chan-size* 1024)

(defn make-websocket-in-chan
  ([close-chan]
   (make-websocket-in-chan close-chan (csp/chan :buf *websocket-chan-size*)))
  ([close-chan chan]
   (let [texts (volatile! [])
         bins (volatile! [])
         listener
         (reify WebSocket$Listener
           (onText [this websocket data last]
             (try
               (vswap! texts conj (str data))
               (when last
                 (let [ok (csp/put! chan (apply str @texts))]
                   (vreset! texts [])
                   (assert ok)))
               (catch Exception err
                 (csp/close! close-chan err)))
             nil)
           (onBinary [this websocket data last]
             (try
               (vswap! bins conj (copy-byte-buffer data))
               (when last
                 (let [ok (csp/put! chan (apply b/concat @bins))]
                   (vreset! bins [])
                   (assert ok)))
               (catch Exception err
                 (csp/close! close-chan err)))
             nil)
           (onClose [this websocket code reason]
             (csp/close! chan)
             nil)
           (onError [this websocket err]
             (csp/close! close-chan err)))]
     [chan listener])))

(defn make-websocket-out-chan
  ([websocket close-chan]
   (make-websocket-out-chan websocket close-chan (csp/chan)))
  ([websocket close-chan chan]
   (p/vthread
    (-> (p/loop [data (csp/take chan)]
          (cond (string? data) (p/do
                                 (.sendText websocket (.subSequence data 0 (count data)) true)
                                 (p/recur (csp/take chan)))
                (bytes? data) (p/do
                                (.sendBinary websocket (ByteBuffer/wrap data) true)
                                (p/recur (csp/take chan)))
                :else (do (assert (not data))
                          (.sendClose websocket WebSocket/NORMAL_CLOSURE ""))))
        (p/catch #(csp/close! close-chan %))))
   chan))

(defn make-websocket-stream
  [client uri & opts]
  (let [close-chan (csp/chan)
        [in-chan listener] (make-websocket-in-chan close-chan)]
    (p/let [websocket (apply make-websocket client uri listener opts)]
      (let [out-chan (make-websocket-out-chan websocket close-chan)]
        (go-close close-chan [in-chan out-chan] #(.abort websocket))
        (b/->stream websocket close-chan in-chan out-chan)))))

(defn websocket-send
  ([websocket]
   (csp/close! (:out-chan websocket)))
  ([websocket data]
   (p/let [ok (csp/put (:out-chan websocket) data)]
     (assert ok))))

(defn websocket-recv [websocket]
  (csp/take (:in-chan websocket)))
