(ns clj-ioutil.clj.streams
  (:require [promesa.core :as p]
            [promesa.exec :as pe]
            [promesa.exec.csp :as csp]
            [clj-ioutil.bytes.impl :as bi]
            [clj-ioutil.bytes :as b])
  (:import java.time.Duration
           java.nio.ByteBuffer
           [java.io
            InputStream OutputStream File FileDescriptor FileInputStream FileOutputStream]
           [java.net
            SocketAddress InetAddress InetSocketAddress URI Socket
            Proxy Proxy$Type ProxySelector Authenticator PasswordAuthentication
            CookieHandler CookieManager CookieStore CookiePolicy]
           javax.net.ssl.SSLSocketFactory
           [java.net.http
            HttpClient HttpClient$Builder HttpClient$Version HttpClient$Redirect
            HttpRequest HttpRequest$Builder HttpRequest$BodyPublisher  HttpRequest$BodyPublishers
            HttpResponse HttpResponse$BodyHandlers
            WebSocket WebSocket$Builder WebSocket$Listener]))

(defn go-close [close-chan chans callback]
  (p/vthread
   (-> (csp/take close-chan)
       (p/finally
         (fn [_ _]
           (csp/close! close-chan)
           (doseq [chan chans]
             (csp/close! chan))
           (callback))))))

(defn ^Duration make-duration-time [time]
  (if (instance? Duration time)
    time
    (Duration/ofMillis time)))

(defn ^long make-int-time [time]
  (if (instance? Duration time)
    (.toMillis ^Duration time)
    time))

;;; io

(def ^:dynamic *input-buffer-size* 8192)

(defn input-stream->readfn
  ([input-stream]
   (input-stream->readfn input-stream (b/bmake *input-buffer-size*)))
  ([^InputStream input-stream ^bytes buffer]
   (fn []
     (let [n (.read input-stream buffer)]
       (when (pos? n)
         ;; use pure sub here
         (bi/pure-sub buffer 0 n))))))

(defn output-stream->writefn [^OutputStream output-stream]
  (fn
    ([]
     (.close output-stream))
    ([^bytes b]
     (.write output-stream b)
     (.flush output-stream))))

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
   (output-stream->chan output-stream close-chan (csp/chan :buf 1 :xf (remove b/bempty?))))
  ([output-stream close-chan chan]
   (let [write (output-stream->writefn output-stream)]
     (p/vthread
      (-> (p/loop [b (csp/take chan)]
            (if-not b
              (write)
              (do
                (write b)
                (p/recur (csp/take chan)))))
          (p/catch #(csp/close! close-chan %))))
     chan)))

(defn input-stream->read-stream [^InputStream input-stream]
  (let [close-chan (csp/chan)
        in-chan (input-stream->chan input-stream close-chan)]
    (go-close close-chan [in-chan] #(.close input-stream))
    (b/->stream input-stream close-chan in-chan nil)))

(defn output-stream->write-stream [^OutputStream output-stream]
  (let [close-chan (csp/chan)
        out-chan (output-stream->chan output-stream close-chan)]
    (go-close close-chan [out-chan] #(.close output-stream))
    (b/->stream output-stream close-chan nil out-chan)))

;;; file

(defn ^File make-file
  ([file]
   (if (instance? File file)
     file
     (if-not (vector? file)
       (File. ^String file)
       (apply make-file file))))
  ([]
   (File. "."))
  ([parent ^String child]
   (if (instance? File parent)
     (File. ^File parent child)
     (File. ^String parent child))))

(comment
  (do
    (println (str (make-file ".gitignore")))
    (println (str (make-file (System/getenv "HOME") "work")))))

(defn ^InputStream make-file-input-stream
  ([file]
   (if (instance? InputStream file)
     file
     (if-not (vector? file)
       (cond (instance? File file) (FileInputStream. ^File file)
             (instance? FileDescriptor file) (FileInputStream. ^FileDescriptor file)
             :else (FileInputStream. ^String file))
       (apply make-file-input-stream file)))))

(defn ^OutputStream make-file-output-stream
  ([file]
   (if (instance? OutputStream file)
     file
     (if-not (vector? file)
       (cond (instance? File file) (FileOutputStream. ^File file)
             (instance? FileDescriptor file) (FileOutputStream. ^FileDescriptor file)
             :else (FileOutputStream. ^String file))
       (apply make-file-output-stream file))))
  ([file & {:keys [^boolean append] :or {append false}}]
   (if (instance? File file)
     (FileOutputStream. ^File file append)
     (FileOutputStream. ^String file append))))

(defn make-file-read-stream [file]
  (p/vthread
   (let [^InputStream input-stream (make-file-input-stream file)]
     (input-stream->read-stream input-stream))))

(defn make-file-write-stream [file]
  (p/vthread
   (let [^OutputStream output-stream (make-file-output-stream file)]
     (output-stream->write-stream output-stream))))

(comment
  (do
    (def f ".gitignore")
    (def s @(make-file-read-stream f))
    @(p/let [r (b/make-stream-reader s)
             [r it] (b/read-all r)]
       (println (b/bytes->str it))))
  (do
    (def f "target/test.txt")
    (def s @(make-file-write-stream f))
    @(p/let [w (b/make-stream-writer s)
             w (b/write w (b/str->bytes "hello\n"))]
       (b/flush w))))

;;; address

(defn ^InetAddress make-address
  ([address]
   (if (instance? InetAddress address)
     address
     (if-not (vector? address)
       (if (bytes? address)
         (InetAddress/getByAddress ^bytes address)
         (InetAddress/getByName ^String address))
       (apply make-address address))))
  ([]
   (InetAddress/getLocalHost)))

(comment
  (do
    (println (str (make-address "localhost")))
    (println (str (make-address "bing.com")))
    (println (str (make-address "127.0.0.1")))))

(defn ^SocketAddress make-socket-address
  ([address]
   (if (instance? SocketAddress address)
     address
     (if-not (vector? address)
       (InetSocketAddress. address)
       (apply make-socket-address address))))
  ([]
   (InetSocketAddress. 0))
  ([address ^long port]
   (if (instance? InetAddress address)
     (InetSocketAddress. ^InetAddress address port)
     (InetSocketAddress. ^String address port))))

(comment
  (do
    (println (str (make-socket-address 80)))
    (println (str (make-socket-address "bing.com" 80)))))

(defn ^URI make-uri
  ([uri]
   (if (instance? URI uri)
     uri
     (if-not (vector? uri)
       (URI. uri)
       (apply make-uri uri))))
  ([scheme host & {:keys [user port path query fragment]}]
   (let [port (or port (case scheme "http" 80 "https" 443))]
     (URI. scheme user host port path query fragment))))

(comment
  (do
    (println (str (make-uri "http://bing.com?s=clojure")))
    (println (str (make-uri "http" :host "bing.com" :query "s=clojure")))))

;;; proxy/auth/cookie

(def proxy-type
  {:direct Proxy$Type/DIRECT
   :http   Proxy$Type/HTTP
   :socks  Proxy$Type/SOCKS})

(defn ^Proxy make-proxy
  ([proxy]
   (if (instance? Proxy proxy)
     proxy
     (if (= proxy :direct)
       Proxy/NO_PROXY
       (do
         (assert (vector? proxy))
         (apply make-proxy proxy)))))
  ([]
   Proxy/NO_PROXY)
  ([type address]
   (Proxy. (proxy-type type) (make-socket-address address))))

(defn select-proxy [proxy host port]
  (cond (= proxy :default)
        (select-proxy (ProxySelector/getDefault) host port)
        (instance? ProxySelector proxy)
        (let [^URI uri (URI. "http"  "" host port "" "" "")]
          (rand-nth (.select ^ProxySelector proxy uri)))
        :else
        (make-proxy proxy)))

(defn ^ProxySelector make-proxy-selector
  ([proxy]
   (if (instance? ProxySelector proxy)
     proxy
     (if (= proxy :default)
       (ProxySelector/getDefault)
       (let [proxies (java.util.Arrays/asList (into-array Proxy [(make-proxy proxy)]))]
         (clojure.core/proxy [ProxySelector] []
           (select [uri]
             proxies))))))
  ([]
   (ProxySelector/getDefault)))

(comment
  (do
    (defn- f [proxy host]
      (let [selector (make-proxy-selector proxy)]
        (rand-nth (.select selector (make-uri "http" :host host)))))
    (println (f :default "bing.com"))
    (println (f :default "google.com"))
    (println (f :direct "bing.com"))
    (println (f [:socks 1080] "google.com"))
    (println (f [:http 1080] "google.com")))
  (do
    (println (str (select-proxy :default "bing.com" 80)))
    (println (str (select-proxy :default "google.com" 80)))
    (println (str (select-proxy :direct "bing.com" 80)))
    (println (str (select-proxy [:socks 1080] "google.com" 80)))
    (println (str (select-proxy [:http 1080] "google.com" 80)))))

(defn ^Authenticator make-auth
  ([auth]
   (if (instance? Authenticator auth)
     auth
     (do
       (assert (vector? auth))
       (apply make-auth auth))))
  ([]
   (Authenticator/getDefault))
  ([^String username ^String password]
   (let [auth (PasswordAuthentication. username (.toCharArray password))]
     (proxy [Authenticator] []
       (getPasswordAuthentication []
         auth)))))

(def cookie-policy
  {:accept-all CookiePolicy/ACCEPT_ALL
   :accept-none CookiePolicy/ACCEPT_NONE
   :accept-original-server CookiePolicy/ACCEPT_ORIGINAL_SERVER})

(defn ^CookieHandler make-cookie-manager
  ([cookie]
   (if (instance? CookieHandler cookie)
     cookie
     (if (= cookie :default)
       (CookieHandler/getDefault)
       (do
         (assert (vector? cookie))
         (apply make-cookie-manager cookie)))))
  ([]
   (CookieManager.))
  ([store & {:keys [policy] :or {policy :accept-all}}]
   (let [^CookieStore store (if-not (instance? CookieManager store)
                              store
                              (.getCookieStore ^CookieManager store))]
     (CookieManager. store (cookie-policy policy)))))

;;; socket

(def ^:dynamic *socket-connect-timeout* (Duration/ofSeconds 5))

(defn ^Socket make-socket
  ([socket]
   (if (instance? Socket socket)
     socket
     (do
       (assert (vector? socket))
       (apply make-socket socket))))
  ([host ^long port]
   (if (instance? InetAddress host)
     (Socket. ^InetAddress host port)
     (Socket. ^String host port)))
  ([host port & {:keys [timeout proxy ssl]
                 :or {timeout *socket-connect-timeout* ssl false}}]
   (let [^Socket socket (if-not proxy
                          (Socket.)
                          (Socket. (select-proxy proxy host port)))]
     (if-not timeout
       (.connect socket (make-socket-address host port))
       (.connect socket (make-socket-address host port) (make-int-time timeout)))
     (if-not ssl
       socket
       (let [^SSLSocketFactory f (SSLSocketFactory/getDefault)]
         (.createSocket f socket ^String host ^int port true))))))

(defn make-socket-stream [^Socket socket]
  (p/vthread
   (let [^Socket socket (make-socket socket)
         input-stream (.getInputStream socket)
         output-stream (.getOutputStream socket)
         close-chan (csp/chan)
         in-chan (input-stream->chan input-stream close-chan)
         out-chan (output-stream->chan output-stream close-chan)]
     (go-close close-chan [in-chan out-chan] #(.close socket))
     (b/->stream socket close-chan in-chan out-chan))))

(comment
  (do
    (def h "bing.com")
    (def s @(make-socket-stream [h 80]))
    @(p/let [w (b/make-stream-writer s)
             w (b/write w (b/str->bytes (str "GET / HTTP/1.1\r\nHost: " h "\r\n\r\n")))]
       (b/flush w))
    @(p/let [r (b/make-stream-reader s)
             [r it] (b/read r)]
       (println (b/bytes->str it)))))

;;; http

(def ^:dynamic *http-connect-timeout* (Duration/ofSeconds 5))
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

(defn ^HttpRequest$BodyPublisher make-http-body [body]
  (cond (string? body) (HttpRequest$BodyPublishers/ofString body)
        (bytes? body) (HttpRequest$BodyPublishers/ofByteArray body)
        :else (HttpRequest$BodyPublishers/noBody)))

(defn- http-client-builder-set-executor [^HttpClient$Builder builder executor]
  (.executor builder @executor))

(defn- http-client-builder-set-proxy [^HttpClient$Builder builder proxy]
  (.proxy builder (make-proxy-selector proxy)))

(defn- http-client-builder-set-auth [^HttpClient$Builder builder auth]
  (.authenticator builder (make-auth auth)))

(defn- http-client-builder-set-cookie [^HttpClient$Builder builder cookie]
  (.cookieHandler builder (make-cookie-manager cookie)))

(defn- http-client-builder-set-version [^HttpClient$Builder builder version]
  (.version builder (http-version version)))

(defn- http-client-builder-set-timeout [^HttpClient$Builder builder timeout]
  (.connectTimeout builder (make-duration-time timeout)))

(defn- http-client-builder-set-redirect [^HttpClient$Builder builder redirect]
  (.followRedirects builder (http-redirect redirect)))

(defn- http-client-builder-build [^HttpClient$Builder builder]
  (.build builder))

(defn ^HttpClient make-http-client
  [& {:keys [executor proxy auth cookie version timeout redirect]
      :or {executor pe/default-vthread-executor
           timeout *http-connect-timeout*}}]
  (-> (cond-> (HttpClient/newBuilder)
        executor (http-client-builder-set-executor executor)
        proxy    (http-client-builder-set-proxy proxy)
        auth     (http-client-builder-set-auth auth)
        cookie   (http-client-builder-set-cookie cookie)
        version  (http-client-builder-set-version version)
        timeout  (http-client-builder-set-timeout timeout)
        redirect (http-client-builder-set-redirect redirect))
      http-client-builder-build))

(defn- http-request-builder-add-headers [^HttpRequest$Builder builder headers]
  (reduce
   (fn [^HttpRequest$Builder builder [k v]]
     (.header builder (if-not (keyword? k) k (name k)) v))
   builder headers))

(defn- http-request-builder-set-version [^HttpRequest$Builder builder version]
  (.version builder (http-version version)))

(defn- http-request-builder-set-timeout [^HttpRequest$Builder builder timeout]
  (.timeout builder (make-duration-time timeout)))

(defn- http-request-builder-set-method [^HttpRequest$Builder builder method body]
  (.method builder (http-method method) (make-http-body body)))

(defn- http-request-builder-set-uri [^HttpRequest$Builder builder uri]
  (.uri builder (make-uri uri)))

(defn- http-request-builder-build [^HttpRequest$Builder builder]
  (.build builder))

(defn ^HttpRequest make-http-request
  [uri & {:keys [method body headers version timeout]
          :or {method :get timeout *http-request-timeout*}}]
  (-> (cond-> (HttpRequest/newBuilder)
        headers (http-request-builder-add-headers headers)
        version (http-request-builder-set-version version)
        timeout (http-request-builder-set-timeout timeout))
      (http-request-builder-set-method method body)
      (http-request-builder-set-uri uri)
      http-request-builder-build))

(defmulti http-send (fn [rtype client request] rtype))

(defmethod http-send :discard [rtype ^HttpClient client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/discarding)))

(defmethod http-send :text [rtype ^HttpClient client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofString)))

(defmethod http-send :bin [rtype ^HttpClient client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofByteArray)))

(defmethod http-send :stream [rtype ^HttpClient client request]
  (.sendAsync client request (HttpResponse$BodyHandlers/ofInputStream)))

(defn make-http-read-stream [client uri & opts]
  (let [^HttpRequest request (apply make-http-request uri opts)]
    (p/let [^HttpResponse response (http-send :stream client request)]
      (let [^InputStream input-stream (.body response)
            close-chan (csp/chan)
            in-chan (input-stream->chan input-stream close-chan)]
        (go-close close-chan [in-chan] #(.close input-stream))
        (b/->stream response close-chan in-chan nil)))))

(comment
  (do
    (def u "https://www.bing.com")
    (def s @(make-http-read-stream (make-http-client) u))
    @(p/let [r (b/make-stream-reader s)
             [r it] (b/read-all r)]
       (println (b/bytes->str it)))))

;;; websocket

(def ^:dynamic *websocket-connect-timeout* (Duration/ofSeconds 5))

(defn- websocket-builder-add-headers [^WebSocket$Builder builder headers]
  (reduce
   (fn [^WebSocket$Builder builder [k v]]
     (.header builder (if-not (keyword? k) k (name k)) v))
   builder headers))

(defn- websocket-builder-set-subprotocol [^WebSocket$Builder builder subprotocol]
  (if (string? subprotocol)
    (.subprotocols builder subprotocol (make-array String 0))
    (.subprotocols builder (first subprotocol) (into-array String (rest subprotocol)))))

(defn- websocket-builder-set-timeout [^WebSocket$Builder builder timeout]
  (.connectTimeout builder (make-duration-time timeout)))

(defn- websocket-builder-build [^WebSocket$Builder builder uri listener]
  (.buildAsync builder (make-uri uri) listener))

(defn ^WebSocket make-websocket
  [^HttpClient client uri listener
   & {:keys [headers subprotocol timeout] :or {timeout *websocket-connect-timeout*}}]
  (-> (cond-> (.newWebSocketBuilder client)
        headers     (websocket-builder-add-headers headers)
        subprotocol (websocket-builder-set-subprotocol subprotocol)
        timeout     (websocket-builder-set-timeout timeout))
      (websocket-builder-build uri listener)))

(def ^:dynamic *websocket-chan-size* 1024)

(defn- make-websocket-in-chan
  ([close-chan]
   (make-websocket-in-chan close-chan (csp/chan :buf *websocket-chan-size*)))
  ([close-chan chan]
   (let [texts (volatile! [])
         bins (volatile! [])
         listener
         (reify WebSocket$Listener
           (onText [this websocket data last]
             (vswap! texts conj (str data))
             (when last
               (let [text (apply str @texts)]
                 (vreset! texts [])
                 @(-> (p/let [ok (csp/put chan text)]
                        (assert ok))
                      (p/catch #(csp/close! close-chan %)))))
             nil)
           (onBinary [this websocket data last]
             (vswap! bins conj (bi/pure-sub data))
             (when last
               (let [bin (apply b/concat @bins)]
                 (vreset! bins [])
                 @(-> (p/let [ok (csp/put chan bin)]
                        (assert ok))
                      (p/catch #(csp/close! close-chan %)))))
             nil)
           (onClose [this websocket code reason]
             (csp/close! chan)
             nil)
           (onError [this websocket error]
             (csp/close! close-chan error)))]
     [chan listener])))

(defn- make-websocket-out-chan
  ([websocket close-chan]
   (make-websocket-out-chan websocket close-chan (csp/chan)))
  ([^WebSocket websocket close-chan chan]
   (p/vthread
    (-> (p/loop [data (csp/take chan)]
          (cond (string? data) (p/do
                                 (.sendText websocket (.subSequence ^String data 0 (count data)) true)
                                 (p/recur (csp/take chan)))
                (bytes? data) (p/do
                                (.sendBinary websocket (ByteBuffer/wrap data) true)
                                (p/recur (csp/take chan)))
                :else (do (assert (not data))
                          (.sendClose websocket WebSocket/NORMAL_CLOSURE ""))))
        (p/catch #(csp/close! close-chan %))))
   chan))

(defn make-websocket-stream [client uri & opts]
  (let [close-chan (csp/chan)
        [in-chan listener] (make-websocket-in-chan close-chan)]
    (p/let [^WebSocket websocket (apply make-websocket client uri listener opts)]
      (let [out-chan (make-websocket-out-chan websocket close-chan)]
        (go-close close-chan [in-chan out-chan] #(.abort websocket))
        (b/->stream websocket close-chan in-chan out-chan)))))
