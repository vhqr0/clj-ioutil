(ns clj-ioutil.http.client.impl
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [promesa.exec :as pe]
            [clj-ioutil.bytes.clj.streams :as s]
            [clj-ioutil.http.util :as u]
            [clj-ioutil.http.protocol :as pt])
  (:import java.util.Arrays
           java.time.Duration
           [java.net
            URI Proxy ProxySelector Authenticator PasswordAuthentication
            CookieHandler CookieManager CookieStore CookiePolicy]
           [java.net.http
            HttpClient HttpClient$Builder HttpClient$Version HttpClient$Redirect
            HttpRequest HttpRequest$Builder HttpRequest$BodyPublisher HttpRequest$BodyPublishers
            HttpResponse HttpResponse$BodyHandlers HttpHeaders]))

;;; proxy/auth/cookie

(defn ^ProxySelector proxy-selector
  ([]
   (ProxySelector/getDefault))
  ([prx]
   (let [prxs (Arrays/asList (into-array Proxy [prx]))]
     (proxy [ProxySelector] []
       (select [uri]
         prxs)))))

(defn ^Authenticator authenticator
  ([]
   (Authenticator/getDefault))
  ([^String user ^String pwd]
   (let [auth (PasswordAuthentication. user (.toCharArray pwd))]
     (proxy [Authenticator] []
       (getPasswordAuthentication []
         auth)))))

(def cookie-policy
  {:accept-all CookiePolicy/ACCEPT_ALL
   :accept-none CookiePolicy/ACCEPT_NONE
   :accept-original-server CookiePolicy/ACCEPT_ORIGINAL_SERVER})

(defn ^CookieHandler cookie-handler []
  ([]
   (CookieHandler/getDefault)))

(defn ^CookieHandler cookie-manager
  ([]
   (CookieManager.))
  ([^CookieManager base policy]
   (let [^CookieStore store (.getCookieStore base)]
     (CookieManager. store (cookie-policy policy)))))

;;; client

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

(defn ^HttpRequest$BodyPublisher http-body [body]
  (cond (string? body) (HttpRequest$BodyPublishers/ofString body)
        (bytes? body) (HttpRequest$BodyPublishers/ofByteArray body)
        :else (HttpRequest$BodyPublishers/noBody)))

(defn- cb-set-executor [^HttpClient$Builder builder executor]
  (.executor builder @executor))

(defn- cb-set-proxy [^HttpClient$Builder builder proxy]
  (.proxy builder proxy))

(defn- cb-set-auth [^HttpClient$Builder builder auth]
  (.authenticator builder auth))

(defn- cb-set-cookie [^HttpClient$Builder builder cookie]
  (.cookieHandler builder cookie))

(defn- cb-set-version [^HttpClient$Builder builder version]
  (.version builder (http-version version)))

(defn- cb-set-timeout [^HttpClient$Builder builder timeout]
  (.connectTimeout builder (s/duration-time timeout)))

(defn- cb-set-redirect [^HttpClient$Builder builder redirect]
  (.followRedirects builder (http-redirect redirect)))

(defn- cb-build [^HttpClient$Builder builder]
  (.build builder))

(defn ^HttpClient make-client
  [{:keys [executor proxy auth cookie version timeout redirect]}]
  (-> (cond-> (HttpClient/newBuilder)
        executor (cb-set-executor executor)
        proxy    (cb-set-proxy proxy)
        auth     (cb-set-auth auth)
        cookie   (cb-set-cookie cookie)
        version  (cb-set-version version)
        timeout  (cb-set-timeout timeout)
        redirect (cb-set-redirect redirect))
      cb-build))

;;; request

(defn- rb-add-headers [^HttpRequest$Builder builder headers]
  (reduce
   (fn [^HttpRequest$Builder builder [k v]]
     (.header builder (if-not (keyword? k) k (name k)) v))
   builder headers))

(defn- rb-set-version [^HttpRequest$Builder builder version]
  (.version builder (http-version version)))

(defn- rb-set-timeout [^HttpRequest$Builder builder timeout]
  (.timeout builder (s/duration-time timeout)))

(defn- rb-set-method [^HttpRequest$Builder builder method body]
  (.method builder (http-method method) (http-body body)))

(defn- rb-set-uri [^HttpRequest$Builder builder uri]
  (.uri builder (if (instance? URI uri) uri (URI. uri))))

(defn- rb-build [^HttpRequest$Builder builder]
  (.build builder))

(defn ^HttpRequest make-request
  [uri {:keys [method body headers version timeout] :or {method :get}}]
  (-> (cond-> (HttpRequest/newBuilder)
        headers (rb-add-headers headers)
        version (rb-set-version version)
        timeout (rb-set-timeout timeout))
      (rb-set-method method body)
      (rb-set-uri uri)
      rb-build))

;;; fetch

(defmulti fetch (fn [rtype cli req] rtype))

(defmethod fetch :discard [rtype ^HttpClient cli req]
  (.sendAsync cli req (HttpResponse$BodyHandlers/discarding)))

(defmethod fetch :text [rtype ^HttpClient cli req]
  (.sendAsync cli req (HttpResponse$BodyHandlers/ofString)))

(defmethod fetch :bin [rtype ^HttpClient cli req]
  (.sendAsync cli req (HttpResponse$BodyHandlers/ofByteArray)))

(defmethod fetch :stream [rtype ^HttpClient cli req]
  (.sendAsync cli req (HttpResponse$BodyHandlers/ofInputStream)))

(defn- http-headers->map [^HttpHeaders headers]
  (->> (.map headers)
       (map
        (fn [[k v]]
          [(keyword (str/lower-case k)) (str/join \, v)]))
       (into {})))

;;; wrapper

(def ^:dynamic *connect-timeout* (Duration/ofSeconds 5))
(def ^:dynamic *request-timeout* (Duration/ofSeconds 5))
(def ^:dynamic *executor* pe/default-vthread-executor)
(def ^:dynamic *proxy* (proxy-selector))
(def ^:dynamic *cookie* (cookie-manager))

(defn client [& {:as opts}]
  (let [defaults {:executor *executor*
                  :proxy *proxy*
                  :cookie *cookie*
                  :timeout *connect-timeout*
                  :redirect :default}]
    (make-client (merge defaults opts))))

(extend-type HttpClient
  pt/IHttpClient
  (pt/do-http-fetch
    [this url {:keys [method headers body body-type accept accept-type accept-keywordize version timeout]
               :or {accept :text accept-keywordize false timeout *request-timeout*}}]
    (let [[headers body] (u/process-request-data headers body body-type)
          ^HttpRequest req (->> (cond-> {}
                                  method  (assoc :method method)
                                  headers (assoc :headers headers)
                                  body    (assoc :body body)
                                  version (assoc :version version)
                                  timeout (assoc :timeout timeout))
                                (make-request url))]
      (p/let [^HttpResponse resp (fetch accept this req)]
        (let [status (u/check-status (.statusCode resp))
              headers (http-headers->map (.headers resp))
              body (case accept
                     :discard nil
                     :stream (s/input-stream->read-stream (.body resp))
                     :bin (.body resp)
                     :text (.body resp))]
          {:status status
           :headers headers
           :body (u/process-response-data headers body accept accept-type accept-keywordize)})))))
