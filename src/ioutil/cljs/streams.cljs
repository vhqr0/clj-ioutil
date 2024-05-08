(ns ioutil.cljs.streams
  (:require [promesa.core :as p]
            [promesa.exec.csp :as csp]
            [ioutil.bytes :as b]))

(defn go-close [close-chan chans callback]
  (p/vthread
   (-> (csp/take close-chan)
       (p/finally
         (fn [_ _]
           (csp/close! close-chan)
           (doseq [chan chans]
             (csp/close! chan))
           (callback))))))

(defn readable-stream->chan
  ([readable-stream close-chan]
   (readable-stream->chan readable-stream close-chan (csp/chan)))
  ([readable-stream close-chan chan]
   (p/vthread
    (let [reader (.getReader readable-stream)]
      (-> (p/loop [r (.read reader)]
            (if (.-done r)
              (csp/close! chan)
              (p/let [ok (csp/put chan (.-buffer (.-value r)))]
                (assert ok)
                (p/recur (.read reader)))))
          (p/catch #(csp/close! close-chan %)))))
   chan))

;;; url

(defn make-url-search [search]
  (if (map? search)
    (js/URLSearchParams. (clj->js search))
    (js/URLSearchParams. search)))

(defn make-url
  ([url]
   (js/URL. url))
  ([protocol hostname
    & {:keys [username password port pathname search hash]}]
   (let [url (js/URL. (str protocol "://" hostname))]
     (when username
       (set! (.-username url) username))
     (when password
       (set! (.-password url) password))
     (when port
       (set! (.-port url) port))
     (when pathname
       (set! (.-pathname url) pathname))
     (when search
       (set! (.-search url) (make-url-search search)))
     (when hash
       (set! (.-hash url) hash))
     url)))

;;; http

(def http-method
  {:get     "GET"
   :head    "HEAD"
   :post    "POST"
   :put     "PUT"
   :delete  "DELETE"
   :options "OPTIONS"
   :trace   "TRACE"
   :patch   "PATCH"})

(def http-mode
  {:cors "cors"
   :no-cors "no-cors"
   :same-oirgin "same-origin"})

(def http-credentials
  {:omit "omit"
   :same-origin "same-origin"
   :include "include"})

(def http-cache
  {:default "default"
   :no-store "no-store"
   :reload "reload"
   :no-cache "no-cache"
   :force-cache "force-cache"
   :only-if-cached "only-if-cached"})

(def http-redirect
  {:follow "follow"
   :error "error"
   :manual "manual"})

(defn make-http-headers
  ([]
   (js/Headers.))
  ([headers]
   (if (map? headers)
     (js/Headers. (clj->js headers))
     (js/Headers. headers))))

(defn make-http-request
  ([url]
   (js/Request. (make-url url)))
  ([url & {:keys [method headers body mode credentials
                  cache redirect referrer integrity]}]
   (let [opts (cond-> {}
                method (assoc "method" (http-method method))
                headers (assoc "headers" (make-http-headers headers))
                body (assoc "body" body)
                mode (assoc "mode" (http-mode mode))
                credentials (assoc "credentials" (http-credentials credentials))
                cache (assoc "cache" (http-cache cache))
                redirect (assoc "redirect" (http-redirect redirect))
                referrer (assoc "referrer" referrer)
                integrity (assoc "integrity" integrity))]
     (js/Request. (make-url url) (clj->js opts)))))

(def http-referrer-policy
  {:no-referrer "no-referrer"
   :no-referrer-when-downgrade "no-referrer-when-downgrade"
   :same-origin "same-origin"
   :strict-origin "strict-origin"
   :origin-when-cross-origin "origin-when-cross-origin"
   :strict-origin-when-corss-origin "strict-origin-when-cross-origin"
   :unsafe-url "unsafe-url"})

(def http-prioirty
  {:high "high"
   :low "low"
   :auto "auto"})

(defn http-fetch
  ([request]
   (js/fetch request))
  ([request &
    {:keys [referrer-policy priority keepalive signal]}]
   (let [opts (cond-> {}
                referrer-policy (assoc "referrerPolicy" (http-referrer-policy referrer-policy))
                priority (assoc "priority" (http-prioirty priority))
                keepalive (assoc "keepalive" keepalive)
                signal (assoc "signal" signal))]
     (js/fetch request (clj->js opts)))))

(defn make-http-reader-stream [request & opts]
  (p/let [response (apply http-fetch request opts)
          body (.-body response)
          close-chan (csp/chan)
          in-chan (readable-stream->chan body close-chan)]
    (go-close close-chan [in-chan] #(.cancel body))
    (b/->stream response close-chan in-chan nil)))

(comment
  (do
    (def u "http://www.bing.com")
    (def res (atom nil))
    (-> (p/let [s (make-http-reader-stream u)
                r (b/make-stream-reader s)
                [r it] (b/read-all r)]
          (reset! res (b/bytes->str it)))
        (p/catch #(reset! res %)))))

;;; websocket

(defn make-websocket
  ([url]
   (js/WebSocket. (make-url url)))
  ([url protocols]
   (js/WebSocket. (make-url url) (to-array protocols))))

(def ^:dynamic *websocket-chan-size* 1024)

(defn make-websocket-in-chan
  ([websocket close-chan]
   (make-websocket-in-chan websocket close-chan (csp/chan *websocket-chan-size*)))
  ([websocket close-chan chan]
   (.addEventListener websocket "message"
                      (fn [event]
                        (-> (p/let [ok (csp/put chan (.-data event))]
                              (assert ok))
                            (p/catch #(csp/close! close-chan %)))))
   (.addEventListener websocket "close"
                      (fn [_]
                        (csp/close! chan)))
   (.addEventListener websocket "error"
                      #(csp/close! close-chan (.-error %)))
   chan))

(defn make-websocket-out-chan
  ([websocket close-chan]
   (make-websocket-out-chan websocket close-chan (csp/chan)))
  ([websocket close-chan chan]
   (-> (p/vthread
        (p/loop [data (csp/take chan)]
          (if-not data
            (.close websocket)
            (.send websocket data))))
       (p/catch #(csp/close! close-chan %)))
   chan))

(defn make-websocket-stream [& args]
  (p/vthread
   (let [websocket (apply make-websocket args)
         close-chan (csp/chan)
         in-chan (make-websocket-in-chan websocket close-chan)
         out-chan (make-websocket-out-chan websocket close-chan)]
     (b/->stream websocket close-chan in-chan out-chan))))

(defn websocket-send
  ([websocket]
   (csp/close! (:out-chan websocket)))
  ([websocket data]
   (p/let [ok (csp/put (:out-chan websocket) data)]
     (assert ok))))

(defn websocket-recv [websocket]
  (csp/take (:in-chan websocket)))

(comment
  (do
    (def u "wss://echo.websocket.org")
    (def res (atom nil))
    (-> (p/let [s (make-websocket-stream u)
                it (websocket-recv s)]
          (reset! res it))
        (p/catch #(reset! res %)))))
