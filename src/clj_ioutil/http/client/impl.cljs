(ns clj-ioutil.http.client.impl
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [clj-ioutil.bytes.cljs.streams :as s]
            [clj-ioutil.http.util :as u]
            [clj-ioutil.http.protocol :as pt]))

(defn timeout-signal [timeout]
  (let [controller (js/AbortController.)]
    (js/setTimeout #(.abort controller) timeout)
    (.-signal controller)))

(defn map->http-headers [headers]
  (js/Headers. (clj->js headers)))

(defn http-headers->map [headers]
  (->> (.entries headers)
       (map
        (fn [[k v]]
          [(keyword (str/lower-case k)) v]))
       (into {})))

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
   :same-oirgin "same-origin"
   :navigate "navigate"})

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

(def ^:dynamic *timeout* 5000)

(defrecord HttpClient [opts]
  pt/IHttpClient
  (pt/do-http-fetch [this url opts]
    (let [{:keys [method headers body body-type accept accept-type accept-keywordize
                  mode credentials cache redirect referrer referrer-policy integrity priority keepalive signal]
           :or {accept :text accept-keywordize false redirect :follow signal *timeout*}}
          (merge (:opts this) opts)
          [headers body] (u/process-request-data headers body body-type)
          req (->> (cond-> {}
                     method (assoc "method" (http-method method))
                     headers (assoc "headers" (map->http-headers headers))
                     body (assoc "body" body)
                     mode (assoc "mode" (http-mode mode))
                     credentials (assoc "credentials" (http-credentials credentials))
                     cache (assoc "cache" (http-cache cache))
                     redirect (assoc "redirect" (http-redirect redirect))
                     referrer (assoc "referrer" referrer)
                     referrer-policy (assoc "referrerPolicy" (http-referrer-policy referrer-policy))
                     integrity (assoc "integrity" integrity)
                     priority (assoc "priority" (http-prioirty priority))
                     keepalive (assoc "keepalive" keepalive)
                     signal (assoc "signal" (if-not (number? signal) signal (timeout-signal signal))))
                   clj->js
                   (js/Request. url))]
      (p/let [resp (js/fetch req)]
        (let [status (u/check-status (.-status resp))
              headers (http-headers->map (.-headers resp))]
          (p/let [body (case accept
                         :discard nil
                         :stream (s/readable-stream->read-stream (.-body resp))
                         :bin (.arrayBuffer resp)
                         :text (.text resp))]
            {:status status
             :headers headers
             :body (u/process-response-data headers body accept accept-type accept-keywordize)}))))))

(defn client [& {:as opts}]
  (->HttpClient opts))
