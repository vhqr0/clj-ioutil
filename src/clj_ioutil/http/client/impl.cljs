(ns clj-ioutil.http.client.impl
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [clj-ioutil.cljs.streams :as s]
            [clj-ioutil.http.status :as sta]
            [clj-ioutil.http.data :as d]
            [clj-ioutil.http.client.protocol :as pt]))

(defn http-headers->map [headers]
  (->> (.entries headers)
       (map
        (fn [[k v]]
          [(keyword (str/lower-case k)) v]))
       (into {})))

(def ^:dynamic *request-timeout* 5000)

(defrecord client [defaults headers]
  pt/IHTTPClient
  (pt/-send [this url opts]
    (let [{:keys [method headers body accept accept-type accept-keywordize
                  mode credentials cache redirect referrer referrer-policy integrity priority keepalive]
           :or {accept :text accept-keywordize false}}
          (merge (:defaults this) opts)
          headers (merge (:headers this) headers)
          [headers body] (if-not (vector? body)
                           [headers body]
                           (let [[type body] body]
                             [(assoc headers :content-type (d/content-type type))
                              (d/clj->data body type)]))
          controller (js/AbortController.)
          request (->> (cond-> []
                         method            (conj :method method)
                         headers           (conj :headers headers)
                         body              (conj :body body)
                         mode              (conj :mode mode)
                         credentials       (conj :credentials credentials)
                         cache             (conj :cache cache)
                         redirect          (conj :redirect redirect)
                         referrer          (conj :referrer referrer)
                         referrer-policy   (conj :referrer-policy referrer-policy)
                         integrity         (conj :integrity integrity)
                         priority          (conj :priority priority)
                         (some? keepalive) (conj :keepalive keepalive))
                       (apply s/make-http-request url :signal (.-signal controller)))]
      (js/setTimeout #(.abort controller) 5)
      (p/let [response (apply s/http-fetch url (apply concat opts))]
        (let [status (.-status response)]
          (if-not (sta/success? status)
            (p/rejected (ex-info "http status error" {:type :http-status :status status}))
            (p/let [headers (http-headers->map (.-headers response))
                    body (case accept
                           :discard nil
                           :stream (s/readable-stream->read-stream (.-body response))
                           :bin (.arrayBuffer response)
                           :text (.text response))]
              (if-not (and (= accept :text) accept-type)
                {:status status :headers headers :body body}
                (let [content-type (str/replace (:content-type headers) #";.*" "")]
                  (if-not (= content-type (d/content-type accept-type))
                    (p/rejected (ex-info "http content type error" {:type :http-content-type :content-type content-type}))
                    {:status status :headers headers :body (d/data->clj body accept-type :keywordize accept-keywordize)}))))))))))

(defn make-client [& {:keys [defaults headers redirect]}]
  (let [defaults (if (nil? redirect)
                   defaults
                   (assoc defaults :redirect (if-not redirect :error :follow)))]
    (->client defaults headers)))
