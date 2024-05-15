(ns clj-ioutil.http.client.impl
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [clj-ioutil.clj.streams :as s]
            [clj-ioutil.http.status :as sta]
            [clj-ioutil.http.data :as d]
            [clj-ioutil.http.client.protocol :as pt])
  (:import [java.net.http HttpHeaders HttpResponse]))

(defn http-headers->map [^HttpHeaders headers]
  (->> (.map headers)
       (map
        (fn [[k v]]
          [(keyword (str/lower-case k)) (apply str (interpose \, v))]))
       (into {})))

(defrecord client [client defaults headers]
  pt/IHTTPClient
  (pt/-send [this url opts]
    (let [{:keys [method headers body accept accept-type accept-keywordize version timeout]
           :or {accept :text accept-keywordize false}}
          (merge (:defaults this) opts)
          headers (merge (:headers this) headers)
          [headers body] (if-not (vector? body)
                           [headers body]
                           (let [[type body] body]
                             [(assoc headers :content-type (d/content-type type))
                              (d/clj->data body type)]))
          request (->> (cond-> []
                         method  (conj :method method)
                         headers (conj :headers headers)
                         body    (conj :body body)
                         version (conj :version version)
                         timeout (conj :timeout timeout))
                       (apply s/make-http-request url))]
      (p/let [^HttpResponse response (s/http-send accept (:client this) request)]
        (let [status (.statusCode response)]
          (if-not (sta/success? status)
            (p/rejected (ex-info "http status error" {:type :http-status :status status}))
            (let [headers (http-headers->map (.headers response))
                  body (case accept
                         :discard nil
                         :stream (s/input-stream->read-stream (.body response))
                         :bin (.body response)
                         :text (.body response))]
              (if-not (and (= accept :text) accept-type)
                {:status status :headers headers :body body}
                (let [content-type (str/replace (:content-type headers) #";.*" "")]
                  (if-not (= content-type (d/content-type accept-type))
                    (p/rejected (ex-info "http content type error" {:type :http-content-type :content-type content-type}))
                    {:status status :headers headers :body (d/data->clj body accept-type :keywordize accept-keywordize)}))))))))))

(defn make-client
  [& {:keys [defaults headers redirect executor proxy auth cookie version timeout]}]
  (let [client (->> (cond-> []
                      (some? redirect) (conj :redirect (if redirect :default :never))
                      executor         (conj :executor executor)
                      proxy            (conj :proxy proxy)
                      auth             (conj :auth auth)
                      cookie           (conj :cookie cookie)
                      version          (conj :version version)
                      timeout          (conj :timeout timeout))
                    (apply s/make-http-client))]
    (->client client defaults headers)))
