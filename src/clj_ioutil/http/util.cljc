(ns clj-ioutil.http.util
  (:require [clojure.string :as str]
            [clj-ioutil.http.status :as sta]
            [clj-ioutil.http.data :as d]))

(defn check-status [status]
  (when-not (sta/success? status)
    (throw (ex-info "http status error" {:type :http :reason :status :status status})))
  status)

(defn process-request-data [headers body body-type]
  (if-not body-type
    [headers body]
    [(assoc headers :content-type (d/content-type body-type))
     (d/clj->data body body-type)]))

(defn process-response-data [headers body accept accept-type accept-keywordize]
  (if-not (and (= accept :text) accept-type)
    body
    (let [content-type (str/replace (:content-type headers) #";.*" "")]
      (if-not (= content-type (d/content-type accept-type))
        (throw (ex-info "http content type error" {:type :http :reason :content-type :content-type content-type}))
        (d/data->clj body accept-type :keywordize accept-keywordize)))))
