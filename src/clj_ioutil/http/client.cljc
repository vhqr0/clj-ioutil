(ns clj-ioutil.http.client
  (:refer-clojure :exclude [send])
  (:require [clj-ioutil.http.client.protocol :as pt]
            [clj-ioutil.http.client.impl :as impl]))

(def make-client
  "[& opts]
  make a platform native http client from opts, atleast support:
  - default: default values of request opts.
  - headers: map from str to printable (eg. str, number).
  - redirect: bool, should follow the redirect, default to true.
  "
  impl/make-client)

(defn send [client url & {:as opts}]
  (pt/-send client url opts))

(comment
  (do
    (def cli (make-client {:redirect true}))
    (def req ["https://jsonplaceholder.typicode.com/todos/1" :accept-type :json])
    (def res (atom nil))
    (-> (promesa.core/let [resp (apply send cli req)]
          (reset! res resp))
        (promesa.core/catch #(reset! res %)))))
