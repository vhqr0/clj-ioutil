(ns clj-ioutil.http.client
  (:refer-clojure :exclude [send])
  (:require [promesa.core :as p]
            [clj-ioutil.http.client.protocol :as pt]
            [clj-ioutil.http.client.impl :as impl]))

(def make-client
  "[& opts]
  make a platform native http client from opts, atleast support:
  - default: default values of request opts.
  - headers: map from str to printable (eg. str, number).
  "
  impl/make-client)

(defn send [client url & {:as opts}]
  (pt/-send client url opts))

(defn make-read-stream [client url & {:as opts}]
  (p/let [response (pt/-send client url (assoc opts :accept :stream))]
    (:body response)))

(comment
  (do
    (def cli (make-client))
    (def req ["https://jsonplaceholder.typicode.com/todos/1" :accept-type :json])
    (def res (atom nil))
    (-> (p/let [resp (apply send cli req)]
          (reset! res resp))
        (p/catch #(reset! res %)))))
