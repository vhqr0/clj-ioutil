(ns clj-ioutil.http.client
  (:refer-clojure :exclude [send])
  (:require [promesa.core :as p]
            [clj-ioutil.http.protocol :as pt]
            [clj-ioutil.http.client.impl :as impl]))

(def client
  "[& opts]
  make a platform native http client."
  impl/client)

(defn fetch [cli url & {:as opts}]
  (pt/do-http-fetch cli url opts))

(defn read-stream [cli url & {:as opts}]
  (p/let [resp (pt/do-http-fetch cli url (assoc opts :accept :stream))]
    (:body resp)))

(def ^:dynamic *client* (delay (client)))

(comment
  (do
    (def cli (client))
    (def req ["https://jsonplaceholder.typicode.com/todos/1" :accept-type :json])
    (def res (atom nil))
    (-> (p/let [resp (apply fetch cli req)]
          (reset! res resp))
        (p/catch #(reset! res %)))))
