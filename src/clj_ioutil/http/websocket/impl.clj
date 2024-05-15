(ns clj-ioutil.http.websocket.impl
  (:require [clj-ioutil.clj.streams :as s]))

(def ^:dynamic *default-http-client* (delay (s/make-http-client)))

(defn make-stream
  [url & {:keys [subprotocol client timeout] :or {client *default-http-client*}}]
  (->> (cond-> []
         subprotocol (conj :subprotocol subprotocol)
         timeout     (conj :timeout timeout))
       (apply s/make-websocket-stream @client url)))
