(ns clj-ioutil.http.websocket.impl
  (:require [clj-ioutil.cljs.streams :as s]))

(defn make-stream [url & {:keys [subprotocol]}]
  (->> (cond-> []
         subprotocol (conj :subprotocol subprotocol))
       (apply s/make-websocket-stream url)))
