(ns clj-ioutil.http.client
  (:refer-clojure :exclude [send])
  (:require [promesa.core :as p]
            [promesa.exec.csp :as csp]
            [clj-ioutil.http.websocket.impl :as impl]))

(def make-stream
  "[url & opts]
  make a platform native websocket stream to url from opts, atleast support:
  - subprotocol: string or vector of strings."
  impl/make-stream)

(defn send
  ([websocket]
   (csp/close! (:out-chan websocket)))
  ([websocket data]
   (p/let [ok (csp/put (:out-chan websocket) data)]
     (assert ok))))

(defn recv [websocket]
  (csp/take (:in-chan websocket)))

(comment
  (do
    (def u "wss://echo.websocket.org")
    (def res (atom nil))
    (-> (p/let [s (make-stream u)
                it (recv s)]
          (reset! res it))
        (p/catch #(reset! res %)))))
