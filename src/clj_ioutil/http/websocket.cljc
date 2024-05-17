(ns clj-ioutil.http.websocket
  (:refer-clojure :exclude [send])
  (:require [promesa.core :as p]
            [promesa.exec.csp :as csp]
            [clj-ioutil.bytes :as b]
            [clj-ioutil.http.protocol :as pt]
            [clj-ioutil.http.websocket.impl :as impl]))

(def websocket
  "[url listener & {:keys [protocol]}]
  make a p/let-able platform native websocket connection to url.
  - listener: {:keys [on-data on-close on-error]}"
  impl/websocket)

(def ws-send pt/ws-send)
(def ws-abort pt/ws-abort)

(def ^:dynamic *chan-size* 1024)

(defn make-chan-listener
  ([cch]
   (make-chan-listener cch (csp/chan :buf *chan-size*)))
  ([cch ch]
   [ch {:on-data (fn [ws data] (assert (csp/offer! ch data)))
        :on-close (fn [ws] (csp/close! ch))
        :on-error (fn [ws err] (csp/close! cch err))}]))

(defn websocket->chan
  ([ws cch]
   (websocket->chan ws cch (csp/chan)))
  ([ws cch ch]
   (p/vthread
     (-> (p/loop [data (csp/take ch)]
           (if-not data
             (pt/ws-send ws)
             (do
               (pt/ws-send ws data)
               (p/recur (csp/take ch)))))
         (p/catch #(csp/close! cch %))))
   ch))

(defn websocket-stream [url & opts]
  (let [cch (csp/chan)
        [ich listener] (make-chan-listener cch)]
    (p/let [ws (apply websocket url listener opts)]
      (let [och (websocket->chan cch ws)]
        (b/create-stream cch ich och #(pt/ws-abort ws))))))

(defn send
  ([s]
   (csp/close! (:out-chan s)))
  ([s data]
   (p/let [ok (csp/put (:out-chan s) data)]
     (assert ok))))

(defn recv [s]
  (csp/take (:in-chan s)))

(comment
  (do
    (def u "wss://echo.websocket.org")
    (def res (atom nil))
    (-> (p/let [s (websocket-stream u)
                it (recv s)]
          (reset! res it))
        (p/catch #(reset! res %)))))
