(ns clj-ioutil.http.websocket.impl
  (:require [clj-ioutil.http.protocol :as pt]))

(defn websocket [url listener & {:keys [protocol]}]
  (let [{:keys [on-data on-close on-error]} listener
        ws (if-not protocol
             (js/WebSocket. url)
             (js/WebSocket. url (if (string? protocol) protocol (to-array protocol))))]
    (when on-data
      (.addEventListener
       ws "message" (fn [ev] (on-data ws (.-data ev))))
      (.addEventListener
       ws "close" (fn [ev] (on-close ws)))
      (.addEventListener
       ws "error" (fn [ev] (on-error ws (.-error ev)))))))

(extend-type js/WebSocket
  pt/IWebSocket
  (pt/ws-send
    ([this] (.close this))
    ([this data] (.send this data)))
  (pt/ws-abort
    ([this] (.close this))))
