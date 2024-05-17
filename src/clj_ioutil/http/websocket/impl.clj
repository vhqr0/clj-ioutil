(ns clj-ioutil.http.websocket.impl
  (:require [clj-ioutil.bytes :as b]
            [clj-ioutil.clj.streams :as s]
            [clj-ioutil.http.protocol :as pt]
            [clj-ioutil.http.client :as cli]
            [clj-ioutil.http.client.impl :as cli-impl])
  (:import java.nio.ByteBuffer
           java.net.URI
           [java.net.http HttpClient WebSocket WebSocket$Builder WebSocket$Listener]))

(defn- wsb-add-headers [^WebSocket$Builder builder headers]
  (reduce
   (fn [^WebSocket$Builder builder [k v]]
     (.header builder (if-not (keyword? k) k (name k)) v))
   builder headers))

(defn- wsb-set-protocol [^WebSocket$Builder builder protocol]
  (if (string? protocol)
    (.subprotocols builder protocol (make-array String 0))
    (.subprotocols builder (first protocol) (into-array String (rest protocol)))))

(defn- wsb-set-timeout [^WebSocket$Builder builder timeout]
  (.connectTimeout builder (s/duration-time timeout)))

(defn- wsb-build [^WebSocket$Builder builder uri listener]
  (.buildAsync builder (if (instance? URI uri) uri (URI. uri)) listener))

(defn map->listener [listener]
  (let [{:keys [on-data on-close on-error]} listener
        texts (volatile! [])
        bins (volatile! [])]
    (reify WebSocket$Listener
      (onText [this ws text last]
        (when on-data
          (vswap! texts conj text)
          (when last
            (let [text (apply str @texts)]
              (vreset! texts [])
              (on-data ws text))))
        nil)
      (onBinary [this ws bin last]
        (when on-data
          (vswap! bins conj bin)
          (when last
            (let [bin (apply b/concat @bins)]
              (vreset! bins [])
              (on-data ws bin))))
        nil)
      (onClose [this ws code reason]
        (when on-close
          (on-close ws))
        nil)
      (onError [this ws err]
        (when on-error
          (on-error ws err))
        nil))))

(defn ^WebSocket websocket
  [uri listener
   & {:keys [client headers protocol timeout]
      :or {client cli/*client* timeout cli-impl/*request-timeout*}}]
  (-> (cond-> (.newWebSocketBuilder ^HttpClient @client)
        headers  (wsb-add-headers headers)
        protocol (wsb-set-protocol protocol)
        timeout  (wsb-set-timeout timeout))
      (wsb-build uri (map->listener listener))))

(extend-type WebSocket
  pt/IWebSocket
  (pt/ws-send
    ([^WebSocket this]
     (.sendClose this WebSocket/NORMAL_CLOSURE ""))
    ([^WebSocket this data]
     (if (bytes? data)
       (.sendBinary this (ByteBuffer/wrap data) true)
       (.sendText this ^String data true))))
  (pt/ws-abort [^WebSocket this]
    (.abort this)))
