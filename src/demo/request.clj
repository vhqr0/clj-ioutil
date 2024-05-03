(ns ioutil.demo.request
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [ioutil.bytes :as b]
            [ioutil.struct :as st]
            [ioutil.clj.streams :as streams]))

(def http-header
  [:assoc
   :fline   st/crlf-line
   :headers [:map [#(->> (butlast %)
                         (map (fn [it] (str/split it #"\s*:\s*" 2)))
                         (into {}))
                   #(-> (map (fn [[k v]] (str k \: \space v)) %)
                        (concat [""]))]
             [:take #(= (last %) "")
              st/crlf-line]]])

(defn request [host & {:keys [meth port path headers socket-opts]
                       :or {meth "GET" port 80 path "/"}}]
  (let [fline (str meth \space path \space "HTTP/1.1")
        headers (merge {"Host" host} headers)
        header {:fline fline :headers headers}]
    (p/let [socket (vec (concat [host port] socket-opts))
            stream (streams/make-socket-stream socket)]
      (-> (p/let [reader (b/make-stream-reader stream)
                  writer (b/make-stream-writer stream)
                  writer (st/write-struct writer http-header header)
                  writer (b/write writer)
                  [reader header] (st/read-struct reader http-header)]
            (if-let [length (get-in header [:headers "Content-Length"])]
              (p/let [length (b/str->int length :unsigned true)
                      [reader content] (b/read reader length)]
                [header (b/bytes->str content)])
              [header nil]))
          (p/finally (fn [_ _] (b/close stream)))))))

(defn request2 [uri & {:keys [client-opts request-opts]}]
  (let [client (apply streams/make-http-client client-opts)
        request (apply streams/make-http-request uri request-opts)]
    (p/let [resp (streams/http-send :str client request)]
      (println (.headers resp))
      (println (.body resp)))))

;; wss://echo.websocket.org
(defn ws-echo [uri & {:keys [message] :or {message "hello"}}]
  (let [client (streams/make-http-client)]
    (p/let [stream (streams/make-websocket-stream client uri)]
      (-> (p/do
            (streams/websocket-send stream message)
            (println "sent" message)
            (p/let [message (streams/websocket-recv stream)]
              (println "recv" message))
            (streams/websocket-send stream))
          (p/finally (fn [_ _] (b/close stream)))))))
