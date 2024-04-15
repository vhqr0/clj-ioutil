(ns proto.http
  (:require [clojure.string :as str]
            [ioutil.bytes :as bytes]
            [ioutil.struct :as struct]
            [ioutil.structs :as structs]
            [proto.codec :as codec]))

;; RFC2616: HTTP

(defrecord http-status [code reason])

(defn status-error
  ([code reason] (status-error (->http-status code reason)))
  ([status] (ex-info (str "http status error: " status)
                     {:type ::status-error ::status status})))

(defn status-error? [e]
  (= (:type (ex-data e)) ::status-error))

(struct/defstruct http-headers
  [:while
   :test (str/blank? it)
   :spec structs/crlf-line
   :to (->> (butlast it)
            (map #(if-let [[k v] (str/split % #":" 2)]
                    [(str/trim k) (str/trim v)]
                    (throw (struct/validation-error))))
            (into {}))
   :from (concat (map #(let [[k v] %] (str k \: \space v)) it) [""])])

(struct/defstruct http-request
  [[[method path] [:line
                   :end "\r\n"
                   :to (if-let [[_ method path] (re-matches #"(\w+)\s+([^\s]+)\s+HTTP/1.1" it)]
                         [method path]
                         (throw (struct/validation-error)))
                   :from (let [[method path] it]
                           (str method \space path \space "HTTP/1.1"))]]
   [headers http-headers]])

(struct/defstruct http-response
  [[status [:line
            :end "\r\n"
            :to (if-let [[_ code reason] (re-matches #"HTTP/1.1\s+(\d+)\s+(.+)" it)]
                  (->http-status (bytes/str->int code) reason)
                  (throw (struct/validation-error)))
            :from (let [{:keys [code reason]} it]
                    (str "HTTP/1.1" \space code \space reason))]]
   [headers http-headers]])
