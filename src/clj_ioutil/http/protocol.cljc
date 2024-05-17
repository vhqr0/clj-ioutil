(ns clj-ioutil.http.protocol)

(defprotocol IHttpClient
  (do-http-fetch [this url opts]
    "Send request to url and return promise of {:keys [status headers body]}. Opts atleast support:
    - method: keyword, at least support :get and :post, default to :get.
    - headers: map from keyword to printable (eg. str, number).
    - body: nil, str, bytes, or clj data to encode by body type.
    - body-type: keyword, atleast support :json, :form and :edn, update headers automatically.
    - accept: keyword, response body type, atleast support :discord, :stream, :bin and :text, default to :text.
    - accept-type: keyword, atleast support :json, :form and :edn.
    - accept-keywordize: bool, should keywordize keys of :form, :json, etc, default to true.
    "))

(defprotocol IWebSocket
  (ws-send [this] [this data])
  (ws-abort [this]))
