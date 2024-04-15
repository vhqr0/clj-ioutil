(ns proto.url
  (:require [clojure.string :as str]
            [ioutil.bytes :as bytes]
            [ioutil.struct :as struct]
            [proto.codec :as codec]))

;; RFC3986: URI

(defrecord url [scheme userinfo host port path query fragment])

;;; utils

(defn- range->char-set [from to]
  (->> (range (int from) (inc (int to))) (map char) set))

(def url-alpha? (set (concat (range->char-set \a \z) (range->char-set \A \Z))))
(def url-dight? (range->char-set \0 \9))
(def url-hexdig? (set (concat url-dight? (range->char-set \a \f) (range->char-set \A \F))))
(def url-gen-delims? #{\: \/ \? \# \[ \] \@})
(def url-sub-delims? #{\! \$ \& \' \( \) \* \, \; \=})
(def url-reserved? (set (concat url-gen-delims? url-sub-delims?)))
(def url-unreserved? (set (concat url-alpha? url-dight? [\- \. \_ \~])))
(def url-pchar? (set (concat url-unreserved? url-sub-delims? #{\: \@ \%})))

(defn- url-take-char [s]
  (cond (= (first s) \%) (let [[[p l r] s] (split-at 3 s)]
                           (if (and (= p \%) (url-hexdig? l) (url-hexdig? r))
                             [(str p l r) s]
                             (throw (struct/validation-error))))
        (seq s) [(first s) (rest s)]
        :else (throw (struct/validation-error))))

(defn- url-take-chars
  ([s pred & preds] (url-take-chars s (apply some-fn pred preds)))
  ([s pred] (loop [s s cs []]
              (if (or (empty? s) (not (pred (first s))))
                [(apply str cs) s]
                (let [[c ns] (url-take-char s)]
                  (recur ns (conj cs c)))))))

;;; take url

(defn url-take-scheme [s]
  (if (url-alpha? (first s))
    (let [[scheme s] (url-take-chars s url-alpha? url-dight? #{\+ \- \.})]
      (if (= (take 3 s) [\: \/ \/])
        [scheme (drop 3 s)]
        (throw (struct/validation-error))))
    (throw (struct/validation-error))))

(defn url-take-userinfo [s]
  (let [[userinfo ns] (url-take-chars s (disj url-pchar? \@))]
    (if (= (first ns) \@)
      [userinfo (rest ns)]
      [nil s])))

(defn url-take-host [s]
  (let [[l r] [\[ \]]]
    (if (= (first s) l)
      (let [[host s] (url-take-chars (rest s) url-hexdig? #{\:})]
        (if (= (first s) r)
          [(apply str host) (rest s)]
          (throw (struct/validation-error))))
      ;; NOTICE: URL host should not contain pct-encoded chars.
      (url-take-chars s (disj url-pchar? \@ \: \%)))))

(defn url-take-port [s]
  (if (= (first s) \:)
    (url-take-chars (rest s) url-dight?)
    [nil s]))

(defn url-take-path [s]
  (if (= (first s) \/)
    (loop [s s acc []]
      (if (= (first s) \/)
        (let [[it ns] (url-take-chars (rest s) url-pchar?)]
          (recur ns (conj acc (str \/ it))))
        [(apply str acc) s]))
    [nil s]))

(defn url-take-query [s]
  (if (= (first s) \?)
    (url-take-chars (rest s) url-pchar? #{\/ \?})
    [nil s]))

(defn url-take-fragment [s]
  (if (= (first s) \#)
    (url-take-chars (rest s) url-pchar? #{\/ \?})
    [nil s]))

;;; parse url

(defn ->url-str [s]
  (when s
    (if (string? s)
      (codec/url->str s)
      (throw (struct/validation-error)))))

(defn ->url-host [host]
  (when host
    (if (string? host)
      (codec/idn->str host)
      (throw (struct/validation-error)))))

(defn ->url-port [port]
  (when port
    (cond (int? port) port
          (string? port) (bytes/str->int port)
          :else (throw (struct/validation-error)))))

(defn ->url-query [query]
  (when query
    (cond (map? query) query
          (string? query) (->> (str/split query #"&")
                               (map #(if-let [[_ k v] (re-matches #"(.*)=(.*)" %)]
                                       [(codec/url->str k) (codec/url->str v)]
                                       (throw (struct/validation-error))))
                               (into {}))
          :else (throw (struct/validation-error)))))

(defn make-url [m]
  (let [{:keys [scheme userinfo host port path query fragment]} m
        userinfo (->url-str userinfo)
        host (->url-host host)
        port (->url-port port)
        path (->url-str path)
        query (->url-query query)
        fragment (->url-str fragment)]
    (->url scheme userinfo host port path query fragment)))

(defn str->url [s]
  (let [[scheme s] (url-take-scheme s)
        [userinfo s] (url-take-userinfo s)
        [host s] (url-take-host s)
        [port s] (url-take-port s)
        [path s] (url-take-path s)
        [query s] (url-take-query s)
        [fragment s] (url-take-fragment s)]
    (if (empty? s)
      (make-url {:scheme scheme
                 :userinfo userinfo
                 :host host
                 :port port
                 :path path
                 :query query
                 :fragment fragment})
      (throw (struct/validation-error)))))

;;; build url

(defn url-host->str [host]
  (when host
    (let [host (if (str/index-of host \:) (str \[ host \]) host)]
      (codec/str->idn host))))

(defn url-query->str [query]
  (when query
    (->> query
         (map #(let [[k v] %] (str (codec/str->url k) \= (codec/str->url v))))
         (interpose \&)
         (apply str \?))))

(defn url->str [u]
  (let [{:keys [scheme userinfo host port path query fragment]} u]
    (str scheme \: \/ \/
         (when userinfo (str (codec/str->url userinfo) \@))
         (when host (url-host->str host))
         (when port (str \: port))
         (when path (codec/str->url path))
         (when query (str \? (url-query->str query)))
         (when fragment (str \# (codec/str->url fragment))))))
