(ns proto.codec
  (:require [ioutil.bytes :as bytes])
  (:import java.util.UUID
           java.util.Base64
           java.net.URLEncoder
           java.net.URLDecoder
           java.net.IDN
           java.security.MessageDigest))

(defn rand-uuid []
  (UUID/randomUUID))

(defn str->uuid [s]
  (UUID/fromString s))

(defn base64->bytes
  ([s] (base64->bytes s (Base64/getDecoder)))
  ([s decoder] (.decode decoder s)))

(defn bytes->base64
  ([b] (bytes->base64 b (Base64/getEncoder)))
  ([b encoder] (bytes/bytes->str (.encode encoder (if (string? b) (bytes/str->bytes b) b)))))

(defn url-base64->bytes [s]
  (base64->bytes s (Base64/getUrlDecoder)))

(defn bytes->url-base64 [b]
  (bytes->base64 b (Base64/getUrlEncoder)))

(defn mime-base64->bytes [s]
  (base64->bytes s (Base64/getMimeDecoder)))

(defn bytes->mime-base64 [b]
  (bytes->base64 b (Base64/getMimeEncoder)))

(defn str->url [s]
  (URLEncoder/encode s))

(defn url->str [u]
  (URLDecoder/decode u))

(defn str->idn [s]
  (IDN/toASCII s))

(defn idn->str [i]
  (IDN/toUnicode i))

(defn bytes->digest [b algo]
  (let [b (if (string? b) (bytes/str->bytes b) b)
        m (MessageDigest/getInstance algo)]
    (.update m b)
    (.digest m)))

(defn bytes->md5 [b] (bytes->digest b "MD5"))
(defn bytes->sha1 [b] (bytes->digest b "SHA-1"))
(defn bytes->sha224 [b] (bytes->digest b "SHA-224"))
(defn bytes->sha256 [b] (bytes->digest b "SHA-256"))
(defn bytes->sha384 [b] (bytes->digest b "SHA-384"))
(defn bytes->sha512 [b] (bytes->digest b "SHA-512"))
