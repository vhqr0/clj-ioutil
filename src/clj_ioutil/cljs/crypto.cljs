(ns clj-ioutil.cljs.crypto
  (:require [clj-ioutil.bytes :as b]))

;;; params

(defmulti make-crypto-params
  (fn [type opts] type))

(defn crypto-params [type & {:as opts}]
  (clj->js (make-crypto-params type opts)))

;;;; aes

;; iv: 16 bytes
(defmethod make-crypto-params :aes-cbc [type {:keys [iv]}]
  {"name" "AES-CBC" "iv" (or iv (b/rand-bytes 16))})

;; counter: 16 bytes
;; length: 0..128
(defmethod make-crypto-params :aes-ctr [type {:keys [counter length] :or {length 64}}]
  {"name" "AES-CTR"
   "counter" (or counter (b/rand-bytes 16))
   "length" length})

;; iv: 12 bytes
;; additional-data: bytes
;; tag-length: 32, 64, 96, ..., 128
(defmethod make-crypto-params :aes-gcm [type {:keys [iv additional-data tag-length]}]
  (cond-> {"name" "AES-GCM"
           "iv" (or iv (b/rand-bytes 12))}
    additional-data (assoc "additionalData" additional-data)
    tag-length (assoc "tagLength" tag-length)))

;; name: AES-CBC, AES-CTR, AES-GCM, AES-KW
;; length: 128, 192, 256
(defmethod make-crypto-params :aes-key-gen
  [type {:keys [name length] :or {length 128}}]
  {"name" name "length" length})

(defmethod make-crypto-params :aes-cbc-key-gen [type opts]
  (make-crypto-params :aes-key-gen (assoc opts :name "AES-CBC")))

(defmethod make-crypto-params :aes-ctr-key-gen [type opts]
  (make-crypto-params :aes-key-gen (assoc opts :name "AES-CTR")))

(defmethod make-crypto-params :aes-gcm-key-gen [type opts]
  (make-crypto-params :aes-key-gen (assoc opts :name "AES-GCM")))

(defmethod make-crypto-params :aes-kw-key-gen [type opts]
  (make-crypto-params :aes-key-gen (assoc opts :name "AES-KW")))

;;;; ec

;; hash: SHA-256, SHA-384, SHA-512
(defmethod make-crypto-params :ecdsa [type {:keys [hash] :or {hash "SHA-256"}}]
  {"name" "ECDSA" "hash" hash})

;; public: ec public key
(defmethod make-crypto-params :ecdh [type {:keys [public]}]
  {"name" "ECDH" "public" public})

;; name: ECDSA, ECDH
;; named-curve: P-256, P-384, P-521
(defmethod make-crypto-params :ec-key-gen
  [type {:keys [name named-curve] :or {named-curve "P-256"}}]
  {"name" name "namedCurve" named-curve})

(defmethod make-crypto-params :ecdsa-key-gen [type opts]
  (make-crypto-params :ec-key-gen (assoc opts :name "ECDSA")))

(defmethod make-crypto-params :ecdh-key-gen [type opts]
  (make-crypto-params :ec-key-gen (assoc opts :name "ECDH")))

;; same as :ec-key-gen
(defmethod make-crypto-params :ec-key-import [type opts]
  (make-crypto-params :ec-key-gen opts))

;;;; rsa

;; salt-length: Math.ceil((keySizeInBits - 1) / 8) - digestSizeInBytes - 2;
(defmethod make-crypto-params :rsa-pss [type {:keys [salt-length] :or {salt-length 32}}]
  {"name" "RSA-PSS" "saltLength" salt-length})

;; label: bytes
(defmethod make-crypto-params :rsa-oaep [type {:keys [label]}]
  (cond-> {"name" "RSA-OAEP"}
    label (assoc "label" label)))

(def bytes-65537 (b/bmake [1 0 1]))

;; name: RSASSA-PKCS1-v1_5, RSA-PSS, RSA-OAEP
;; modulus-length: >=2048
;; public-exponent: bytes, usually 65537(0x010001)
;; hash: SHA-256, SHA-384, SHA-512
(defmethod make-crypto-params :rsa-hashed-key-gen
  [type {:keys [name modulus-length public-exponent hash]
         :or {modulus-length 4096 public-exponent bytes-65537 hash "SHA-256"}}]
  {"name" name "modulusLength" modulus-length "publicExponent" public-exponent "hash" hash})

(defmethod make-crypto-params :rsa-ssa-hashed-key-gen [type opts]
  (make-crypto-params :rsa-hashed-key-gen (assoc opts :name "RSASSA-PKCS1-v1_5")))

(defmethod make-crypto-params :rsa-pss-hashed-key-gen [type opts]
  (make-crypto-params :rsa-hashed-key-gen (assoc opts :name "RSA-PSS")))

(defmethod make-crypto-params :rsa-oaep-hashed-key-gen [type opts]
  (make-crypto-params :rsa-hashed-key-gen (assoc opts :name "RSA-OAEP")))

;; name: RSASSA-PKCS1-v1_5, RSA-PSS, RSA-OAEP
;; hash: SHA-256, SHA-384, SHA-512
(defmethod make-crypto-params :rsa-hashed-import
  [type {:keys [name hash] :or {hash "SHA-256"}}]
  {"name" name "hash" hash})

(defmethod make-crypto-params :rsa-ssa-hashed-import [type opts]
  (make-crypto-params :rsa-hashed-import (assoc opts :name "RSASSA-PKCS1-v1_5")))

(defmethod make-crypto-params :rsa-pss-hashed-import [type opts]
  (make-crypto-params :rsa-hashed-import (assoc opts :name "RSA-PSS")))

(defmethod make-crypto-params :rsa-oaep-hashed-import [type opts]
  (make-crypto-params :rsa-hashed-import (assoc opts :name "RSA-OAEP")))

;;;; hmac

;; hash: SHA-256, SHA-384, SHA-512
;; length: length of bits
(defmethod make-crypto-params :hmac-key-gen
  [type {:keys [hash length] :or {hash "SHA-256"}}]
  (cond-> {"name" "HMAC" "hash" hash}
    length (assoc "length" length)))

;; same as hmac-key-gen
(defmethod make-crypto-params :hmac-import [type opts]
  (make-crypto-params :hmac-key-gen opts))

;;;; kdf

;; hash: SHA-256, SHA-384, SHA-512
;; salt: bytes
;; info: bytes
(defmethod make-crypto-params :hkdf
  [type {:keys [hash salt info] or {hash "SHA-256"}}]
  {"name" "HKDF" "hash" hash "salt" salt "info" info})

;; hash: SHA-256, SHA-384, SHA-512
;; salt: 16 bytes
;; iterations: int
(defmethod make-crypto-params :pbkdf2
  [type {:keys [hash salt iterations] :or {hash "SHA-256"}}]
  {"name" "PBKDF2" "hash" hash "salt" salt "iterations" iterations})

;;; key

(def key-usage
  {:encrypt "encrypt"
   :decrypt "decrypt"
   :sign "sign"
   :verify "verify"
   :derive-key "deriveKey"
   :derive-bits "deriveBits"
   :wrap-key "wrapKey"
   :unwrap-key "unwrapKey"})

(def key-format
  {:raw "raw"
   :pkcs8 "pkcs8"
   :spki "spki"
   :jwk "jwk"})

(defn create-key [params usage & {:keys [extractable from] :or {extractable false}}]
  (let [usage (if (keyword? usage)
                (array (key-usage usage))
                (to-array (map key-usage usage)))]
    (if-not from
      (js/crypto.subtle.generateKey
       params extractable usage)
      (case (first from)
        :derive (let [[from derive-params derive-key] from]
                  (js/crypto.subtle.deriveKey
                   derive-params derive-key
                   params extractable usage))
        :import (let [[from format data] from]
                  (js/crypto.subtle.importKey
                   (key-format format) data
                   params extractable usage))
        :unwrap (let [[from format data wrap-params wrap-key] from]
                  (js/crypto.subtle.unwrapKey
                   (key-format format) data
                   wrap-key wrap-params
                   params extractable usage))))))

(defn export-key [key & {:keys [format wrap] :or {format :raw}}]
  (if-not wrap
    (js/crypto.subtle.exportKey
     (key-format format) key)
    (let [[wrap-params wrap-key] wrap]
      (js/crypto.subtle.wrapKey
       (key-format format) key
       wrap-key wrap-params))))

(comment
  (do
    (def res (atom nil))
    (-> (promesa.core/let [key (create-key (crypto-params :aes-gcm-key-gen) :encrypt :extractable true)
                           ex (export-key :jwk key)]
          (reset! res ex))
        (promesa.core/catch #(reset! res %))))
  (do
    (-> (promesa.core/let [key (create-key (crypto-params :aes-gcm-key-gen) :encrypt :from [:import :jwk @res])]
          (reset! res key))
        (promesa.core/catch #(reset! res %)))))

;;; crypt

(defn sign [data params key]
  (js/crypto.subtle.sign params key data))

(defn verify [data params key signature]
  (js/crypto.subtle.verify params key signature data))

(defn encrypt [data params key]
  (js/crypto.subtle.encrypt params key data))

(defn decrypt [data params key]
  (js/crypto.subtle.decrypt params key data))

;; hash: SHA-256, SHA-384, SHA-512
(defn digest [data & {:keys [hash] :or {hash "SHA-256"}}]
  (js/crypto.subtle.digest hash data))

(comment
  (do
    (def res (atom nil))
    (def m "hello, world")
    (-> (promesa.core/let [h (digest (b/str->bytes m))]
          (reset! res h))
        (promesa.core/catch #(reset! res %)))))
