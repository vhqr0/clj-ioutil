(ns clj-ioutil.crypto.impl
  (:require [promesa.core :as p]
            [clj-ioutil.bytes :as b]))

;;; params

(defmulti make-algo-params
  (fn [type opts] type))

(defn algo-params [type & {:as opts}]
  (clj->js (make-algo-params type opts)))

;;;; hmac

;; hash: SHA-256, SHA-384, SHA-512
;; length: length of bits
(defmethod make-algo-params :hmac-key-gen [type {:keys [hash length]}]
  (cond-> {"name" "HMAC" "hash" hash}
    length (assoc "length" length)))

;; same as hmac-key-gen
(defmethod make-algo-params :hmac-import [type opts]
  (make-algo-params :hmac-key-gen opts))

;;;; kdf

;; hash: SHA-256, SHA-384, SHA-512
;; salt: bytes
;; info: bytes
(defmethod make-algo-params :hkdf [type {:keys [hash salt info]}]
  {"name" "HKDF" "hash" hash "salt" salt "info" info})

;; hash: SHA-256, SHA-384, SHA-512
;; salt: 16 bytes
;; iterations: int
(defmethod make-algo-params :pbkdf2 [type {:keys [hash salt iterations]}]
  {"name" "PBKDF2" "hash" hash "salt" salt "iterations" iterations})

;;;; cipher

;; iv: 16 bytes
(defmethod make-algo-params :aes-cbc [type {:keys [iv]}]
  {"name" "AES-CBC" "iv" (or iv (b/rand-bytes 16))})

;; counter: 16 bytes
;; length: 0..128
(defmethod make-algo-params :aes-ctr [type {:keys [counter length] :or {length 64}}]
  {"name" "AES-CTR"
   "counter" (or counter (b/rand-bytes 16))
   "length" length})

;; iv: 12 bytes
;; additional-data: bytes
;; tag-length: 32, 64, 96, ..., 128
(defmethod make-algo-params :aes-gcm [type {:keys [iv additional-data tag-length]}]
  (cond-> {"name" "AES-GCM"
           "iv" (or iv (b/rand-bytes 12))}
    additional-data (assoc "additionalData" additional-data)
    tag-length (assoc "tagLength" tag-length)))

;; name: AES-CBC, AES-CTR, AES-GCM, AES-KW
;; length: 128, 192, 256
(defmethod make-algo-params :aes-key-gen [type {:keys [name length]}]
  {"name" name "length" length})

(defmethod make-algo-params :aes-cbc-key-gen [type opts]
  (make-algo-params :aes-key-gen (assoc opts :name "AES-CBC")))

(defmethod make-algo-params :aes-ctr-key-gen [type opts]
  (make-algo-params :aes-key-gen (assoc opts :name "AES-CTR")))

(defmethod make-algo-params :aes-gcm-key-gen [type opts]
  (make-algo-params :aes-key-gen (assoc opts :name "AES-GCM")))

(defmethod make-algo-params :aes-kw-key-gen [type opts]
  (make-algo-params :aes-key-gen (assoc opts :name "AES-KW")))

;;;; ec

;; public: ec public key
(defmethod make-algo-params :ecdh [type {:keys [public]}]
  {"name" "ECDH" "public" public})

;; hash: SHA-256, SHA-384, SHA-512
(defmethod make-algo-params :ecdsa [type {:keys [hash]}]
  {"name" "ECDSA" "hash" hash})

;; name: ECDSA, ECDH
;; named-curve: P-256, P-384, P-521
(defmethod make-algo-params :ec-key-gen [type {:keys [name named-curve]}]
  {"name" name "namedCurve" named-curve})

(defmethod make-algo-params :ecdh-key-gen [type opts]
  (make-algo-params :ec-key-gen (assoc opts :name "ECDH")))

(defmethod make-algo-params :ecdsa-key-gen [type opts]
  (make-algo-params :ec-key-gen (assoc opts :name "ECDSA")))

;; same as ec-key-gen
(defmethod make-algo-params :ec-key-import [type opts]
  (make-algo-params :ec-key-gen opts))

(defmethod make-algo-params :ecdh-key-import [type opts]
  (make-algo-params :ec-key-import (assoc opts :name "ECDH")))

(defmethod make-algo-params :ecdsa-key-import [type opts]
  (make-algo-params :ec-key-import (assoc opts :name "ECDSA")))

;; Impl notes: curve 25519/448 were standardized recently (2024.03),
;; though they were been experimental for years.
;;
;; [https://github.com/w3c/webcrypto/pull/362]
;; [https://github.com/w3c/webcrypto/issues/196]
;; [https://wicg.github.io/webcrypto-secure-curves/]

(defmethod make-algo-params :x25519 [type {:keys [public]}]
  {"name" "X25519" "public" public})

(defmethod make-algo-params :x448 [type {:keys [public]}]
  {"name" "X448" "public" public})

(defmethod make-algo-params :x25519-key-gen [type opts]
  {"name" "X25519"})

(defmethod make-algo-params :x448-key-gen [type opts]
  {"name" "X448"})

(defmethod make-algo-params :x25519-key-import [type opts]
  {"name" "X25519"})

(defmethod make-algo-params :x448-key-import [type opts]
  {"name" "X448"})

(defmethod make-algo-params :ed25519 [type opts]
  {"name" "Ed25519"})

(defmethod make-algo-params :ed448 [type opts]
  {"name" "Ed448"})

(defmethod make-algo-params :ed25519-key-gen [type opts]
  {"name" "Ed25519"})

(defmethod make-algo-params :ed448-key-gen [type opts]
  {"name" "Ed448"})

(defmethod make-algo-params :ed25519-key-import [type opts]
  {"name" "Ed25519"})

(defmethod make-algo-params :ed448-key-import [type opts]
  {"name" "Ed448"})

;;;; rsa

;; salt-length: Math.ceil((keySizeInBits - 1) / 8) - digestSizeInBytes - 2;
(defmethod make-algo-params :rsa-pss [type {:keys [salt-length]}]
  {"name" "RSA-PSS" "saltLength" salt-length})

;; label: bytes
(defmethod make-algo-params :rsa-oaep [type {:keys [label]}]
  (cond-> {"name" "RSA-OAEP"}
    label (assoc "label" label)))

(def bytes-65537 (b/bmake [1 0 1]))

;; name: RSASSA-PKCS1-v1_5, RSA-PSS, RSA-OAEP
;; modulus-length: >=2048
;; public-exponent: bytes, usually 65537(0x010001)
;; hash: SHA-256, SHA-384, SHA-512
(defmethod make-algo-params :rsa-hashed-key-gen
  [type {:keys [name modulus-length public-exponent hash]
         :or {modulus-length 4096 public-exponent bytes-65537}}]
  {"name" name "modulusLength" modulus-length "publicExponent" public-exponent "hash" hash})

(defmethod make-algo-params :rsassa-pkcs1-hashed-key-gen [type opts]
  (make-algo-params :rsa-hashed-key-gen (assoc opts :name "RSASSA-PKCS1-v1_5")))

(defmethod make-algo-params :rsa-pss-hashed-key-gen [type opts]
  (make-algo-params :rsa-hashed-key-gen (assoc opts :name "RSA-PSS")))

(defmethod make-algo-params :rsa-oaep-hashed-key-gen [type opts]
  (make-algo-params :rsa-hashed-key-gen (assoc opts :name "RSA-OAEP")))

;; name: RSASSA-PKCS1-v1_5, RSA-PSS, RSA-OAEP
;; hash: SHA-256, SHA-384, SHA-512
(defmethod make-algo-params :rsa-hashed-import [type {:keys [name hash]}]
  {"name" name "hash" hash})

(defmethod make-algo-params :rsassa-pkcs1-hashed-import [type opts]
  (make-algo-params :rsa-hashed-import (assoc opts :name "RSASSA-PKCS1-v1_5")))

(defmethod make-algo-params :rsa-pss-hashed-import [type opts]
  (make-algo-params :rsa-hashed-import (assoc opts :name "RSA-PSS")))

(defmethod make-algo-params :rsa-oaep-hashed-import [type opts]
  (make-algo-params :rsa-hashed-import (assoc opts :name "RSA-OAEP")))

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

(defn- key-usages [usage]
  (if (keyword? usage)
    (array (key-usage usage))
    (to-array (map key-usage usage))))

(def key-format
  {:raw "raw"
   :pkcs8 "pkcs8"
   :spki "spki"
   :jwk "jwk"})

(defn generate-key [params usage & {:keys [extractable] :or {extractable false}}]
  (js/crypto.subtle.generateKey
   params extractable (key-usages usage)))

(defn generate-keypair [& args]
  (p/let [kp (apply generate-key args)]
    [(.-privateKey kp) (.-publicKey kp)]))

(defn export-key [key & {:keys [format] :or {format :raw}}]
  (js/crypto.subtle.exportKey
   (key-format format) key))

(defn import-key
  [data params usage & {:keys [extractable format] :or {extractable false format :raw}}]
  (js/crypto.subtle.importKey
   (key-format format) data
   params extractable (key-usages usage)))

(defn wrap-key [key wrap-params wrap-key & {:keys [format] :or {format :raw}}]
  (js/crypto.subtle.wrapKey
   (key-format format) key
   wrap-key wrap-params))

(defn unwrap-key
  [data params usage wrap-params wrap-key
   & {:keys [extractable format] :or {extractable false format :raw}}]
  (js/crypto.subtle.unwrapKey
   (key-format format) data
   wrap-key wrap-params
   params extractable (key-usages usage)))

(defn derive-bits [key params length]
  (js/crypto.subtle.deriveBits params key length))

(defn derive-key [key params key-params usage & {:keys [extractable] :or {extractable false}}]
  (js/crypto.subtle.deriveKey
   params key
   key-params extractable (key-usages usage)))

;;; crypt

;; hash: SHA-256, SHA-384, SHA-512
(defn digest [data hash]
  (js/crypto.subtle.digest hash data))

(defn sign [data pri params]
  (js/crypto.subtle.sign params pri data))

(defn verify [data sig pub params]
  (js/crypto.subtle.verify params pub sig data))

(defn encrypt [data key params]
  (js/crypto.subtle.encrypt params key data))

(defn decrypt [data key params]
  (js/crypto.subtle.decrypt params key data))

;;; impl

(def digest-hash
  {:sha256 "SHA-256"
   :sha384 "SHA-384"
   :sha512 "SHA-512"})

(defn bytes->digest [data algo]
  (digest data (digest-hash algo)))

(defn bytes->hmac [data key algo]
  (p/let [key (if-not (instance? js/ArrayBuffer key)
                key
                (import-key key (algo-params :hmac-import :hash (digest-hash algo)) :sign))]
    (sign data key "HMAC")))

(defn aead-key->bytes [key algo]
  (export-key key))

(defn bytes->aead-key [data algo]
  (import-key
   data
   (case algo
     :aes128gcm (algo-params :aes-gcm-key-gen :length 128)
     :aes256gcm (algo-params :aes-gcm-key-gen :length 256))
   [:encrypt :decrypt :wrap-key :unwrap-key]
   :extractable true))

(defn- aead-crypt [cryptfn data key iv algo & {:keys [aad]}]
  (p/let [key (if-not (instance? js/ArrayBuffer key)
                key
                (bytes->aead-key key algo))]
    (cryptfn
     data key
     (case algo
       (:aes128gcm :aes256gcm) (algo-params :aes-gcm :iv iv :additional-data aad)))))

(def aead-encrypt (partial aead-crypt encrypt))
(def aead-decrypt (partial aead-crypt decrypt))

(def ec-algo
  {:p256 "P-256"
   :p384 "P-384"
   :p521 "P-521"})

(defn ecdh-generate-keypair [algo] (generate-keypair (algo-params :ecdh-key-gen :named-curve (ec-algo algo)) [:derive-bits :derive-key] :extractable true))
(defn ecdh-pub->bytes [pub] (export-key pub :format :spki))
(defn ecdh-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->ecdh-pub [data algo] (import-key data (algo-params :ecdh-key-import :named-curve (ec-algo algo)) [] :format :spki))
(defn bytes->ecdh-pri [data algo] (import-key data (algo-params :ecdh-key-import :named-curve (ec-algo algo)) [:derive-bits :derive-key] :format :pkcs8))
(defn ecdh-key-exchange [pub pri] (derive-bits pri (algo-params :ecdh :public pub) 384))

(defn ecdsa-generate-keypair [algo] (generate-keypair (algo-params :ecdsa-key-gen :named-curve (ec-algo algo)) [:sign :verify] :extractable true))
(defn ecdsa-pub->bytes [pub] (export-key pub :format :spki))
(defn ecdsa-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->ecdsa-pub [data algo] (import-key data (algo-params :ecdsa-key-import :named-curve (ec-algo algo)) [:sign :verify] :format :spki))
(defn bytes->ecdsa-pri [data algo] (import-key data (algo-params :ecdsa-key-import :named-curve (ec-algo algo)) [:sign :verify] :format :pkcs8))
(defn ecdsa-sign [data pri algo] (sign data pri (algo-params :ecdsa :hash (digest-hash algo))))
(defn ecdsa-verify [data sig pub algo] (verify data sig pub (algo-params :ecdsa :hash (digest-hash algo))))

(defn x25519-generate-keypair [] (generate-keypair (algo-params :x25519-key-gen) [:derive-bits :derive-key] :extractable true))
(defn x25519-pub->bytes [pub] (export-key pub :format :spki))
(defn x25519-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->x25519-pub [data] (import-key data (algo-params :x25519-key-import) [] :format :spki))
(defn bytes->x25519-pri [data] (import-key data (algo-params :x25519-key-import) [:derive-bits :derive-key] :format :pkcs8))
(defn x25519-key-exchange [pub pri] (derive-bits pri (algo-params :x25519 :public pub) 256))

(defn x448-generate-keypair [] (generate-keypair (algo-params :x448-key-gen) [:derive-bits :derive-key] :extractable true))
(defn x448-pub->bytes [pub] (export-key pub :format :spki))
(defn x448-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->x448-pub [data] (import-key data (algo-params :x448-key-import) [] :format :spki))
(defn bytes->x448-pri [data] (import-key data (algo-params :x448-key-import) [:derive-bits :derive-key] :format :pkcs8))
(defn x448-key-exchange [pub pri] (derive-bits pri (algo-params :x448 :public pub) 448))

(defn ed25519-generate-keypair [] (generate-keypair (algo-params :ed25519-key-gen) [:sign :verify] :extractable true))
(defn ed25519-pub->bytes [pub] (export-key pub :format :spki))
(defn ed25519-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->ed25519-pub [data] (import-key data (algo-params :ed25519-key-import) [] :format :spki))
(defn bytes->ed25519-pri [data] (import-key data (algo-params :ed25519-key-import) [:derive-bits :derive-key] :format :pkcs8))
(defn ed25519-sign [data pri] (sign data pri (algo-params :ed25519)))
(defn ed25519-verify [data sig pub] (verify data sig pub (algo-params :ed25519)))

(defn ed448-generate-keypair [] (generate-keypair (algo-params :ed448-key-gen) [:sign :verify] :extractable true))
(defn ed448-pub->bytes [pub] (export-key pub :format :spki))
(defn ed448-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->ed448-pub [data] (import-key data (algo-params :ed448-key-import) [] :format :spki))
(defn bytes->ed448-pri [data] (import-key data (algo-params :ed448-key-import) [:derive-bits :derive-key] :format :pkcs8))
(defn ed448-sign [data pri] (sign data pri (algo-params :ed448)))
(defn ed448-verify [data sig pub] (verify data sig pub (algo-params :ed448)))
