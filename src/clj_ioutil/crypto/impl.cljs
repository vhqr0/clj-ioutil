(ns clj-ioutil.crypto.impl
  (:require [promesa.core :as p]
            [clj-ioutil.bytes :as b]))

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

(def digest-algo
  {:sha256 "SHA-256"
   :sha384 "SHA-384"
   :sha512 "SHA-512"})

(defn bytes->digest [data algo]
  (digest data (digest-algo algo)))

(defn bytes->hmac [data key algo]
  (p/let [key (if-not (instance? js/ArrayBuffer key)
                key
                (import-key key #js {"name" "HMAC" "hash" (digest-algo algo)} :sign))]
    (sign data key "HMAC")))

(defn aead-key->bytes [key algo]
  (export-key key))

(defn bytes->aead-key [data algo]
  (import-key
   data
   (case algo
     :aes128gcm #js {"name" "AES-GCM" "length" 128}
     :aes256gcm #js {"name" "AES-GCM" "length" 256})
   [:encrypt :decrypt :wrap-key :unwrap-key]
   :extractable true))

(defn- aead-crypt [cryptfn data key iv algo & {:keys [aad]}]
  (p/let [key (if-not (instance? js/ArrayBuffer key)
                key
                (bytes->aead-key key algo))]
    (cryptfn
     data key
     (case algo
       (:aes128gcm :aes256gcm) (if-not aad
                                 #js {"name" "AES-GCM" "iv" iv}
                                 #js {"name" "AES-GCM" "iv" iv "additionalData" aad})))))

(def aead-encrypt (partial aead-crypt encrypt))
(def aead-decrypt (partial aead-crypt decrypt))

(def ec-algo
  {:p256 "P-256"
   :p384 "P-384"
   :p521 "P-521"})

(defn ecdh-generate-keypair [algo] (generate-keypair #js {"name" "ECDH" "namedCurve" (ec-algo algo)} [:derive-bits :derive-key] :extractable true))
(defn ecdh-pub->bytes [pub] (export-key pub :format :spki))
(defn ecdh-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->ecdh-pub [data algo] (import-key data #js {"name" "ECDH" "namedCurve" (ec-algo algo)} [] :format :spki))
(defn bytes->ecdh-pri [data algo] (import-key data #js {"name" "ECDH" "namedCurve" (ec-algo algo)} [:derive-bits :derive-key] :format :pkcs8))
(defn ecdh-key-exchange [pub pri] (derive-bits pri #js {"name" "ECDH" "public" pub} 384))

(defn ecdsa-generate-keypair [algo] (generate-keypair #js {"name" "ECDSA" "namedCurve" (ec-algo algo)} [:sign :verify] :extractable true))
(defn ecdsa-pub->bytes [pub] (export-key pub :format :spki))
(defn ecdsa-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->ecdsa-pub [data algo] (import-key data #js {"name" "ECDSA" "namedCurve" (ec-algo algo)} [:sign :verify] :format :spki))
(defn bytes->ecdsa-pri [data algo] (import-key data #js {"name" "ECDSA" "namedCurve" (ec-algo algo)} [:sign :verify] :format :pkcs8))
(defn ecdsa-sign [data pri algo] (sign data pri #js {"name" "ECDSA" "hash" (digest-algo algo)}))
(defn ecdsa-verify [data sig pub algo] (verify data sig pub #js {"name" "ECDSA" "hash" (digest-algo algo)}))

;; Impl notes: curve 25519/448 were standardized recently (2024.03),
;; though they were been experimental for years.
;;
;; [https://github.com/w3c/webcrypto/pull/362]
;; [https://github.com/w3c/webcrypto/issues/196]
;; [https://wicg.github.io/webcrypto-secure-curves/]

(defn x25519-generate-keypair [] (generate-keypair "X25519" [:derive-bits :derive-key] :extractable true))
(defn x25519-pub->bytes [pub] (export-key pub :format :spki))
(defn x25519-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->x25519-pub [data] (import-key data "X25519" [] :format :spki))
(defn bytes->x25519-pri [data] (import-key data "X25519" [:derive-bits :derive-key] :format :pkcs8))
(defn x25519-key-exchange [pub pri] (derive-bits pri #js {"name" "X25519" "public" pub} 256))

(defn x448-generate-keypair [] (generate-keypair "X448" [:derive-bits :derive-key] :extractable true))
(defn x448-pub->bytes [pub] (export-key pub :format :spki))
(defn x448-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->x448-pub [data] (import-key data "X448" [] :format :spki))
(defn bytes->x448-pri [data] (import-key data "X448" [:derive-bits :derive-key] :format :pkcs8))
(defn x448-key-exchange [pub pri] (derive-bits pri #js {"name" "X448" "public" pub} 448))

(defn ed25519-generate-keypair [] (generate-keypair "Ed25519" [:sign :verify] :extractable true))
(defn ed25519-pub->bytes [pub] (export-key pub :format :spki))
(defn ed25519-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->ed25519-pub [data] (import-key data "Ed25519" [] :format :spki))
(defn bytes->ed25519-pri [data] (import-key data "Ed25519" [:derive-bits :derive-key] :format :pkcs8))
(defn ed25519-sign [data pri] (sign data pri "Ed25519"))
(defn ed25519-verify [data sig pub] (verify data sig pub "Ed25519"))

(defn ed448-generate-keypair [] (generate-keypair "Ed448" [:sign :verify] :extractable true))
(defn ed448-pub->bytes [pub] (export-key pub :format :spki))
(defn ed448-pri->bytes [pri] (export-key pri :format :pkcs8))
(defn bytes->ed448-pub [data] (import-key data "Ed448" [] :format :spki))
(defn bytes->ed448-pri [data] (import-key data "Ed448" [:derive-bits :derive-key] :format :pkcs8))
(defn ed448-sign [data pri] (sign data pri "Ed448"))
(defn ed448-verify [data sig pub] (verify data sig pub "Ed448"))
