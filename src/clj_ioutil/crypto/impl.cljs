(ns clj-ioutil.crypto.impl
  (:require [promesa.core :as p]
            [clj-ioutil.bytes :as b]))

(def digest-algo
  {:sha256 "SHA-256"
   :sha384 "SHA-384"
   :sha512 "SHA-512"})

(defn digest [data algo]
  (js/crypto.subtle.digest (digest-algo algo) data))

(defn hmac [data key algo]
  (p/let [key (js/crypto.subtle.importKey
               "raw" key
               #js {"name" "HMAC" "hash" (digest-algo algo)} false #js ["sign"])]
    (js/crypto.subtle.sign "HMAC" key data)))

;; Impl notes: chacha20-poly1305 is not supported.

(def cipher-key-params
  {:aes128-cbc        #js {"name" "AES-CBC" "length" 128}
   :aes192-cbc        #js {"name" "AES-CBC" "length" 192}
   :aes256-cbc        #js {"name" "AES-CBC" "length" 256}
   :aes128-ctr        #js {"name" "AES-CTR" "length" 128}
   :aes192-ctr        #js {"name" "AES-CTR" "length" 192}
   :aes256-ctr        #js {"name" "AES-CTR" "length" 256}
   :aes128-gcm        #js {"name" "AES-GCM" "length" 128}
   :aes192-gcm        #js {"name" "AES-GCM" "length" 192}
   :aes256-gcm        #js {"name" "AES-GCM" "length" 256}
   ;; :chacha20-poly1305 "ChaCha20-Poly1305"
   })

(def cipher-key-size
  {:aes128-cbc        16
   :aes192-cbc        24
   :aes256-cbc        32
   :aes128-ctr        16
   :aes192-ctr        24
   :aes256-ctr        32
   :aes128-gcm        16
   :aes192-gcm        24
   :aes256-gcm        32
   ;; :chacha20-poly1305 32
   })

(defn cipher-params [algo iv aad]
  (case algo
    (:aes128-cbc :aes192-cbc :aes256-cbc) #js {"name" "AES-CBC" "iv" iv}
    (:aes128-ctr :aes192-ctr :aes256-ctr) #js {"name" "AES-CTR" "counter" iv "length" 64}
    (:aes128-gcm :aes192-gcm :aes256-gcm) (if-not aad
                                            #js {"name" "AES-GCM" "iv" iv}
                                            #js {"name" "AES-GCM" "iv" iv "additionalData" aad})
    ;; :chacha20-poly1305 (if-not aad
    ;;                      #js {"name" "ChaCha20-Poly1305" "iv" iv}
    ;;                      #js {"name" "ChaCha20-Poly1305" "iv" iv "additionalData" aad})
    ))

(defn generate-key [algo]
  (js/crypto.subtle.generateKey
   (cipher-key-params algo) true #js ["encrypt" "decrypt" "wrapKey" "unwrapKey"]))

(defn bytes->key [data algo]
  (p/do
    (assert (= (b/blength data)) (cipher-key-size algo))
    (js/crypto.subtle.importKey
     "raw" data
     (cipher-key-params algo) true #js ["encrypt" "decrypt" "wrapKey" "unwrapKey"])))

(defn key->bytes [key algo]
  (p/let [data (js/crypto.subtle.exportKey "raw" key)]
    (assert (= (b/blength data)) (cipher-key-size algo))
    data))

(defn encrypt [data key iv algo & {:keys [aad]}]
  (p/let [key (if-not (instance? js/ArrayBuffer key) key (bytes->key key algo))]
    (js/crypto.subtle.encrypt (cipher-params algo iv aad) key data)))

(defn decrypt [data key iv algo & {:keys [aad]}]
  (p/let [key (if-not (instance? js/ArrayBuffer key) key (bytes->key key algo))]
    (js/crypto.subtle.decrypt (cipher-params algo iv aad) key data)))

;; Impl notes: curve 25519/448 were standardized recently (2024.03),
;; though they were been experimental for years.
;;
;; [https://github.com/w3c/webcrypto/pull/362]
;; [https://github.com/w3c/webcrypto/issues/196]
;; [https://wicg.github.io/webcrypto-secure-curves/]

(def no-usage #js [])
(def ke-usage #js ["deriveBits" "deriveKey"])
(def sign-usage #js ["sign" "verify"])
(def verify-usage #js ["verify"])

(def pri-usage
  {:ecdh-p256         ke-usage
   :ecdh-p384         ke-usage
   :ecdh-p521         ke-usage
   :ecdsa-p256-sha256 sign-usage
   :ecdsa-p256-sha384 sign-usage
   :ecdsa-p256-sha512 sign-usage
   :ecdsa-p384-sha256 sign-usage
   :ecdsa-p384-sha384 sign-usage
   :ecdsa-p384-sha512 sign-usage
   :ecdsa-p521-sha256 sign-usage
   :ecdsa-p521-sha384 sign-usage
   :ecdsa-p521-sha512 sign-usage
   :x25519            ke-usage
   :x448              ke-usage
   :ed25519           sign-usage
   :ed448             sign-usage
   :rsa-pkcs1-sha256  sign-usage
   :rsa-pkcs1-sha384  sign-usage
   :rsa-pkcs1-sha512  sign-usage
   :rsa-pss-sha256    sign-usage
   :rsa-pss-sha384    sign-usage
   :rsa-pss-sha512    sign-usage})

(def pub-usage
  {:ecdh-p256         no-usage
   :ecdh-p384         no-usage
   :ecdh-p521         no-usage
   :ecdsa-p256-sha256 verify-usage
   :ecdsa-p256-sha384 verify-usage
   :ecdsa-p256-sha512 verify-usage
   :ecdsa-p384-sha256 verify-usage
   :ecdsa-p384-sha384 verify-usage
   :ecdsa-p384-sha512 verify-usage
   :ecdsa-p521-sha256 verify-usage
   :ecdsa-p521-sha384 verify-usage
   :ecdsa-p521-sha512 verify-usage
   :x25519            no-usage
   :x448              no-usage
   :ed25519           verify-usage
   :ed448             verify-usage
   :rsa-pkcs1-sha256  verify-usage
   :rsa-pkcs1-sha384  verify-usage
   :rsa-pkcs1-sha512  verify-usage
   :rsa-pss-sha256    verify-usage
   :rsa-pss-sha384    verify-usage
   :rsa-pss-sha512    verify-usage})

(def rsa-65537 (js/Uint8Array.from [1 0 1]))

(def kp-params
  {:ecdh-p256         #js {"name" "ECDH" "namedCurve" "P-256"}
   :ecdh-p384         #js {"name" "ECDH" "namedCurve" "P-384"}
   :ecdh-p521         #js {"name" "ECDH" "namedCurve" "P-521"}
   :ecdsa-p256-sha256 #js {"name" "ECDSA" "namedCurve" "P-256"}
   :ecdsa-p256-sha384 #js {"name" "ECDSA" "namedCurve" "P-256"}
   :ecdsa-p256-sha512 #js {"name" "ECDSA" "namedCurve" "P-256"}
   :ecdsa-p384-sha256 #js {"name" "ECDSA" "namedCurve" "P-384"}
   :ecdsa-p384-sha384 #js {"name" "ECDSA" "namedCurve" "P-384"}
   :ecdsa-p384-sha512 #js {"name" "ECDSA" "namedCurve" "P-384"}
   :ecdsa-p521-sha256 #js {"name" "ECDSA" "namedCurve" "P-521"}
   :ecdsa-p521-sha384 #js {"name" "ECDSA" "namedCurve" "P-521"}
   :ecdsa-p521-sha512 #js {"name" "ECDSA" "namedCurve" "P-521"}
   :x25519            "X25519"
   :x448              "X448"
   :ed25519           "Ed25519"
   :ed448             "Ed448"
   :rsa-pkcs1-sha256  #js {"name" "RSASSA-PKCS1-v1_5" "hash" "SHA-256" "modulusLength" 4096 "publicExponent" rsa-65537}
   :rsa-pkcs1-sha384  #js {"name" "RSASSA-PKCS1-v1_5" "hash" "SHA-384" "modulusLength" 4096 "publicExponent" rsa-65537}
   :rsa-pkcs1-sha512  #js {"name" "RSASSA-PKCS1-v1_5" "hash" "SHA-512" "modulusLength" 4096 "publicExponent" rsa-65537}
   :rsa-pss-sha256    #js {"name" "RSA-PSS"           "hash" "SHA-256" "modulusLength" 4096 "publicExponent" rsa-65537}
   :rsa-pss-sha384    #js {"name" "RSA-PSS"           "hash" "SHA-384" "modulusLength" 4096 "publicExponent" rsa-65537}
   :rsa-pss-sha512    #js {"name" "RSA-PSS"           "hash" "SHA-512" "modulusLength" 4096 "publicExponent" rsa-65537}})

(defn ke-params [algo pub]
  (case algo
    (:ecdh-p256 :ecdh-p384 :ecdh-p521) #js {"name" "ECDH"   "public" pub}
    :x25519                            #js {"name" "X25519" "public" pub}
    :x448                              #js {"name" "X448"   "public" pub}))

(def ke-key-size
  {:ecdh-p256 48
   :ecdh-p384 48
   :ecdh-p521 48
   :x25519    32
   :x448      56})

(def sign-params
  {:ecdsa-p256-sha256 #js {"name" "ECDSA" "hash" "SHA-256"}
   :ecdsa-p256-sha384 #js {"name" "ECDSA" "hash" "SHA-384"}
   :ecdsa-p256-sha512 #js {"name" "ECDSA" "hash" "SHA-512"}
   :ecdsa-p384-sha256 #js {"name" "ECDSA" "hash" "SHA-256"}
   :ecdsa-p384-sha384 #js {"name" "ECDSA" "hash" "SHA-384"}
   :ecdsa-p384-sha512 #js {"name" "ECDSA" "hash" "SHA-512"}
   :ecdsa-p521-sha256 #js {"name" "ECDSA" "hash" "SHA-256"}
   :ecdsa-p521-sha384 #js {"name" "ECDSA" "hash" "SHA-384"}
   :ecdsa-p521-sha512 #js {"name" "ECDSA" "hash" "SHA-512"}
   :ed25519           "Ed25519"
   :ed448             "Ed448"
   :rsa-pkcs1-sha256  #js {"name" "RSASSA-PKCS1-v1_5" "saltLength" 32}
   :rsa-pkcs1-sha384  #js {"name" "RSASSA-PKCS1-v1_5" "saltLength" 48}
   :rsa-pkcs1-sha512  #js {"name" "RSASSA-PKCS1-v1_5" "saltLength" 64}
   :rsa-pss-sha256    #js {"name" "RSA-PSS" "saltLength" 32}
   :rsa-pss-sha384    #js {"name" "RSA-PSS" "saltLength" 48}
   :rsa-pss-sha512    #js {"name" "RSA-PSS" "saltLength" 64}})

(defn generate-keypair [algo]
  (p/let [kp (js/crypto.subtle.generateKey
              (kp-params algo) true (pri-usage algo))]
    [(.-privateKey kp) (.-publicKey kp)]))

(defn bytes->pri [data algo]
  (js/crypto.subtle.importKey
   "pkcs8" data
   (kp-params algo) true (pri-usage algo)))

(defn bytes->pub [data algo]
  (js/crypto.subtle.importKey
   "spki" data
   (kp-params algo) true (pub-usage algo)))

(defn pri->bytes [key algo]
  (js/crypto.subtle.exportKey "pkcs8" key))

(defn pub->bytes [key algo]
  (js/crypto.subtle.exportKey "spki" key))

(defn key-exchange [pub pri algo & {:keys [size]}]
  (js/crypto.subtle.deriveBits
   (ke-params algo pub) pri (bit-shift-left (or size (ke-key-size algo)) 3)))

(defn sign [data pri algo]
  (js/crypto.subtle.sign
   (sign-params algo) pri data))

(defn verify [data sig pub algo]
  (js/crypto.subtle.verify
   (sign-params algo) pub sig data))
