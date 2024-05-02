(ns ioutil.http.status
  (:require [clojure.set :as set]))

;; rfc 9110
;; https://developer.mozilla.org/en-US/docs/Web/HTTP/Status

(defn status?       [status] (<= 100 status 599))
(defn info?         [status] (<= 100 status 199)) ;; informational
(defn success?      [status] (<= 200 status 299)) ;; successful
(defn redirect?     [status] (<= 300 status 399)) ;; redirection
(defn client-error? [status] (<= 400 status 499)) ;; client error
(defn server-error? [status] (<= 500 status 599)) ;; server error
(defn error?        [status] (<= 400 status 599)) ;; client/server error

;;; 1xx informational

(def continue 100)
(def switching-protocols 101)

(def info-codes #{continue switching-protocols})

;;; 2xx successful

(def ok 200)
(def created 201)
(def accepted 202)
(def non-authoritative-information 203)
(def no-content 204)
(def reset-content 205)
(def partial-content 206)

(def success-codes #{ok
                     created
                     accepted
                     non-authoritative-information
                     no-content
                     reset-content
                     partial-content})

;;; 3xx redirection

(def multiple-choices 300)
(def moved-permanently 301)
(def found 302)
(def see-other 303)
(def not-modified 304)
(def use-proxy 305)
(def temporary-redirect 307)
(def permanent-redirect 308)

(def redirect-codes #{multiple-choices
                   moved-permanently
                   found
                   see-other
                   not-modified
                   use-proxy
                   temporary-redirect
                   permanent-redirect})

;;; 4xx 5xx error

(def bad-request 400)
(def unauthorized 401)
(def payment-required 402)
(def forbidden 403)
(def not-found 404)
(def method-not-allowed 405)
(def not-acceptable 406)
(def proxy-authentication-required 407)
(def request-timeout 408)
(def conflict 409)
(def gone 410)
(def length-required 411)
(def precondition-failed 412)
(def content-too-large 413)
(def uri-too-long 414)
(def unsupported-media-type 415)
(def range-not-satisfieable 416)
(def expectation-failed 417)
(def misdirected-request 421)
(def unprocessable-content 422)
(def upgrade-required 426)

(def internal-server-error 500)
(def not-implemented 501)
(def bad-gateway 502)
(def service-unavailable 503)
(def gateway-timeout 504)
(def http-version-not-supported 505)

(def client-error-codes #{bad-request
                          unauthorized
                          payment-required
                          forbidden
                          not-found
                          method-not-allowed
                          not-acceptable
                          proxy-authentication-required
                          request-timeout
                          conflict
                          gone
                          length-required
                          precondition-failed
                          content-too-large
                          uri-too-long
                          unsupported-media-type
                          range-not-satisfieable
                          expectation-failed
                          misdirected-request
                          unprocessable-content
                          upgrade-required})

(def server-error-codes #{internal-server-error
                          not-implemented
                          bad-gateway
                          service-unavailable
                          gateway-timeout
                          http-version-not-supported})

(def error-codes (set/union client-error-codes server-error-codes))

;;; status

(def status-codes (set/union info-codes success-codes redirect-codes error-codes))
