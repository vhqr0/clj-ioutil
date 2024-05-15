(ns clj-ioutil.http.data
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clj-json.core :as json]
            [clj-ioutil.bytes :as b]))

;;; x-www-form-urlencoded

(defn- str->form-urlencoded [s]
  (str/replace (b/str->urlencoded s) "%20" "+"))

(defn- form-urlencoded->str [u]
  (b/urlencoded->str (str/replace u "+" "%20")))

(defn clj->form [x]
  (assert (map? x))
  (->> x
       (map
        (fn [[k v]]
          (str (str->form-urlencoded (if (keyword? k) (name k) k))
               \=
               (str->form-urlencoded (str v)))))
       (str/join \&)))

(defn form->clj [x & {:keys [keywordize] :or {keywordize false}}]
  (->> (str/split x #"&")
       (map
        #(let [[k v] (str/split % #"=" 2)]
           (assert v)
           (let [k (form-urlencoded->str k)
                 v (form-urlencoded->str v)
                 k (if-not keywordize k (keyword k))]
             [k v])))
       (into {})))

;;; json

(defn clj->json [x]
  (json/write-string x))

(defn json->clj [x  & {:keys [keywordize] :or {keywordize false}}]
  (binding [json/*read-keyfn* (if-not keywordize identity keyword)]
    (json/read-string x)))

;;; edn

(defn clj->edn [x]
  (pr-str x))

(defn edn->clj [x & opts]
  (edn/read-string x))

;;; data

(def content-type
  {:form "application/x-www-form-urlencoded"
   :json "application/json"
   :edn "application/edn"})

(defn clj->data [x type]
  (case type
    :form (clj->form x)
    :json (clj->json x)
    :edn (clj->edn x)))

(defn data->clj [x type & opts]
  (case type
    :form (apply form->clj x opts)
    :json (apply json->clj x opts)
    :edn (apply edn->clj x opts)))
