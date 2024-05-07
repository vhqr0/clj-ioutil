(defproject ioutil "0.1.0-SNAPSHOT"
  :description "IO library for clojure and clojurescript (WIP)."
  :url "http://github.com/vhqr0/clj-ioutil"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.txt"}
  :dependencies [[org.clojure/clojure "1.12.0-alpha11"]
                 [funcool/promesa "11.0.678"]]
  :profiles
  {:cljs {:dependencies [[thheller/shadow-cljs "2.28.5"]]}})
