(defproject herald.services "0.2.0-SNAPSHOT"
  :description "API connector"
  :url "http://tauho.github.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :dependencies [[clj-http "1.0.0"]
                 [prismatic/fnhouse "0.1.1"]
                 [prismatic/schema "0.2.6"]
                 [cheshire "5.3.1"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [com.cemerick/url "0.1.1"]
                 [org.blancas/morph "0.3.0"]
                 [com.taoensso/timbre "3.2.1"]]
  :profiles {:dev
              {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :test
              {:dependencies [[org.clojure/clojure "1.6.0"]
                              [midje "1.6.3"]]
               :plugins [[lein-midje "3.0.0"]]}})
