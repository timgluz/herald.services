(defproject herald.core.scm "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-http "0.9.1"]
                 [cheshire "5.3.1"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [com.cemerick/url "0.1.1"]
                 [com.taoensso/timbre "3.2.1"]]
  :profiles {:dev
              {:dependencies [[org.clojure/clojure "1.6.0"]
                              [midje "1.6.3"]]
               :plugins [[lein-midje "3.0.0"]]}})
