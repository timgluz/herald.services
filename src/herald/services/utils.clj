(ns herald.services.utils
  (:require [cemerick.url :refer [url url-encode]]
            [cheshire.core :refer [parse-string]]
            [clj-http.client :as http]
            [taoensso.timbre :as log]))

(defn get-template-keys
  [^String template-str]
  (->> template-str
       (re-seq #"(?<=[\/|\.]\:\w{1,50})")
       (map read-string)
       (into #{})))

(defn check-replacement
  [^String template-str value-dt]
  (let [templ-keys (get-template-keys template-str)
        value-keys (set (keys value-dt))
        diffs (clojure.set/difference templ-keys value-keys)]
    (when-not (empty? diffs)
      (throw
        (IllegalArgumentException.
          (str "Template `" template-str "` is missing replacements for: " diffs))))))


(defn match-path-template
  [url-template replacement-dt]
  (check-replacement url-template replacement-dt)

  (reduce (fn [acc [tmpl-key tmpl-val]]
            (clojure.string/replace acc (str tmpl-key) (str tmpl-val)))
          url-template
          replacement-dt))


