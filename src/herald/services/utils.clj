(ns herald.services.utils
  (:require [cemerick.url :refer [url url-encode]]
            [cheshire.core :refer [parse-string]]
            [blancas.morph.monads :refer [make-either]]
            [schema.core :as s]
            [schema.macros :as sm]
            [clj-http.client :as http]
            [taoensso.timbre :as log]
            [herald.services.impl :refer [IRPC_Client]]))

(defn do-request
  "does plain HTTP request and returns response boxed into Either-monad"
  [request-dt]
  (make-either
    (http/request request-dt)))

(sm/defn build-url :- s/Str
  "builds full url from root-url , api path and query-params.
  NB! if path has `/` as first character, then it means add it to root-url,
  and replace already existing path; if you dont use `/` as first character
  of path string, then it will be appended to already existing path.
  Example:
    (build-url \"https://www.versioneye.com/api/v2\" \"/me\" nil)
    returns: https://www.versioneye.com/me
    BUT NOT: https://www.versioneye.com/api/v2/me
  "
  [api-url :- s/Str
   path :- (s/either s/Str s/Num s/Keyword [s/Any])
   query-params :- {s/Keyword s/Any}]
  (let [query-params_ (if (empty? query-params)
                        {} ;;if query-params is nil
                        query-params)
        path-str (condp instance? path
                   String path
                   Number (str path)
                   clojure.lang.Keyword (name path)
                   clojure.lang.Seqable (->> path (interpose \/) (apply str)))]
    (try
      (-> api-url
        (url path-str)
        (assoc :query query-params_)
        str)
      (catch Exception e
        (log/error "build-url failed: \n"
                   {:url api-url
                    :path path
                    :query-params query-params}
                   "\n Reason: \n"
                   (.getMessage e))))))


(sm/defn build-request
  [client :- IRPC_Client
   method  :- (s/enum :get :post :put :delete :patch)
   path  :- (s/either s/Keyword s/Str [s/Any])
   query-params :- {s/Keyword s/Any}
   extra-client-opts :- {s/Any s/Any}]
  (let [url-str (build-url (:api-url client) path query-params)]
    (merge (:client-opts client)
            {:method method
            :url url-str}
            (when-not (:empty? extra-client-opts)
              extra-client-opts))))



;;------------------------------------------
;;TODO: remove if not required anymore
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


