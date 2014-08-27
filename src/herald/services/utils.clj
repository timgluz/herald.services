(ns herald.apis.utils
  (:require [cemerick.url :refer [url url-encode]]
            [cheshire.core :refer [parse-string]]
            [clj-http.client :as http]
            [taoensso.timbre :as log]))

(defn get-template-keys
  [^String template-str]
  (->> template-str
       (re-seq #"(?<=[\/ | \.]\:\w+)")
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





;;--------------------------------------------
(def default-client-opts
  {:socket-timeout 2000
   :conn-timeout 1000
   :debug false})

(def default-request-map
  {:method :get
   :headers {"User-Agent" "Herald core client (info@versioneye.com)"
             "Connection" "Keep-Alive"}})

(defn build-url
  "builds proper URL for apis by using the url of host and
  the path to the API resource. You can also add an optional
  map with query parameters:
  Usage:
    (build-url \"https://api.github.com\" \"path/to/resource\")
    ;; https://api.github.com/path/to/resource"
  ([api-url path]
    (build-url api-url path {}))
  ([api-url path query-params]
    (-> api-url
      (url path)
      (assoc :query query-params)
      str)))

(defn parse-url
  "splits a string of url into components"
  [url-string]
  (when url-string
    (url url-string)))

(defn api-url-builder
  "Constructor function, which builds a closure with api-url and
  returns function, which you can use to build API url without
  specifing api-url all the time.
  Usage:
    (def make-git-url (api-url-builder \"https://api.github.com\"))
    (make-git-url [\"/repos\" \"timgluz\" \"contents\"])
    ;;it also accepts keywords params for additional query params
    (make-git-url [\"feed\"] :since \"today\")"
  [api-url]
  (fn [path-items & query-params]
    (build-url api-url
              (->>
                (if (string? path-items) [path-items] path-items)
                (interpose \/)
                (apply str))
              (apply hash-map query-params))))

(defn build-request-map
  ([method url api-key]
    (build-request-map method url api-key {}))
  ([method url api-key extra-params]
    (let [auth-token (format "token %s" api-key)]
      (-> default-request-map
        (merge {:method method, :url url} extra-params)
        (assoc-in [:headers "Authorization"] auth-token) ;;TODO: make it universal
        doall))))


(defn process-response
  "very tasks specific response handler, which returns parsed content of response;
  unless error occured, then it return Clojure map with error type, message and
  original/untouched response

  Usage:
    (process-response (http/get \"api.com/url\"))"
  [{:keys [error body] :as response}
   & {:keys [raw-response?]
      :or {raw-response? true}}]
  (if (nil? error)
    (if (false? raw-response?)
      (parse-string body true)
      (assoc response :body (parse-string body true)))
    ;else
    (do
      (log/error "#-- failed request: \n" response)
      {:error error
        :msg "failed request"
        :response response})))

(defmacro request-api-resource
  "macro that simplifies requesting API resources."
  [method api-url path-items api-key request-params]
  `(let [to-api-url# (api-url-builder ~api-url)
         raw-response?# (get ~request-params :raw-response? false)
         request-map# (build-request-map ~method
                                         (to-api-url# ~path-items)
                                         ~api-key
                                         ~request-params)]
    (try
      (process-response
        (http/request (merge default-client-opts request-map#))
        :raw-response? raw-response?# )
      (catch Exception e#
        {:error (.getMessage e#)}))))

;;TODO: REST resource

(defn make-req
  [client method ])



