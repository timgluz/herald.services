(ns herald.services.clients
 (:require [blancas.morph.core :refer :all]
           [blancas.morph.monads :refer :all]
           [clj-http.client :as http]
           [cemerick.url :refer [url]]
           [schema.core :as s]
           [schema.macros :as sm]
           [herald.services.schemas :as schemas]
           [taoensso.timbre :as log])
 (:import [herald.services.schemas SRResponse SRError]))

;;-- PROTOCOLS

(defprotocol IURLBuilder
  (to-url [this path] [this path query-params]))

(defprotocol IAuthorizer
  (append-auth [this request-dt] "adds authorization info to request"))

(defprotocol IRequestBuilder
  (build-request [this method path query-params form-params]))

(defprotocol IRPC_Client
  (rpc-call [this method path]
            [this method path query-params]
            [this method path query-params extra-client-opts]))

;;-- RPC client implementations
(defn build-url
  [api-url path query-params]
  (let [path-str (condp instance? path
                   String path
                   Number (str path)
                   clojure.lang.Keyword (name path)
                   clojure.lang.Seqable (->> path (interpose \/) (apply str)))]
    (-> api-url
        (url path-str)
        (assoc :query query-params)
        str)))

(defn do-request
  "does plain HTTP request and returns response boxed into Either-monad"
  [request-dt]
    (make-either
      (http/request request-dt)))


;;-- Client implementations

;;TODO: refactor as schema/defrecord as Github client
(defrecord VeyeClient [api-url auth client-opts]
  IRequestBuilder
  (build-request [this method path query-params extra-client-opts]
    (let [url-str (build-url (:api-url this) path {})]
      (merge (:client-opts this)
             {:method method
              :url url-str}
             (when-not (:empty? query-params)
               {:query-params query-params})
             (when-not (:empty? extra-client-opts)
               extra-client-opts))))
  IAuthorizer
  (append-auth [this request-dt]
    (assoc-in request-dt
              [:query-params :api_key]
              (get-in this [:auth :secret])))

  IRPC_Client
  (rpc-call [this method path]
    (rpc-call this method path {} {}))
  (rpc-call [this method path query-params]
    (rpc-call this method path query-params {}))


  (rpc-call [this method path query-params extra-client-opts]
    (let [request-dt (build-request this method path query-params extra-client-opts)]
      (either [resp (do-request (append-auth this request-dt))]
        (do
          ; left form
          (log/error "VeyeClient:rpc-call failed: " path "\n " resp)
          (left (assoc resp
                       :body {:error :request-error
                              :msg (str "VeyeClient cant access resource: " path)
                              :data {:path path
                                     :query-params query-params
                                     :body (:body resp)}})))
        (right resp)))))


(sm/defrecord GithubClient
  [api-url :- s/Str
   auth :- schemas/Auth
   client-opts :- schemas/ClientOptions]
  IRequestBuilder
  (build-request [this method path query-params extra-client-opts]
    (let [url-str (build-url (:api-url this) path query-params)]
      (merge (:client-opts this)
             {:method method
              :url url-str}
             #_(when-not (:empty? query-params)
               {:query-params query-params})
             (when-not (:empty? extra-client-opts)
               extra-client-opts))))
  IAuthorizer
  (append-auth [this request-dt]
    (assoc-in request-dt
              [:headers "Authorization"]
              (format "token %s" (get-in this [:auth :secret]))))
  IRPC_Client
  (rpc-call [this method path]
    (rpc-call this method path {} {}))
  (rpc-call [this method path query-params]
    (rpc-call this method path query-params {}))
  (rpc-call [this method path query-params extra-client-opts]
    (let [request-dt (build-request this method path query-params extra-client-opts)]
      (either [resp (do-request (append-auth this request-dt))]
        (do
          (log/error "GithubClient:rpc-call failed: " path "\n" resp)
          (left (SRError. (:status resp)
                          (str "Github client cant access resource: " path)
                          resp)))
        (right (SRResponse. (:status resp) (:headers resp) (:body resp)))))))

;;-- CONSTRUCTORS
(def default-urls {::veye "https://www.versioneye.com/api/v2"
                   ::github "https://api.github.com"})

(def default-http-client-opts {:socket-timeout 1000
                               :conn-timeout 1000
                               :throw-entire-message? true
                               :headers {"User-Agent" "Herald (info@versioneye.com)"}})

(def default-json-client-opts {:coerce :unexceptional
                               :as :json})

(defmulti make-client (fn [client-type & _] client-type))

(defmethod make-client :veye [_ auth client-opts]
  (let [api-url (if-let [url (:url client-opts)]
                  url
                  (::veye default-urls))
        client-opts_ (merge default-http-client-opts
                            default-json-client-opts
                            client-opts)]
    (VeyeClient. api-url auth client-opts_ )))

(defmethod make-client :github [_ auth client-opts]
  (let [api-url (if-let [url (:url client-opts)]
                  url
                  (::github default-urls))
        client-opts_ (merge default-http-client-opts
                            default-json-client-opts
                            {:headers {"Accept" "application/vnd.github.v3+json"}})]
    (GithubClient. api-url auth client-opts_ )))


(defmethod make-client :default [client-type]
  (throw (IllegalArgumentException.
           (str "Unsupported client type: " client-type))))


