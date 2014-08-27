(ns herald.services.clients
 (:require [blancas.morph.core :refer :all]
           [blancas.morph.monads :refer :all]
           [clj-http.client :as http]
           [cemerick.url :refer [url]]
           [schema.core :as s]
           [schema.macros :as sm]
           [taoensso.timbre :as log]))

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
          (log/error "VeyeClient get-rpc cant access data from: " path "\n " resp)
          (left (assoc resp
                       :body {:error :request-error
                              :msg (str "VeyeClient cant access resource: " path)
                              :data {:path path
                                     :query-params query-params
                                     :body (:body resp)}})))
        (right resp)))))


;;-- CONSTRUCTORS
(def default-urls {::veye "https://www.versioneye.com/api/v2"
                   ::github "https://api.github.com"})

(def default-http-client-opts {:socket-timeout 1000
                               :conn-timeout 1000
                               :throw-entire-message? true})

(def default-json-client-opts {:coerce :unexceptional
                               :as :json})


(defmulti make-client (fn [client-type & _] client-type))

(defmethod make-client :veye [_ auth client-opts]
  (let [api-url (if-let [url (:api-url client-opts)]
                  url
                  (::veye default-urls))
        client-opts_ (merge default-http-client-opts
                            default-json-client-opts
                            client-opts)]
    (VeyeClient. api-url auth client-opts_ )))

(defmethod make-client :default [client-type]
  (throw (IllegalArgumentException.
           (str "Unsupported client type: " client-type))))


