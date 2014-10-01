(ns herald.services.clients
 (:require [blancas.morph.core :refer :all]
           [blancas.morph.monads :refer :all]
           [schema.core :as s]
           [schema.macros :as sm]
           [herald.services.veye.core :as veye :refer [->VeyeClient]]
           [herald.services.github.core :as github
                                        :refer [->GithubClient]]
           [taoensso.timbre :as log])
 (:import [herald.services.schemas SRResponse SRError]))

;;-- CONSTRUCTORS
(def default-urls {::veye "https://www.versioneye.com/api/v2"
                   ::github "https://api.github.com"})

(def default-http-client-opts {:socket-timeout 5000
                               :conn-timeout 2500
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
    (->VeyeClient api-url auth client-opts_ )))

(defmethod make-client :github [_ auth client-opts]
  (let [api-url (if-let [url (:url client-opts)]
                  url
                  (::github default-urls))
        client-opts_ (merge default-http-client-opts
                            default-json-client-opts
                            client-opts)]
    (->GithubClient api-url
                    auth
                    (assoc-in  client-opts_
                              [:headers "Accept"]
                              "application/vnd.github.v3+json"))))


(defmethod make-client :default [client-type _ _]
  (throw (IllegalArgumentException.
           (str "Unsupported client type: " client-type))))


;;-- function dispatch by clients aka to make resource agnostic API calls
(def service-nss {:github 'herald.services.github.core
                  :veye 'herald.services.veye.core})

;;usage (def req-fn (get-in service-fns [:github 'get-file-content])
(def service-fns (reduce
                       (fn [acc [k ns-name]]
                         (assoc acc k (ns-interns ns-name)))
                       {}
                       service-nss))

;;-- build source agnostic request function based on client
(defmulti make-req-fn (fn [client _] (class client)))

(defmethod make-req-fn herald.services.github.core.GithubClient
  [client fn-sym]
  (partial
    (get-in service-fns [:github fn-sym])
    client))

(defmethod make-req-fn herald.services.veye.core.VeyeClient
  [client fn-sym]
  (partial
    (get-in service-fns [:veye fn-sym])
    client))

(defmethod make-req-fn :default
  [client fn-sym]
  (throw (Exception. "Unknown client was passed into make-req-fn")))

