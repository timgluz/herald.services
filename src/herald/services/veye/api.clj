(ns herald.services.veye.api
  (:require [plumbing.core :refer :all]
            [schema.core :as s]
            ;[blancas.morph.core :refer [left right]]
            [blancas.morph.monads :refer [either left right]]
            [herald.services.clients :refer [make-client] :as clients]
            [herald.services.schemas :as schemas]
            [clojure.string :as string]
            [taoensso.timbre :as log]
            [herald.services.veye.core :as veye])
  (:import [herald.services.schemas SRMessage SRResponse SRError]))



(defnk $search$:term$GET
  "searches software packages on VersionEye API"
  ;- meta info
  {:responses {200 SRResponse}}

  ;- input args
  [[:request [:uri-args term :- s/Str]
             [:query-params]]
   [:resources auth]]

  ;- fn body
  (let [auth {:key "" :secret ""}
        client (make-client :veye auth {})]
    (either [resp (veye/search client term {})]
      (SRResponse. (:status resp) {} (:msg resp))
      (SRResponse. 200 {} resp) ;;iff success
     )))


(comment
  (require '[herald.services.core :as services] :reload-all)
  (require '[herald.services.veye.apis :as veye-api] :reload-all)
  (import '[herald.services.schemas GetRequest])

  (def resources {:auth (atom {})})
  (def req (GetRequest. :get "/veye/search/log4j" nil))

  (def dispatcher (services/make-dispatcher services/handlers-nss resources))

  )

