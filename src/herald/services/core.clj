(ns herald.services.core
  (:require [fnhouse.routes :refer [root-handler]]
            [fnhouse.handlers :refer [nss->handlers-fn curry-resources]]
            [schema.core :as s]
            [schema.macros :as sm]
            [taoensso.timbre :as log]
            [herald.services.apis [veye :as veye]]))

;;-- REQUEST DISPATCHER

;- map of handlers namespaces
(def handlers-nss {"veye" 'herald.services.apis.veye})

(sm/defn make-dispatcher
  "returns dispatcher function, which accepts any schemas/SRRequest as input argument"
  [fn-nss :- {s/Str s/Symbol}
   resources :- {s/Keyword s/Any}]
  #_(->> fn-nss
        nss->handlers-fn
        ( #(% {}))
        root-handler)
  (->> fn-nss
       (handlers/nss-proto-handlers)))

