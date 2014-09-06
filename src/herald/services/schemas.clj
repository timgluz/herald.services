(ns herald.services.schemas
  (:require [schema.core :as s]
            [schema.macros :as sm]))

(defn matches-schema?
  [schema block]
  (and (map? block)
       ((comp empty? s/check) schema block)))

(defn response-walker [schema coerce-fn]
  (s/start-walker
    (fn [s]
      (let [walk (s/walker s)]
        (fn [x]
          (let [result (walk x)]
            (coerce-fn result)))))
    schema))

(def Paging {:current s/Int
             :per-page s/Int
             :total s/Int  ;no. pages aka last page
             :total-items s/Int})

(def Query {:query s/Str
            (s/optional-key :languages) [s/Str]})

(def Auth {:key s/Str
           :secret s/Str})

(def ClientOptions {:url s/Str
                    s/Keyword s/Any})

(sm/defrecord SRMessage
  [client-opts  :- {s/Keyword s/Any}
   data         :- {s/Keyword s/Any}])

(sm/defrecord SRResponse
  [status :- s/Int
   headers :- s/Any
   body   :- s/Any])

(sm/defrecord SRError
  [status :- s/Int
   msg    :- s/Str
   data   :- {s/Any s/Any}])

(sm/defrecord SRPagedEntity
  [data   :- [s/Any]
   paging :- Paging])

(sm/defrecord SREntity
  [data :- {s/Keyword s/Any}])

;; body is only allowed for PostRequest
(sm/defrecord GetRequest
  [request-method :- s/Keyword
   uri :- s/Str
   query-params :- {s/Keyword s/Any}])


