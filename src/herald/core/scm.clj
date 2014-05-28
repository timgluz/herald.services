;;SCM
;;imports data from 3rd party apis

(ns herald.core.scm
  (:require [clojurewerkz.meltdown
              [reactor :as mr]
              [selectors :refer [$] :as mr-sel]]
            [clj-http.client :as http]
            [taoensso.timbre :as log]))

(def scm-reactor (mr/create :dispatcher-type :ring-buffer))
(def default-client-opts {:throw-entire-message? true
                          :ignore-unknown-host? true
                          :conn-timeout 5000
                          :socket-timeout 5000})

(defn fetch-uri
  [event]
  (try
    (log/debug "Got request:\n" (:data event))
    (http/request (merge
                    default-client-opts
                    (:data event)))
    (catch Exception e
      {:error (.getMessage e)})))

;; subsribe to events
(mr/receive-event
  scm-reactor
  ($ "http-request")
  (fn [request-data] (fetch-uri request-data)))


;; request/event dispatcher
(defn make-http-request
  [request-map & {:keys [async] :or {async true}}]
  (let [response-promise (promise)
        callback-fn (fn [ev] (deliver response-promise (:data ev)))]
    (mr/send-event
      scm-reactor "http-request" request-map callback-fn)
    (if (true? async)
      response-promise
      (deref response-promise))))



