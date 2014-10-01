(ns herald.services.impl)

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


