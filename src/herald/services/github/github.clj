(ns herald.services.apis.github
  (:require [herald.core.sources.api-utils :as api-utils]
            [clojure.core.async :refer [<! <!! >! >!! close! chan go go-loop]
                                :as async]
            [taoensso.timbre :as log]))

(def default-headers {"User-Agent" "HeraldClient (info@versioneye.com)"
                      "Accept" "application/vnd.github.v3+json"})
(def not-nil? (comp not nil?))

;; github api accessors
(defn get-resource
  ([path-items api-key]
   (get-resource path-items api-key {}))
  ([path-items api-key extra-params]
    (api-utils/request-api-resource
      :get api-url
      path-items
      api-key
      (merge {:headers default-headers} extra-params))))

;;TODO: add ratelimit func

(defn get-repo-branches
  [repo-name api-key]
  (get-resource ["/repos" repo-name "branches"] api-key))

(defn get-repo-tree
  [repo-name sha api-key]
  (get-resource ["/repos" repo-name "git/trees" sha]
                api-key
                {:query-params {:recursive 1}}))

(defn get-file-content
  "gets encoded file content on the path by the ref-key
    ref-key may be branch name/commit sha or tag id"
  [repo-name ref-key file-path api-key]
  (get-resource ["/repos" repo-name "contents" file-path]
                api-key
                {:query-params {:ref ref-key}}))


