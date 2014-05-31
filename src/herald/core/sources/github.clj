(ns herald.core.sources.github
  (:require [herald.core.scm :as scm]
            [herald.core.sources.api-utils :as api-utils]
            [clojure.core.async :refer [<! <!! >! >!! close! chan go go-loop]
                                :as async]
            [taoensso.timbre :as log]))

(def token-key "8e5d78cd111578074c0daadf7c88aedcc1ece571")
(def api-url "https://api.github.com")
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

;; resource functions
(defn get-current-user
  "fetch users github profile information"
  [api-key]
  (get-resource "/user" api-key))

(defn get-last-page-number
  [links]
  (if (empty? links)
    0 ;; do nothing when there's no pagination links in response
    (let [last-url (get-in links [:last :href])]
      (-> last-url api-utils/parse-url :query (get "page") read-string))))

(defn run-pagination-pool
  "requests all links from Links header."
  [request-fn start-page-nr end-page-nr]
  (let [response-ch (chan)
        ;; last page has to be bigger or equal start-page
        end-page-nr_ (max start-page-nr end-page-nr)]
    ;;spawn tasks thread
    (async/thread
      (go-loop [cur-page start-page-nr]
        (if (<= cur-page end-page-nr_)
          (do
            (>! response-ch (request-fn cur-page))
            (recur (inc cur-page)))
          (close! response-ch))))
    ;;handle received responses
    (<!!
      (go-loop [pages []]
        (if-let [resp (<! response-ch)]
          (recur (conj pages resp))
          pages)))))

(defn fetch-pagination
  [request-fn & {:keys [start end] :or {start 1}}]
  (let [first-response (request-fn start)
        last-page-nr  (if (nil? end)
                        (-> first-response :links get-last-page-number)
                        end)]
    (->>
      (run-pagination-pool request-fn (inc start) last-page-nr)
      (map :body)
      (apply concat)
      (concat (:body first-response)))))

(defn fetch-paginated-resource
  "loads multiple pages of the API resource"
  ([path-items api-key]
   (fetch-paginated-resource path-items api-key {}))
  ([path-items api-key query-params & {:keys [start end] :or {start 1}}]
    (letfn [(request-fn [page-nr]
              (get-resource path-items
                            api-key
                            {:raw-response? true
                             :query-params (merge {:page page-nr}
                                                   query-params)}))]
      (fetch-pagination request-fn :start start :end end))))

(defn get-user-repos
  "fetches all github repos user is owner or admin"
  [user-id api-key & {:keys [per-page sorted-by sorted-dir]
                      :or {per-page 30, sorted-by "pushed", sorted-dir "asc"}}]
  (fetch-paginated-resource
    ["/users" user-id "repos"]
    api-key
    {:per_page per-page
     :type "all"
     :sort sorted-by
     :direction sorted-dir}))

(defn get-user-orgs
  [user-id api-key]
  (fetch-paginated-resource ["/users" user-id "orgs"] api-key))

(defn get-org-repos
  [org-id api-key]
  (fetch-paginated-resource ["/orgs" org-id "repos"] api-key {:type "all"}))

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


