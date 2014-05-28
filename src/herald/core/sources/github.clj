(ns herald.core.sources.github
  (:require [herald.core.scm :as scm]
            [herald.core.sources.api-utils :refer [process-response]
                                           :as api-utils]
            [taoensso.timbre :as log]))

(def token-key "8e5d78cd111578074c0daadf7c88aedcc1ece571")
(def api-url "https://api.github.com")
(def default-headers {"User-Agent" "HeraldClient (info@versioneye.com)"
                      "Accept" "application/vnd.github.v3+json"})
(def not-nil? (comp not nil?))

;; github api accessors
(defn get-resource
  [path-items api-key & {:as extra-params}]
  (api-utils/request-api-resource
    :get api-url path-items api-key
    (merge {:headers default-headers} extra-params)))

;; resource functions
(defn get-current-user
  "fetch users github profile information"
  [api-key]
  (process-response
    (get-resource "/user" api-key)))

(defn get-user-repos-page
  "fetch repos on the page X of the paginated response"
  [user-id api-key
   & {:keys [page all? per-page async?]
      :or {page 1 all? true, per-page 30, async? false}}]
  (get-resource  ["/users" user-id "repos"] api-key
    :query-params {:page page :per_page per-page
                    :type "all" :sort "pushed"
                    :direction "desc"}
    :async? async?))

(defn get-last-page-number
  [links]
  (if (empty? links)
    0 ;; do nothing when there's no pagination links in response
    (let [last-url (get-in links [:last :href])]
      (-> last-url api-utils/parse-url :query (get "page") read-string))))

(defn paginate-through-links
  "requests all links from Links header."
  [request-fn]
  (let [first-response (process-response (request-fn 1 false) :uncut? true)
        last-page (-> first-response :links get-last-page-number)
        tasks (for [page-nr (range 2 (inc last-page))]
                (request-fn page-nr true))]
    (concat
      (:body first-response)
      (if (< last-page 2)
        []
        (apply concat
          (for [task tasks]
            (process-response @task)))))))

(defn get-user-repos
  "fetches all github repos user is owner or admin"
  [user-id api-key]
  (letfn [(request-fn [page-nr async?]
            (get-user-repos-page user-id api-key :page page-nr :async? async?))]
    (paginate-through-links request-fn)))


(defn get-user-orgs
  [user-id api-key]
  (letfn [(request-fn [page-nr async?]
            (get-resource ["/users" user-id "orgs"]
                          api-key
                          :query-params {:page page-nr}
                          :async? async?))]
    (paginate-through-links request-fn)))

(defn get-org-repos
  [org-id api-key]
  (letfn [(request-fn [page-nr async?]
            (get-resource ["/orgs" org-id "repos"]
                          api-key
                          :query-params {:page page-nr :type "all"}
                          :async? async?))]
    (paginate-through-links request-fn)))

(defn get-repo-branches
  [repo-name api-key]
  (process-response
    (get-resource ["/repos" repo-name "branches"]
                  api-key)))

(defn get-repo-tree
  [repo-name sha api-key]
  (process-response
    (get-resource ["/repos" repo-name "git/trees" sha]
                  api-key
                  :query-params {:recursive 1})))

(defn get-file-content
  "gets encoded file content on the path by the ref-key
    ref-key may be branch name/commit sha or tag id"
  [repo-name ref-key file-path api-key]
  (process-response
    (get-resource ["/repos" repo-name "contents" file-path]
                  api-key
                  :query-params {:ref ref-key})))


