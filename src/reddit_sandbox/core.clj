(ns reddit-sandbox.core
  (require [reddit.clj.core :as reddit]
           [reddit.clj.client :as redditclient]
           [slingshot.slingshot :refer [try+ throw+]]
           [clojure.set :as sets])
  (import java.net.URISyntaxException))

;;;----REDDIT api stuff-------------------------------------------------------------------------
(defn my-build-comment-url
  "Probably deprecated"
  [reddit_id]
  (str "http://www.reddit.com/user/" reddit_id "/comments.json"))

(defn my-build-pagination
  [& {:keys [after count limit]
      :or {count 0 limit 25}}]
  (str "?"
       (and after (str "after=" after))
       (and after count "&")
       "count=" count "&"
       "limit=" limit))

(defn my-url-builder [url]
  (str "http://www.reddit.com/" url ".json"))

(defn get-random-subreddit-post
  "For sampling a random reddit post"
  []
  (first
   (#'redditclient/parse-reddits
    (first ;return was [topic [comments]]
     (redditclient/urlopen
      (#'redditclient/build-subreddit-url "random" "random" nil nil) nil)))))

(defn get-random-author "Get author of random post" [] ((get-random-subreddit-post) :author))

(defn get-author-subreddits
  "Get the subreddits a particular author has commented to"
  [rc author]
  (when-let [resp (try+ (reddit/user-comments rc author)
                        (catch [:status 404] {:keys [request-time headers body]};banned
                          (prn :error author body))
                        (catch URISyntaxException _ (prn :uri));nonstandard character
                        (catch Object _ {:author author} (prn :error author)))];deleted author
    (map :subreddit resp)))

;;;-----------------------------------------------------------------------------------------------

(defn jaccard-index
  "Similarity between sets"
  [A B]
  (let [A (set A)
        B (set B)]
    (/ (count (sets/intersection A B))
      (count (sets/union A B)))))



(def sample100
  (let [rc (reddit/login)]
    (->> (repeatedly (fn []
                       (Thread/sleep 2000)
                       (get-author-subreddits rc (get-random-author))))
         (remove nil?)
         (take 100)
         doall)))
