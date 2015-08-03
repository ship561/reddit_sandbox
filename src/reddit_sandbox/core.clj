(ns reddit-sandbox.core
  (require [reddit.clj.core :as reddit]
           [reddit.clj.client :as redditclient]
           [slingshot.slingshot :refer [try+ throw+]]
           [clojure.set :as sets])
  (import java.net.URISyntaxException))

;;;----REDDIT api stuff-------------------------------------------------------------------------
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

(defn get-author-comments [rc author]
  (loop [after nil
         data []
         sleep? false]
    (when sleep? (Thread/sleep 2000))
    (let [xxx (redditclient/urlopen
               (str (my-url-builder (str "/user/" author "/comments"))
                    (my-build-pagination :after after :limit 100)) nil)
          d (concat data (-> xxx :data :children))
          new-after (-> xxx :data :after)]
      (if (nil? new-after)
        d
        (recur new-after
               d
               true)))))

(defn get-author-subreddits
  "Get the subreddits a particular author has commented to"
  [rc author]
  (when-let [resp (try+ (get-author-comments rc author)
                        (catch [:status 404] {:keys [request-time headers body]};banned
                          (prn :error author body))
                        (catch [:status 521] {:keys [request-time headers body]} (prn :521 body))
                        (catch URISyntaxException _ (prn :uri));nonstandard character
                        (catch Object _ {:author author} (prn :error author)))];deleted author
    (map #(-> % :data :subreddit) resp)))

;;;-----------------------------------------------------------------------------------------------

(defn jaccard-index
  "Similarity between sets"
  [A B]
  (let [A (set A)
        B (set B)]
    (/ (count (sets/intersection A B))
      (count (sets/union A B)))))

(defn get-neighbors [n usr]
  (->> (map (partial jaccard-index usr) sample100)
       (map #(vector (set %1) %2) sample100)
       (remove #(zero? (second %)))
       (sort-by second >) ;for k-NN
       (take n)))

(def sample100
  (let [rc (reddit/login)]
    (->> (repeatedly (fn []
                       (Thread/sleep 2000)
                       (get-author-subreddits rc (get-random-author))))
         (remove nil?)
         (take 100)
         doall)))

(defn make-recommendation [user]
  (let [usr (get-author-subreddits (reddit/login) user)
        dists (get-neighbors 10 usr)]
    (->> dists
         (map (fn [[subreddits _]] (sets/difference subreddits (set usr))))
         (apply concat)
         frequencies ;identify common subreddits
         (sort-by second >);sort most commmon subreddits among k-NN
         (map first) ;return top subreddits
         (take 10))))
