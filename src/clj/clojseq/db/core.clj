(ns clojseq.db.core
  (:require
    [xtdb.api :as xt]
    [mount.core :refer [defstate]]
    [clojseq.config :refer [env]])
  (:import (java.util UUID)))

(defstate node
  :start (doto (xt/start-node (:xtdb-config env))
           xt/sync)
  :stop (-> node .close))

(defn- put-user!
  [node user]
  (let [user-doc (assoc user :xt/id (:user/id user)
                             :clojseq/type :user)]
    (xt/await-tx
      node
      (xt/submit-tx node [[::xt/put user-doc]]))))

(defn- remove-type-and-xtdb-id
  [user-doc]
  (dissoc user-doc :clojseq/type :xt/id))

(defn create-user!
  "e.g.
    (create-user! node {:user/email \"test@example.com\"
                        :user/name \"Example user\"})"
  [node user]
  (let [user-with-id (assoc user :user/id (UUID/randomUUID))]
    (put-user! node user-with-id)
    user-with-id))

(defn update-user!
  [node user]
  (put-user! node user))

(defn find-user-by-attribute
  [node attr value]
  (-> (xt/q
        (xt/db node)
        {:find  '[(pull user [*])]
         :where [['user attr 'value]
                 ['user :clojseq/type :user]]
         :in    '[attr value]}
        attr value)
      ffirst
      remove-type-and-xtdb-id))

(defn find-user-by-id
  [node id]
  (find-user-by-attribute node :user/id id))

;;;;;;;;;;;;;;;; queries ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dump-db []
  (let [q (xt/q (xt/db node)
                '{:find [i]
                  :where [[i :xt/id _]]})]
    (map (fn [e] (xt/entity (xt/db node) (first e))) q)))

(defn find-tx [kw]
  (xt/entity (xt/db node) kw))

(defn submit-tx [tx]
  (xt/submit-tx node [[::xt/put tx]]))

(defn match-tx [kw tx]
  (xt/submit-tx node [[::xt/match kw tx]]))

(defn delete-tx [kw]
  (xt/submit-tx node [[::xt/delete kw]]))



(defn q [query & args]
  (apply xt/q (xt/db node) query args))

;;;;;;;;;;;;;;; limma-voom ;;;;;;;;;;;;;;;;;;;;;;
(defn submit-fit-method [fit-method]
  (submit-tx {:xt/id :limma/current-fit-method
              :name fit-method}))

(defn submit-limma-trend [trend]
  (submit-tx {:xt/id :limma/trend
              :trend trend}))

(defn submit-limma-trend-method [trend-method]
  (submit-tx {:xt/id :limma/trend-method
              :trend-method trend-method}))
