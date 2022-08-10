(ns clojseq.db.utils
      (:require
        [clojseq.db.core :as db]
        [clojseq.config :refer [env]]))

  (defn dump-db []
        (db/dump-db))
  ;;;;;;;;;;;;;;;;; limma ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defn submit-fit-method [fit-method]
        (db/submit-fit-method fit-method))

  (defn find-current-fit-method []
        (:name (db/find-tx :limma/current-fit-method)))

  ;;;;;;;;;; trend
  (defn submit-limma-trend [trend]
        (db/submit-limma-trend trend))

  (defn find-limma-trend []
        (:trend (db/find-tx :limma/trend)))

  (defn submit-limma-trend-method [trend-method]
        (db/submit-limma-trend-method trend-method))

  (defn find-limma-trend-method []
        (:trend-method (db/find-tx :limma/trend-method)))

