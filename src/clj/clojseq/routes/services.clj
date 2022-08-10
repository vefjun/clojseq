(ns clojseq.routes.services
  (:require [clojseq.middleware :as middleware]
            [clojseq.db.core :as db]
            [clojseq.storage.voom :as data]
            [clojseq.plots.plots :as plots]
            [ring.util.http-response :as response])
  (:import (java.util UUID)))

(defn service-routes []
  ["/api"
   {:middleware [middleware/wrap-formats]}
   ["/ping"
    {:get (constantly (response/ok {:message "pong"}))}]
   ["/data/get-selected-counts"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/get-selected-counts params)))}]
   ["/data/get-samples-from-dge"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/get-samples-from-dge params)))}]
   ["/data/get-samples-for-data"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/get-samples-for-data params)))}]
   ["/data/get-samples-column-names"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/get-samples-column-names params)))}]
   ["/data/get-levels"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/get-levels params)))}]
   ["/data/get-design"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/get-design params)))}]
   ["/data/get-top-table"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/get-top-table params)))}]
   ["/data/save-fit-method"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/save-fit-method params)))}]
   ["/data/get-fit-method"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/get-fit-method params)))}]
   ["/data/save-limma-trend"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/save-limma-trend params)))}]
   ["/data/get-limma-trend"
    {:get
     (fn []
       (response/ok (data/get-limma-trend)))}]
   ["/data/save-limma-trend-method"
    {:get
     (fn [{:keys [params]}]
       (response/ok (data/save-limma-trend-method params)))}]
   ["/data/get-limma-trend-method"
    {:get
     (fn []
       (response/ok (data/get-limma-trend-method)))}]
   ["/plots/plot"
    {:get
     (fn [{:keys [params]}]
       (response/ok (plots/plot-raw params)))}]


   ])


