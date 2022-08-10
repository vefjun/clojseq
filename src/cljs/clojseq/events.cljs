(ns clojseq.events
  (:require
    [re-frame.core :as rf]
    [ajax.core :as ajax]
    [reitit.frontend.easy :as rfe]
    [reitit.frontend.controllers :as rfc]))

;;dispatchers

(rf/reg-event-db
  :common/navigate
  (fn [db [_ match]]
    (let [old-match (:common/route db)
          new-match (assoc match :controllers
                                 (rfc/apply-controllers (:controllers old-match) match))]
      (assoc db :common/route new-match))))

(rf/reg-fx
  :common/navigate-fx!
  (fn [[k & [params query]]]
    (rfe/push-state k params query)))

(rf/reg-event-fx
  :common/navigate!
  (fn [_ [_ url-key params query]]
    {:common/navigate-fx! [url-key params query]}))

(rf/reg-event-db
  :set-docs
  (fn [db [_ docs]]
    (assoc db :docs docs)))

(rf/reg-event-fx
  :fetch-docs
  (fn [_ _]
    {:http-xhrio {:method          :get
                  :uri             "/docs"
                  :response-format (ajax/raw-response-format)
                  :on-success       [:set-docs]}}))

(rf/reg-event-db
  :common/set-error
  (fn [db [_ error]]
    (assoc db :common/error error)))

(rf/reg-event-fx
  :page/init-home
  (fn [_ _]
    {:dispatch [:fetch-docs]}))

;;subscriptions

(rf/reg-sub
  :common/route
  (fn [db _]
    (-> db :common/route)))

(rf/reg-sub
  :common/page-id
  :<- [:common/route]
  (fn [route _]
    (-> route :data :name)))

(rf/reg-sub
  :common/page
  :<- [:common/route]
  (fn [route _]
    (-> route :data :view)))

(rf/reg-sub
  :docs
  (fn [db _]
    (:docs db)))

(rf/reg-sub
  :common/error
  (fn [db _]
    (:common/error db)))

;;;;;;;;;;;;;;;;;;;;;;;; data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rf/reg-event-fx
  :data/get-selected-counts
  (fn [{:keys [db]} [_ data-name filtered colname1 colname2]]
    (ajax/GET "/api/data/get-selected-counts"
              {:headers {"Accept" "application/transit+json"}
               :params {:data-name data-name
                        :filtered filtered
                        :colname1 colname1
                        :colname2 colname2
                        }
               :handler #(rf/dispatch [:data/set-counts  %])})
    {:db (assoc db :data/loading? true )}))

(rf/reg-event-fx
  :data/get-samples-from-dge
  (fn [{:keys [db]} [_ data-name filtered colname1 colname2]]
    (ajax/GET "/api/data/get-samples-from-dge"
              {:headers {"Accept" "application/transit+json"}
               :params {:data-name data-name
                        :filtered filtered
                        :colname1 colname1
                        :colname2 colname2
                        }
               :handler #(rf/dispatch [:data/set-dge-samples  %])})
    {:db (assoc db :data/loading? true )}))

(rf/reg-event-fx
  :data/get-samples-for-data
  (fn [{:keys [db]} [_ data-name filtered]]
    (ajax/GET "/api/data/get-samples-for-data"
              {:headers {"Accept" "application/transit+json"}
               :params {:data-name data-name
                        }
               :handler #(rf/dispatch [:data/set-samples-for-data  %])})
    {:db (assoc db :data/loading? true )}))

(rf/reg-event-fx
  :data/get-design
  (fn [{:keys [db]} [_ data-name colname1 colname2]]
    (ajax/GET "/api/data/get-design"
              {:headers {"Accept" "application/transit+json"}
               :params {:data-name data-name
                        :colname1 colname1
                        :colname2 colname2
                        }
               :handler #(rf/dispatch [:data/set-design  %])})
    {:db (assoc db :data/loading? true )}))

(rf/reg-event-fx
  :data/get-samples-column-names
  (fn [{:keys [db]} [_ data-name]]
    (ajax/GET "/api/data/get-samples-column-names"
              {:headers {"Accept" "application/transit+json"}
               :params {:data-name data-name}
               :handler #(rf/dispatch [:data/set-samples-column-names  %])})))

(rf/reg-event-db
  :data/set-filtered-counts
  (fn [db [_ filtered]]
    (-> db
        (assoc :data/filtered-counts filtered))
    ))

(rf/reg-sub
  :data/filtered-counts
  (fn [db _]
    (-> db :data/filtered-counts )))



(rf/reg-event-db
  :data/set-samples-column-names
  (fn [db [_ names]]
    (-> db
        (assoc :data/samples-column-names names))))

(rf/reg-sub
  :data/samples-column-names
  (fn [db _]
    (-> db :data/samples-column-names )))

(rf/reg-event-db
  :data/set-selected-samples-column-name
  (fn [db [_ name]]
    (-> db
        (assoc :data/selected-samples-column-name name))))

(rf/reg-sub
  :data/selected-samples-column-name
  (fn [db _]
    (-> db :data/selected-samples-column-name)))

(rf/reg-event-db
  :data/set-selected-samples-column-name2
  (fn [db [_ name]]
    (-> db
        (assoc :data/selected-samples-column-name2 name))))

(rf/reg-sub
  :data/selected-samples-column-name2
  (fn [db _]
    (-> db :data/selected-samples-column-name2)))


(rf/reg-event-db
  :data/set-design-interaction
  (fn [db [_ interaction]]
    (-> db
        (assoc :data/design-interaction interaction))))

(rf/reg-sub
  :data/design-interaction
  (fn [db _]
    (-> db :data/design-interaction)))

(rf/reg-event-db
  :data/set-selected-data-name
  (fn [db [_ data-name]]
    (-> db
        (assoc :data/selected-data-name data-name))))

(rf/reg-sub
  :data/selected-data-name
  (fn [db _]
    (-> db :data/selected-data-name )))

(rf/reg-event-db
  :data/set-counts
  (fn [db [_ data]]
    (-> db
        (assoc
          :data/loading? false
          :data/counts data))
    ))

(rf/reg-sub
  :data/counts
  (fn [db -]
    (-> db :data/counts)))

(rf/reg-event-db
  :data/set-dge-samples
  (fn [db [_ data]]
    (-> db
        (assoc
          :data/loading? false
          :data/dge-samples data))
    ))

(rf/reg-sub
  :data/dge-samples
  (fn [db -]
    (-> db :data/dge-samples)))

(rf/reg-event-db
  :data/set-samples-for-data
  (fn [db [_ data]]
    (-> db
        (assoc
          :data/loading? false
          :data/samples-for-data data))
    ))

(rf/reg-sub
  :data/samples-for-data
  (fn [db -]
    (-> db :data/samples-for-data)))

(rf/reg-event-db
  :data/set-design
  (fn [db [_ data]]
    (-> db
        (assoc
          :data/loading? false
          :data/design data))
    ))

(rf/reg-sub
  :data/design
  (fn [db -]
    (-> db :data/design)))

(rf/reg-sub
  :data/loading?
  (fn [db -]
    (-> db :data/loading?)))



(rf/reg-event-fx
  :data/get-levels
  (fn [{:keys [db]} [_ data-name filtered colname1 colname2]]
    (ajax/GET "/api/data/get-levels"
              {:headers {"Accept" "application/transit+json"}
               :params {:data-name data-name
                        :filtered filtered
                        :colname1 colname1
                        :colname2 colname2
                        }
               :handler #(rf/dispatch [:data/set-levels  %])})
    {:db (assoc db :data/loading? true )}))

(rf/reg-event-db
  :data/set-levels
  (fn [db [_ levels]]
    (-> db
        (assoc
          :data/loading? false
          :data/levels levels))
    ))

(rf/reg-sub
  :data/levels
  (fn [db -]
    (-> db :data/levels)))

(rf/reg-event-fx
  :data/get-top-table
  (fn [{:keys [db]} [_ data-name contrasts filtered fdr colname1 colname2]]
    (ajax/GET "/api/data/get-top-table"
              {:headers {"Accept" "application/transit+json"}
               :params {:data-name data-name
                        :contrasts (pr-str contrasts)
                        :filtered filtered
                        :fdr fdr
                        :colname1 colname1
                        :colname2 colname2
                        }
               :handler #(rf/dispatch [:data/set-top-table  %])})
    {:db (assoc db :data/loading? true )}))

(rf/reg-event-db
  :data/set-top-table
  (fn [db [_ top-table]]
    (-> db
        (assoc
          :data/loading? false
          :data/top-table top-table))
    ))

(rf/reg-sub
  :data/top-table
  (fn [db -]
    (-> db :data/top-table)))

(rf/reg-event-db
  :data/set-which-table
  (fn [db [_ which-table]]
    (-> db
        (assoc
          :data/which-table which-table))
    ))

(rf/reg-sub
  :data/which-table
  (fn [db -]
    (-> db :data/which-table)))

(rf/reg-event-db
  :data/set-selected-contrast1
  (fn [db [_ contrast]]
    (-> db
        (assoc
          :data/selected-contrast1 contrast))
    ))

(rf/reg-sub
  :data/selected-contrast1
  (fn [db -]
    (-> db :data/selected-contrast1)))

(rf/reg-event-db
  :data/set-selected-contrast2
  (fn [db [_ contrast]]
    (-> db
        (assoc
          :data/selected-contrast2 contrast))
    ))

(rf/reg-sub
  :data/selected-contrast2
  (fn [db -]
    (-> db :data/selected-contrast2)))

(rf/reg-event-db
  :data/set-selected-contrasts
  [(rf/path :data/selected-contrasts)]
  (fn [contrasts [_ value]]
    (conj contrasts value)))

(rf/reg-sub
  :data/selected-contrasts
  (fn [db -]
    (-> db :data/selected-contrasts)))

(rf/reg-event-db
  :data/clear-selected-contrasts
  [(rf/path :data/selected-contrasts)]
  (fn [_ _]
    []))

(rf/reg-event-db
  :data/set-selected-fdr
  (fn [db [_ fdr]]
    (-> db
        (assoc :data/selected-fdr fdr))
    ))

(rf/reg-event-db
  :data/set-selected-fdr
  (fn [db [_ fdr]]
    (-> db
        (assoc :data/selected-fdr fdr))
    ))
(rf/reg-sub
  :data/selected-fdr
  (fn [db _]
    (-> db :data/selected-fdr )))

;;;;;;;;;;;;;;;;;;;;; limma ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rf/reg-event-fx
  :data/get-fit-method
  (fn [{:keys [db]} [_]]
    (ajax/GET "/api/data/get-fit-method"
              {:headers {"Accept" "application/transit+json"}
               :handler #(rf/dispatch [:data/set-selected-fit  %])
               })))


(rf/reg-event-fx
  :data/save-fit-method
  (fn [{:keys [db]} [_ fit]]
    (ajax/GET "/api/data/save-fit-method"
              {:headers {"Accept" "application/transit+json"}
               :params {:fit-method fit}
               :handler nil
               })))

(rf/reg-event-db
  :data/set-selected-fit
  (fn [db [_ fit]]
    (-> db
        (assoc :data/selected-fit fit))))

(rf/reg-sub
  :data/selected-fit
  (fn [db _]
    (-> db :data/selected-fit )))

(rf/reg-event-fx
  :limma/get-limma-trend
  (fn [{:keys [db]} [_]]
    (ajax/GET "/api/data/get-limma-trend"
              {:headers {"Accept" "application/transit+json"}
               :handler #(rf/dispatch [:limma/set-trend (cljs.reader/read-string %)])
               })))


(rf/reg-event-fx
  :limma/save-limma-trend
  (fn [{:keys [db]} [_ trend]]
    (ajax/GET "/api/data/save-limma-trend"
              {:headers {"Accept" "application/transit+json"}
               :params {:trend trend}
               ;;:handler #(rf/dispatch [:limma/get-limma-trend])
               })))

(rf/reg-event-db
  :limma/set-trend
  (fn [db [_ trend]]
    (-> db
        (assoc :limma/trend trend))
    ))

(rf/reg-sub
  :limma/trend
  (fn [db _]
    (-> db :limma/trend )))

(rf/reg-event-fx
  :limma/get-limma-trend-method
  (fn [{:keys [db]} [_]]
    (ajax/GET "/api/data/get-limma-trend-method"
              {:headers {"Accept" "application/transit+json"}
               :params {}
               :handler #(rf/dispatch [:limma/set-selected-trend-method  %])
               })))


(rf/reg-event-fx
  :limma/save-limma-trend-method
  (fn [{:keys [db]} [_ trend-method]]
    (ajax/GET "/api/data/save-limma-trend-method"
              {:headers {"Accept" "application/transit+json"}
               :params {:trend-method trend-method}
               ;;:handler #(rf/dispatch [:limma/get-limma-trend-method])
               })))

(rf/reg-event-db
  :limma/set-selected-trend-method
  (fn [db [_ trend-method]]
    (-> db
        (assoc :limma/selected-trend-method trend-method))))

(rf/reg-sub
  :limma/selected-trend-method
  (fn [db _]
    (-> db :limma/selected-trend-method )))


;;;;;;;;;;;;;;;;;;;; voom plots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rf/reg-event-fx
  :plots/plot
  (fn [{:keys [db]} [_ plot-type data-name fdr contrasts filtered colname1 colname2]]
    (ajax/GET "/api/plots/plot"
              {:headers {"Accept" "application/transit+json"}
               :params {:plot-type plot-type
                        :data-name data-name
                        :fdr fdr
                        :contrasts (pr-str contrasts)
                        :filtered filtered
                        :colname1 colname1
                        :colname2 colname2
                        }
               :handler #(rf/dispatch [:plots/set-plot  %])})
    {:db (assoc db :plots/loading? true )}))

(rf/reg-event-db
  :plots/set-plot
  (fn [db [_ data]]
    (-> db
        (assoc :plots/loading? false
               :plots/plot data
               ))))

(rf/reg-event-db
  :plots/set-selected-plot
  (fn [db [_ plot-type]]
    (-> db
        (assoc :plots/selected-plot plot-type))
    ))

(rf/reg-sub
  :plots/loading?
  (fn [db _]
    (-> db :plots/loading? )))

(rf/reg-sub
  :plots/plot
  (fn [db _]
    (-> db :plots/plot )))

(rf/reg-sub
  :plots/selected-plot
  (fn [db _]
    (-> db :plots/selected-plot )))

