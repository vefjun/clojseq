(ns clojseq.views.data
  (:require [re-frame.core :as rf]
            [clojure.string :as string]))

(defn get-counts [data-name filtered]
  (let  [interaction @(rf/subscribe [:data/design-interaction])
         colname1 @(rf/subscribe [:data/selected-samples-column-name])
         colname2 @(rf/subscribe [:data/selected-samples-column-name2])
         colname2 (if interaction colname2 nil)]
    (rf/dispatch [:data/get-selected-counts data-name filtered colname1 colname2])
    (rf/dispatch [:data/get-levels data-name filtered colname1 colname2])
    (rf/dispatch [:data/set-which-table :counts])))

(defn load-data-button []
  (let [data-name @(rf/subscribe [:data/selected-data-name])
        filtered @(rf/subscribe [:data/filtered-counts])
        loading? @(rf/subscribe [:data/loading?])]
    [:input#loaddata.button.is-primary.is-outlined
     {:type :submit
      :on-click #(get-counts data-name filtered)
      :disabled (or (= data-name nil)
                    loading?)
      :value "counts"}]))

(defn get-dge-samples [data-name filtered]
  (let  [interaction @(rf/subscribe [:data/design-interaction])
         colname1 @(rf/subscribe [:data/selected-samples-column-name])
         colname2 @(rf/subscribe [:data/selected-samples-column-name2])
         colname2 (if interaction colname2 nil)]
    (rf/dispatch [:data/get-samples-from-dge data-name filtered colname1 colname2])
    (rf/dispatch [:data/set-which-table :dge-samples])))

(defn get-design [data-name interaction colname1 colname2]
  (let [colname2 (if interaction colname2 nil)]
    (rf/dispatch [:data/get-design data-name colname1 colname2])
    (rf/dispatch [:data/set-which-table :design])))


(defn get-samples-for-data [data-name]
  (rf/dispatch [:data/get-samples-for-data data-name])
  (rf/dispatch [:data/set-which-table :samples-for-data]))


(defn get-top-table [data-name contrasts filtered fdr]
  (let  [fit @(rf/subscribe [:data/selected-fit])
         interaction @(rf/subscribe [:data/design-interaction])
         colname1 @(rf/subscribe [:data/selected-samples-column-name])
         colname2 @(rf/subscribe [:data/selected-samples-column-name2])
         colname2 (if interaction colname2 nil)]
    (cond
      (= fit "dream")
      (do
        (rf/dispatch [:dream/get-top-table data-name contrasts colname1 colname2 fdr])
        (rf/dispatch [:data/set-which-table :top-table-dream]))
      :else
      (do
        (rf/dispatch [:data/get-top-table data-name contrasts filtered fdr colname1 colname2])
        (rf/dispatch [:data/set-which-table :top-table])))))

(defn make-cols [which-table items]
  (cond
    (= which-table :counts)
    (-> (sort (remove #{:ID} (keys (first items)))) (conj :ID))
    (= which-table :design)
    (-> (sort (remove #{:$row.names} (keys (first items)))))
    :else  (keys (first items))))

(defn data-data []
  (let [loading? @(rf/subscribe [:data/loading?])
        which-table @(rf/subscribe [:data/which-table])
        items (cond
                (= which-table :counts) (cljs.reader/read-string (first @(rf/subscribe [:data/counts])))
                (= which-table :samples-for-data) (cljs.reader/read-string @(rf/subscribe [:data/samples-for-data]))
                (= which-table :dge-samples) (cljs.reader/read-string @(rf/subscribe [:data/dge-samples]))
                (= which-table :design) (cljs.reader/read-string @(rf/subscribe [:data/design]))
                (= which-table :top-table) (cljs.reader/read-string @(rf/subscribe [:data/top-table]))
                )
        genes (cond
                (= which-table :counts) (cljs.reader/read-string (second @(rf/subscribe [:data/counts])))
                :else nil)

        cols (make-cols which-table items)
        id (keyword (first cols))]
    (if loading?
      [:div.content
       [:p ""]
       [:h3 "Loading data ..."]]
      [:div.content
       [:p ""]
       (if (empty? cols) [:h3 "No data loaded"]
                         [:div.content
                          [:h2 which-table]
                          (cond
                            (= which-table :counts) [:h3 (str "Number of genes: " genes)]
                            (= which-table :top-table) [:h3 (str "Number of genes: " (count items))]
                            )])
       [:table {:width "100%"}
        [:thead
         [:tr  (for [col cols] [:td {:key col} col] )]]
        [:tbody
         (for [item items]
           [:tr {:key (cond (= which-table :design) (:$row.names item) :else (str (rand-int 100000) (item id)))}
            (for [col cols]
              [:td {:key (cond (= which-table :design) (str (:$row.names item) col)
                               (= which-table :samples-for-data) (str (rand-int 100000) col)
                               :else (str (rand-int 10000) (item id)  col))} (item  col)]
              )])]]])))

(defn set-view [view-name]
  (let [data-name @(rf/subscribe [:data/selected-data-name])
        filtered @(rf/subscribe [:data/filtered-counts])
        contrasts @(rf/subscribe [:data/selected-contrasts])
        fdr @(rf/subscribe [:data/selected-fdr])
        interaction @(rf/subscribe [:data/design-interaction])
        colname1 @(rf/subscribe [:data/selected-samples-column-name])
        colname2 @(rf/subscribe [:data/selected-samples-column-name2])
        loading? @(rf/subscribe [:data/loading?])]
    (cond
      (= view-name "samples")     (get-samples-for-data data-name)
      (= view-name "counts")      (get-counts data-name filtered)
      (= view-name "dgesamples")  (get-dge-samples data-name filtered)
      (= view-name "design")      (get-design data-name interaction colname1 colname2)
      (= view-name "toptable")      (get-top-table data-name contrasts filtered fdr)
      :else (get-counts data-name filtered)
      )))

(defn select-view []
  (let [contrasts @(rf/subscribe [:data/selected-contrasts])
        fdr @(rf/subscribe [:data/selected-fdr])
        fit @(rf/subscribe [:data/selected-fit])]
    [:div.level-item
     [:div.field.is-grouped
      [:div.control
       [:div.select.is-small
        [:select
         {:on-change  #(set-view (-> % .-target .-value))}
         [:option {:key "1" :value "Select"} "Select view"]
         [:option {:key "2" :value "counts"} "Counts"]
         [:option {:key "3" :value "samples"} "Samples"]
         [:option {:key "4" :value "dgesamples"} "dge$samples"]
         [:option {:key "5" :value "design"} "Design"]
         (if (and (not (empty? contrasts)) (not= fdr nil) (not= fit nil))
           [:option {:key "6" :value "toptable"} "Top table"])
         ]]]]]))


(defn filter-counts []
  [:div.field
   [:label.checkbox
    [:input.checkbox
     {:type      :checkbox
      :on-change #(rf/dispatch
                    [:data/set-filtered-counts
                     (-> % .-target .-checked)])
      :name      "filter"
      :checked   @(rf/subscribe [:data/filtered-counts])}]]
   "Filter?"]
  )

(defn data-form []
  (let [data-name @(rf/subscribe [:data/selected-data-name])
        contrasts @(rf/subscribe [:data/selected-contrasts])
        fdr @(rf/subscribe [:data/selected-fdr])
        fit @(rf/subscribe [:data/selected-fit])]
    [:nav
     [:div.level
      (if (not= data-name nil)
        [:div.level-left
         [:div.level-item (select-view)]
         ])

      [:div.level-right
       [:div.level-item
        [:p.subtitle.is-5 data-name]]
       [:div.level-item
        [:p.subtitle.is-5 (str contrasts)]]
       [:div.level-item
        [:p.subtitle.is-5 fdr]]
       [:div.level-item
        [:p.subtitle.is-5 fit]]
       ]]]
    ))

(defn data-page []
  [:section.section
   [data-form]
   [data-data]]
  )

