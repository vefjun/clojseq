(ns clojseq.views.settings
  (:require [re-frame.core :as rf]
            [clojure.string :as string]))


(defn clear []
  (do
    (rf/dispatch [:data/clear-selected-contrasts])
    ))


(defn clear-contrasts-button []
  (let [sel-contrasts #(rf/dispatch [:data/selected-contrasts])]
    [:input#boxplot.button.is-primary.is-outlined
     {:type :submit
      :on-click #(clear)
      :value "clear"}]))

(defn add-contrasts [contrast1 contrast2]
  (let [fit @(rf/subscribe [:data/selected-fit])
        contrasts (str contrast1 "-" contrast2)]
    (rf/dispatch [:data/set-selected-contrasts contrasts])))

(defn add-contrasts-button []
  (let [contrast1 @(rf/subscribe [:data/selected-contrast1])
        contrast2 @(rf/subscribe [:data/selected-contrast2])
        fit @(rf/subscribe [:data/selected-fit])]
    [:input#boxplot.button.is-primary.is-outlined
     {:type :submit
      :on-click #(add-contrasts contrast1 contrast2)
      :disabled (or (= nil contrast1)
                    (= nil contrast2)
                    (= contrast1 contrast2))
      :value "add"}]))



(defn set-contrast1 [contrast]
  (if (not= contrast "Select")
    (rf/dispatch [:data/set-selected-contrast1 contrast])))


(defn select-contrast1 [contrasts]
  [:div.level-item
   [:div.field.is-grouped
    [:div.control
     [:div.select.is-small
      [:select
       {:on-change #(set-contrast1 (-> % .-target .-value))}
       [:option {:key "1" :value "Select"} "Select contrast"]
       (for [contrast contrasts]
         [:option {:key contrast :value contrast} contrast])
       ]]]]])

(defn set-contrast2 [contrast]
  (if (not= contrast "Select")
    (rf/dispatch [:data/set-selected-contrast2 contrast])))


(defn select-contrast2 [contrasts]
  [:div.level-item
   [:div.field.is-grouped
    [:div.control
     [:div.select.is-small
      [:select
       {:on-change #(set-contrast1 (-> % .-target .-value))}
       [:option {:key "1" :value "Select"} "Select contrast"]
       (for [contrast contrasts]
         [:option {:key contrast :value contrast} contrast])
       ]]]]])

(defn set-interaction [interaction]
  (do (rf/dispatch [:data/set-design-interaction interaction])
      (rf/dispatch [:data/set-selected-samples-column-name nil])
      (rf/dispatch [:data/set-selected-samples-column-name2 nil])))

(defn check-interaction []
  [:div.field
   [:label.checkbox
    [:input.checkbox
     {:type      :checkbox
      :on-change #(set-interaction (-> % .-target .-checked))
      :name      "interaction"
      :checked   @(rf/subscribe [:data/design-interaction])}]]
   "Interaction?"]
  )


(defn set-selected-samples-column-name [name]
  (do
    (rf/dispatch [:data/set-selected-samples-column-name name])))

(defn select-samples-column-name [cols]
  [:div.level-item
   [:div.field.is-grouped
    [:div.control
     [:div.select.is-small
      [:select
       {:on-change #(set-selected-samples-column-name (-> % .-target .-value))}
       [:option {:key "1" :value "Select"} "Select column"]
       (for [col cols]
         [:option {:key col :value col} col])
       ]]]]])

(defn set-selected-samples-column-name2 [name]
  (do
    (rf/dispatch [:data/set-selected-samples-column-name2 name])))

(defn select-samples-column-name2 [cols]
  [:div.level-item
   [:div.field.is-grouped
    [:div.control
     [:div.select.is-small
      [:select
       {:on-change #(set-selected-samples-column-name2 (-> % .-target .-value))}
       [:option {:key "1" :value "Select"} "Select column"]
       (for [col cols]
         [:option {:key col :value col} col])
       ]]]]])

(defn set-fdr [fdr]
  (do
    (rf/dispatch [:data/set-selected-fdr fdr])))

(defn select-fdr []
  [:div.level-item
   [:div.field.is-grouped
    [:div.control
     [:div.select.is-small
      [:select
       {:on-change #(set-fdr (-> % .-target .-value))}
       [:option {:key "1" :value "Select"} "Select FDR"]
       [:option {:key "2" :value "nil"} "nil"]
       [:option {:key "3" :value "0.3"} "0.3"]
       [:option {:key "4" :value "0.2"} "0.2"]
       [:option {:key "5" :value "0.1"} "0.1"]
       [:option {:key "6" :value "0.05"} "0.05"]
       [:option {:key "7" :value "0.01"} "0.01"]
       [:option {:key "8" :value "0.001"} "0.001"]
       ]]]]])

(defn get-counts [data-name filtered]
  (rf/dispatch [:data/get-selected-counts data-name filtered])
  ;;(rf/dispatch [:data/get-levels data-name filtered])
  (rf/dispatch [:data/get-samples-column-names data-name])
  (rf/dispatch [:data/set-which-table :counts]))

(defn get-samples [data-name filtered]
  (rf/dispatch [:data/get-samples-column-names data-name])
  ;;(rf/dispatch [:data/set-which-table :counts])
  )

(defn set-data-name [data-name]
  (let [filtered @(rf/subscribe [:data/filtered-counts])]
    (do
      (rf/dispatch [:data/set-selected-data-name data-name])
      (rf/dispatch [:data/clear-selected-contrasts])
      (get-samples data-name filtered)
      )))

(defn select-data-name []
  [:div.level-item
   [:div.field.is-grouped
    [:div.control
     [:div.select.is-small
      [:select
       {:on-change  #(set-data-name (-> % .-target .-value))}
       [:option {:key "1" :value "Select"} "Select data"]
       [:option {:key "2" :value "ucdavis"} "UC Davis"]
       ]]]]])


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

(defn set-limma-trend [trend]
  (do
    (rf/dispatch [:limma/set-trend trend])
    (rf/dispatch-sync [:limma/save-limma-trend trend])))

(defn limma-trend []
  [:div.field
   [:label.checkbox
    [:input.checkbox
     {:type      :checkbox
      :on-change #(set-limma-trend (-> % .-target .-checked))
      :name      "trend"
      :checked   @(rf/subscribe [:limma/trend])}]]
   "Limma trend?"]
  )

(defn set-trend-method [method]
  (do
    (rf/dispatch [:limma/set-selected-trend-method method])
    (rf/dispatch-sync [:limma/save-limma-trend-method method])))

(defn select-trend-method []
  [:div.level-item
   [:div.field.is-grouped
    [:div.control
     [:div.select.is-small
      [:select
       {:on-change  #(set-trend-method (-> % .-target .-value))}
       [:option {:key "1" :value "Select"} "Select trend method"]
       [:option {:key "2" :value "ebayes"} "eBayes"]
       ;[:option {:key "3" :value "treat"} "treat"]
       ]]]]])

(defn set-fit [fit]
  (do
    (rf/dispatch [:data/set-selected-fit fit])
    (rf/dispatch-sync [:data/save-fit-method fit])
    (set-trend-method "ebayes")
    (set-limma-trend false)))


(defn select-fit []
  [:div.level-item
   [:div.field.is-grouped
    [:div.control
     [:div.select.is-small
      [:select
       {:on-change  #(set-fit (-> % .-target .-value))}
       [:option {:key "1" :value "Select"} "Select fit method"]
       [:option {:key "2" :value "lmFit"} "lmFit"]
       [:option {:key "3" :value "glmFit"} "glmFit"]
       ]]]]])

(defn limma-data-form []
  (let [fit @(rf/subscribe [:data/selected-fit])
        trend (if @(rf/subscribe [:limma/trend]) "trend" "")
        trend-method @(rf/subscribe [:limma/selected-trend-method])]

    [:div.content
     [:h4 "Limma-data"]
     [:nav.box
      [:div.level
       [:div.level-left
        [:div.level-item (select-fit)]
        [:div.level-item (if (= fit "lmFit") (limma-trend))]
        [:div.level-item (if (= fit "lmFit") (select-trend-method))]
        ]
       [:div.level-right
        [:div.level-item
         [:p.subtitle.is-5 fit]]
        [:div.level-item
         [:p.subtitle.is-5 trend]]
        [:div.level-item
         [:p.subtitle.is-5 trend-method]]
        ]]]]))

(defn apply-design [data-name filtered interaction colname1 colname2]
  (let [colname2 (if interaction colname2 nil)
        fit @(rf/subscribe [:data/selected-fit])
        fdr @(rf/subscribe [:data/selected-fdr])
        ]
    (rf/dispatch [:data/get-levels data-name filtered colname1 colname2])))

(defn apply-design-button []
  (let [data-name @(rf/subscribe [:data/selected-data-name])
        filtered @(rf/subscribe [:data/filtered-counts])
        colname1  @(rf/subscribe [:data/selected-samples-column-name])
        colname2  @(rf/subscribe [:data/selected-samples-column-name2])
        interaction @(rf/subscribe [:data/design-interaction])]
    [:input#loaddata.button.is-primary.is-outlined
     {:type :submit
      :on-click #(apply-design data-name filtered interaction colname1 colname2)
      :disabled (or (= data-name nil)
                    (= colname1 nil)
                    (= colname1 colname2)
                    )
      :value "apply"}
     ]))

(defn make-design-description [interaction sel-colname sel-colname2]
  (let [fit @(rf/subscribe [:data/selected-fit])]
    (cond
       (= interaction true) (str "~0 + interaction(" sel-colname "," sel-colname2 ")" )
       (false? interaction) (str "~0 + " sel-colname)
      :else (str "~0 + " sel-colname)
      )))

(defn card-samples-form []
  (let [sel-colname1  @(rf/subscribe [:data/selected-samples-column-name])
        sel-colname2  @(rf/subscribe [:data/selected-samples-column-name2])
        interaction @(rf/subscribe [:data/design-interaction])
        colnames @(rf/subscribe [:data/samples-column-names])
        fit @(rf/subscribe [:data/selected-fit])

        ]
    [:div.content
     [:h4 "Design"]
     [:nav.box
      (if (not= nil colnames)
        [:div.level
         [:div.level-left
          [:div.level-item  (check-interaction)]
          (select-samples-column-name colnames)
          (if interaction (select-samples-column-name2 colnames))
          [:div.level-item (apply-design-button)]]
         [:div.level-right
          [:div.level-item
           [:p.subtitle.is-5
            (make-design-description interaction sel-colname1 sel-colname2)]]]])
      ]]))


(defn card-levels-form []
  (let [sel-contrasts  @(rf/subscribe [:data/selected-contrasts])
        fit            @(rf/subscribe [:data/selected-fit])
        levels (cljs.reader/read-string @(rf/subscribe [:data/levels]))
        ]
    [:div.content
     [:h4 "Contrasts"]
     [:nav.box
      (if (not= nil levels)
        [:div.level
         [:div.level-left
          [:div.level-item
           [:div.field.is-grouped
            [:div.control
             [:div.select.is-small
              [:select
               {:on-change  #(set-contrast1 (-> % .-target .-value))}
               [:option {:key "1" :value "Select"} "Select contrast"]
               (for [level levels]
                 [:option {:key level :value level} level])
               ]]]]]
          [:div.level-item
           [:div.field.is-grouped
            [:div.control
             [:div.select.is-small
              [:select
               {:on-change  #(set-contrast2 (-> % .-target .-value))}
               [:option {:key "1" :value "Select"} "Select contrast"]
               (for [level levels]
                 [:option {:key level :value level} level])
               ]]]]]
          [:div.level-item (add-contrasts-button)]
          [:div.level-item (clear-contrasts-button)]
          ]
         [:div.level-right
          [:div.level-item  [:p.subtitle.is-5 (str sel-contrasts)]]]]
        )]]))

(defn data-settings-form []
  (let [data-name @(rf/subscribe [:data/selected-data-name])
        contrasts @(rf/subscribe [:data/selected-contrasts])
        fdr @(rf/subscribe [:data/selected-fdr])]
    [:nav.box
     [:div.level
      [:div.level-left
       [:div.level-item (filter-counts)]
       [:div.level-item (select-data-name)]
       [:div.level-item (select-fdr)]
       ]
      [:div.level-right
       [:div.level-item
        [:p.subtitle.is-5 data-name]]
       [:div.level-item
        [:p.subtitle.is-5 fdr]]
       ]]]
    ))

(defn settings-page []
  (let [data-name @(rf/subscribe [:data/selected-data-name])]
    [:section.section
     [data-settings-form]
     [limma-data-form]
     [card-samples-form]
     [card-levels-form]
     ]
    ))