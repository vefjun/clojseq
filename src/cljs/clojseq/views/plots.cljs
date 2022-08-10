(ns clojseq.views.plots
  (:require [re-frame.core :as rf]
            [clojure.string :as string]))

(defn extension [file]
  (let [es (last (clojure.string/split file #"\."))]
    (identity es)))

(defn make-plot-name [plot-type]
  (cond
    (= plot-type "mean-var") "Mean-variance trend"
    (= plot-type "heatmap") "Heatmap"
    (= plot-type "volcano") "Volcano"
    (= plot-type "density") "Density"
    :else plot-type))

(defn card-plot-form []
  (let [plot  @(rf/subscribe [:plots/plot])
        loading? @(rf/subscribe [:plots/loading?])
        ]
    [:div.content
     [:div.column.is-12-tablet.is-8-desktop.is-4-fullhd
      (cond
        (and (empty? plot) (nil? loading?)) [:p ""]
        (= true loading?) [:p "loading ..."]
        :else [:div.card ;;{:key plot}
               [:div.card-content
                [:iframe {:srcdoc plot :height 700 :width 800}]]])
               ]]))


(defn plot [plot-type data-name fdr contrasts filtered]
  (let [interaction @(rf/subscribe [:data/design-interaction])
        colname1 @(rf/subscribe [:data/selected-samples-column-name])
        colname2 @(rf/subscribe [:data/selected-samples-column-name2])
        colname2 (if interaction colname2 nil)]
    (rf/dispatch
      [:plots/plot plot-type data-name fdr contrasts filtered colname1 colname2])))

(defn plot-button []
  (let [plot-type @(rf/subscribe [:plots/selected-plot])
        loading? @(rf/subscribe [:plots/loading?])
        data-name @(rf/subscribe [:data/selected-data-name])
        contrasts @(rf/subscribe [:data/selected-contrasts])
        fdr @(rf/subscribe [:data/selected-fdr])
        filtered @(rf/subscribe [:data/filtered-counts])

        ]
    [:input#boxplot.button.is-primary.is-outlined
     {:type :submit
      :on-click #(plot plot-type data-name fdr contrasts filtered)
      :disabled (or (= plot-type nil)
                    ;(or (= filtered nil) (= filtered false))
                    (= plot-type "Select")
                    (and (or (= plot-type "heatmap")
                             (= plot-type "volcano"))
                         (empty? contrasts))
                    loading?)
      :value "plot"}]))


(defn set-plot [plot-type]
  (do
    (rf/dispatch [:plots/set-selected-plot plot-type])
    ))

(defn plots-form []
  (let [sel-plot @(rf/subscribe [:plots/selected-plot])
        data-name @(rf/subscribe [:data/selected-data-name])
        fdr @(rf/subscribe [:data/selected-fdr])
        fdr (if fdr fdr "")
        contrasts @(rf/subscribe [:data/selected-contrasts])
        filtered @(rf/subscribe [:data/filtered-counts])
        filtered-str (if filtered "filtered data" "raw data")]
    [:nav
     [:div.level
      [:div.level-left
       [:div.level-item
        [:div.field.is-grouped
         [:div.control
          [:div.select.is-small
           [:select
            {:on-change #(set-plot (-> % .-target .-value))}
            [:option {:key "1" :value "Select"} "Select a plot"]
            [:option {:key "2" :value "heatmap"} "Heatmap"]
            [:option {:key "3" :value "volcano"} "Volcano"]
            [:option {:key "5" :value "mean-var"} "Mean-variance"]
            [:option {:key "6" :value "density"} "Density"]
            [:option {:key "7" :value "boxplot"} "Boxplot"]
            [:option {:key "9" :value "mds"} "MDS"]
            [:option {:key "10" :value "sa"} "SA"]
            ]]]]]
       [:div.level-item (plot-button)]
       [:div.level-item sel-plot]
       [:div.level-item filtered-str]
       ]
      [:div.level-right
       [:div.level-item
        [:p.subtitle.is-5 data-name]]
       [:div.level-item
        [:p.subtitle.is-5 (str contrasts)]]
       [:div.level-item
        [:p.subtitle.is-5 test]]
       [:div.level-item
        [:p.subtitle.is-5 fdr]]
       ]]]))

(defn plots-page []
  (let [data-name @(rf/subscribe [:data/selected-data-name])]
    (if (empty? data-name)
      [:section.setion
       [:div.content
        [:h3 "No dataset selected"]]]
      [:section.section
       [plots-form]
       [card-plot-form]
       ])))


