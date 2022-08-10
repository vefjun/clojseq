(ns clojseq.plots.plots
      (:require [clojisr.v1.r :as r :refer
                 [r eval-r->java r->java java->r java->clj
                  clj->java r->clj clj->r ->code r+ colon]]
                [clojisr.v1.require :refer [require-r]]
                [clojure.string :as str]
                [clojseq.config :refer [env]]
                [clojseq.utils :as utils]
                [clojseq.storage.voom :as store]
                [clojisr.v1.applications.plotting :refer [plot->svg
                                                          plot->file plot->buffered-image]]
                [clojseq.db.utils :as db-utils]))

  (require-r '[graphics :refer [plot hist]]
             '[ggplot2 :refer [ggplot aes geom_point xlab ylab labs]]
             '[gplots]
             '[dplyr]
             '[edgeR]
             '[RColorBrewer]
             '[limma :refer [volcanoplot coolmap voom plotMDS plotSA]]
             ;;'[NMF :refer [aheatmap]]
             '[base :refer [$]])


  ;coolmap(logcpm[rownames(ss),],cluster.by="expression level",
  ; col=brewer.pal(11,"YlGnBu"),linkage.row="complete",
  ; linkage.col="complete", show.dendrogram="both")
  (def fstress
       (r '(function [samples]
                     ($ samples stress))))

  (def fcond
       (r '(function [samples]
                     ($ samples cond))))
(def fasnumeric
  (r '(function [group]
                (as.numeric group))))

(def fInteraction
  (r '(function [s1 s2]
                (interaction s1 s2))))

  (defn plot-boxplot [dge title]
        ((r "function(dge, title){
       nsamples <- ncol(dge)
       col <- brewer.pal(nsamples, 'Paired')
       lcpm <- cpm(dge, log=TRUE)
       boxplot2(lcpm, las=2, col=col, main='')
       title(main=title, ylab='Log-cpm')
       }") dge title))

  (defn plot-density [dge title]
        ((r "function(dge, title){
       lcpm <- cpm(dge, log=TRUE)
       L <- mean(dge$samples$lib.size) * 1e-6
       M <- median(dge$samples$lib.size) * 1e-6
       lcpm.cutoff <- log2(10/M + 2/L)
       nsamples <- ncol(dge)
       col <- brewer.pal(nsamples, 'Paired')
       plot(density(lcpm[,1]), col=col[1], lwd=2, ylim=c(0,0.40), las=2, main='', xlab='')
         title(main=title, xlab='Log-cpm')
         abline(v=lcpm.cutoff, lty=3)
         for (i in 2:nsamples){
           den <- density(lcpm[,i])
           lines(den$x, den$y, col=col[i], lwd=1)
          }
         legend('topright', colnames(dge), text.col=col, cex=0.8)
       }") dge title))

(defn plotVolcano [{:keys [data-name contrasts filtered colname1 colname2]
                    :or {filtered true}}]
  (let [dge (store/get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
        samples (store/get-samples {:data-name data-name})
        gr (store/get-group samples colname1 colname2)
        mm (utils/model-matrix gr)
        fit (utils/getFitContr dge mm contrasts)
        title1 "Volcano "
        title2 (cond (or (= filtered true) (= filtered "true")) " filtered data"
                     :else " raw data")
        title (str title1 data-name " " (first contrasts) " " title2)]

    (plot->svg
      (fn []
        (volcanoplot fit :main title)))))

(defn plotHeatmap [{:keys [data-name contrasts fdr filtered colname1 colname2]
                    :or {filtered true}}]
  (let [counts (store/get-counts {:data-name data-name
                                    :filtered filtered
                                    :colname1 colname1
                                    :colname2 colname2})
          data (store/filter-counts-by-top {:counts counts
                                            :data-name data-name
                                            :contrasts contrasts
                                            :fdr fdr
                                            :filtered filtered
                                            :colname1 colname1
                                            :colname2 colname2})]
    (plot->svg (fn []
                    (coolmap data
                             :cluster.by "expression level"
                             ;;:col (brewer.pal 11 "YlGnBu")
                             :show.dendrogram "both"
                             :margins [5 12]
                             :srtCol 65
                             :adjCol 1
                             )))))


(defn plotMeanVar [{:keys [data-name filtered colname1 colname2]
                    :or {filtered true}}]
  (let [dge (store/get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
        samples (store/get-samples {:data-name data-name})
        gr (store/get-group samples colname1 colname2)
        mm (utils/model-matrix gr)]
    (plot->svg (fn []
                 (voom dge mm :plot true)))))



(defn plotDensity [{:keys [data-name filtered colname1 colname2]
                    :or {filtered true}}]
  (let [dge (store/get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
        title1 "Density"
        title2 (cond (or (= filtered true) (= filtered "true")) " filtered data"
                     :else " raw data")
        title (str title1 title2)]
    (plot->svg (fn []
                 (plot-density dge title)))))

(defn plotBoxplot [{:keys [data-name filtered colname1 colname2]
                    :or {filtered true}}]
  (let [dge (store/get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
        title1 "Boxplot"
        title2 (cond (or (= filtered true) (= filtered "true")) " filtered data"
                     :else " raw data")
        title (str title1 title2)]
    (plot->svg (fn []
                 (plot-boxplot dge title)))))

(defn plotMds [{:keys [data-name filtered colname1 colname2]
                    :or {filtered true}}]
  (let [dge (store/get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
        samples (store/get-samples {:data-name data-name})
        gr (store/get-group samples colname1 colname2)
        title1 "MDS"
        title2 (cond (or (= filtered true) (= filtered "true")) " filtered data"
                     :else " raw data")
        title (str title1 title2)]
    (plot->svg (fn []
                 (plotMDS dge
                          :col (fasnumeric gr) :main title)))))

(defn plotSa [{:keys [data-name filtered colname1 colname2]
                    :or {filtered true}}]
  (let [dge (store/get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
        samples (store/get-samples {:data-name data-name})
        fit-method (db-utils/find-current-fit-method)
        fit (store/make-fit-dge dge samples colname1 colname2 fit-method)
        title1 "SA"
        title2 (cond (or (= filtered true) (= filtered "true")) " filtered data"
                     :else " raw data")
        title (str title1 title2)]

    (plot->svg
      (fn []
        (plotSA fit :main title)))))

  (defn create-plot [{:keys [plot-type data-name contrasts fdr filtered colname1 colname2]
                      :or {filtered true}}]
          (cond
            (= plot-type "volcano") (plotVolcano {:data-name data-name
                                                  :contrasts contrasts
                                                  :fdr fdr
                                                  :filtered filtered
                                                  :colname1 colname1
                                                  :colname2 colname2})
            (= plot-type "heatmap") (plotHeatmap {:data-name data-name
                                                  :contrasts contrasts
                                                  :fdr fdr
                                                  :filtered filtered
                                                  :colname1 colname1
                                                  :colname2 colname2})
            (= plot-type "mean-var") (plotMeanVar {:data-name data-name
                                                  :filtered filtered
                                                  :colname1 colname1
                                                  :colname2 colname2})

            (= plot-type "density") (plotDensity {:data-name data-name
                                                   :filtered filtered
                                                   :colname1 colname1
                                                   :colname2 colname2})
            (= plot-type "boxplot") (plotBoxplot {:data-name data-name
                                                  :filtered filtered
                                                  :colname1 colname1
                                                  :colname2 colname2})
            (= plot-type "mds") (plotMds {:data-name data-name
                                                  :filtered filtered
                                                  :colname1 colname1
                                                  :colname2 colname2})
            (= plot-type "sa") (plotSa {:data-name data-name
                                          :filtered filtered
                                          :colname1 colname1
                                          :colname2 colname2})


            ))


  (defn plot-raw [{:keys [plot-type data-name contrasts fdr filtered colname1 colname2]
                   :or {data-name nil contrasts nil fdr nil filtered nil}}]
        ""
        (let [contrasts (read-string contrasts)
              colname2 (if (= colname2 "null") nil colname2)
              fdr (read-string fdr)
              filtered (read-string filtered)
              plot  (create-plot {:plot-type plot-type
                                   :data-name data-name
                                   :contrasts contrasts
                                   :fdr fdr
                                   :filtered filtered
                                   :colname1 colname1
                                   :colname2 colname2})]

          (do
            (println "plots" plot-type)
            (identity plot))))



