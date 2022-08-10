(ns clojseq.storage.voom
      (:require [tech.v3.dataset :as ds]
                [clojure.string :as str]
                [clojisr.v1.require :refer [require-r]]
                [clojisr.v1.r :as r :refer
                 [r r->clj clj->r ->code r+ colon r->java]]
                [clojseq.utils :as utils]
                [clojseq.db.utils :as db-utils]
                [clojseq.config :refer [env]]
                ))

  (require-r '[limma]
             '[dplyr]
             '[data.table]
             '[conflicted]
             '[base :refer [$]])

  (declare filter-logcpm-by-top)

  (def fasfactor
    (r '(function [group]
                (as.factor group))))

  (def fasnumeric
    (r '(function [group]
                (as.numeric group))))

  (def fInteraction
    (r '(function [s1 s2]
                (interaction s1 s2))))

  (def freadRDS
       (r '(<- freadRDS
               (function [objectname]
                         (readRDS :file objectname)))))

  (def freadCounts
       (r '(<- freadCounts
               (function [objectname]
                         (<- data (readRDS :file objectname))
                         (<- counts ($ data counts))))))

  (defn read-rds [{:keys [data-name]}]
        (let [data (freadRDS data-name)]
          (->>
            data
            r->clj)))

  (defn read-object [data-name]
        (let [voom-dir "resources/voom/"]
          (println "read-object" data-name)
          (freadRDS (str voom-dir data-name ".rds"))))

  (defn read-counts [data-name]
        (let [voom-dir "resources/voom/"]
          (freadCounts (str voom-dir data-name "_data.rds"))))


  (def freadLevels
       (r '(<- freadLevels
               (function [fit]
                         (colnames (coef fit))))))

  (defn get-voom-data [{:keys [data-name]}]
        (let [d-object (read-object (str data-name "_data"))]
          (identity d-object)))

  (defn get-voom-counts-raw [{:keys [data-name]}]
        (let [d-object (read-object (str data-name "_counts_raw"))]
          (identity d-object)))

  (defn get-samples [{:keys [data-name]}]
        (let [d-object (read-object (str data-name "_samples"))]
          (identity d-object)))

  (defn get-samples-column-names [{:keys [data-name]}]
        (let [samples (get-samples {:data-name data-name})]
          (vec (map #(name %) (ds/column-names (r->clj samples))))))


  (defn get-group [samples colname1 colname2]
    (cond
    (and (not= nil colname1) (not= nil colname2))
    (fInteraction ($ samples (name colname1)) ($ samples (name colname2)))
    :else (fasfactor ($ samples (name colname1)))
    ))


  (defn get-voom-dge [{:keys [data-name filtered colname1 colname2]}]
        (let [filtered (if (or (= filtered "true") (= filtered true)) true false)
              counts-raw (get-voom-counts-raw {:data-name data-name})
              samples (get-samples {:data-name data-name})
              group (get-group samples colname1 colname2)
              dge (utils/fDGEList (utils/make-rownames-data counts-raw) group)]
          (if filtered
            (utils/fFilterAndNormalizeDge dge group)
            dge)))


  (defn get-counts [{:keys [data-name filtered colname1 colname2]
                     :or {data-name nil filtered nil}}]
        (let [filtered (if (or (= filtered "true") (= filtered true)) true false)
              dge (get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})]
          ($ dge 'counts)))

  (defn get-dge-samples [{:keys [data-name filtered colname1 colname2]
                          :or {data-name nil filtered nil}}]
        (let [filtered (if (or (= filtered "true") (= filtered true)) true false)
              dge (get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})]
          ($ dge 'samples)))

  (defn get-lmfit [dge group]
        (let [trend (db-utils/find-limma-trend)
              trend-method (db-utils/find-limma-trend-method)]
          (cond
            (and (true? trend) (= trend-method "ebayes")) (utils/get-limma-trend-ebayes dge group)
            (and (true? trend) (= trend-method "treat")) (utils/get-limma-trend-treat dge group)
            (and (false? trend) (= trend-method "ebayes")) (utils/get-voom-ebayes dge group)
            (and (false? trend) (= trend-method "treat")) (utils/get-voom-treat dge group)
            :else "no match"
            )))

  (defn make-fit-dge [dge samples colname1 colname2 fit-method]
        (let [group (get-group samples colname1 colname2)]
          (cond
            (= fit-method "lmFit") (get-lmfit dge group)
            (= fit-method "glmFit") (utils/get-glmfit dge group)
            :else nil
            )))

  (defn get-selected-counts [{:keys [data-name filtered colname1 colname2]
                              :or {data-name nil filtered nil}}]
        (let [colname2 (if (= colname2 "null") nil colname2)
              counts (get-counts {:data-name data-name
                                  :filtered filtered
                                  :colname1 colname1
                                  :colname2 colname2})
              clj-counts (r->clj counts)
              cols (vec (take 6 (ds/column-names clj-counts)))
              cs (ds/select-columns clj-counts cols)
              cs (clojure.set/rename-keys cs {:$row.names :ID})]
          [(pr-str (take 20 (ds/mapseq-reader cs))) (pr-str (count (:$row.names clj-counts)))]))

  (defn get-samples-from-dge [{:keys [data-name filtered colname1 colname2]
                               :or {data-name nil filtered nil}}]
        (let [colname2 (if (= colname2 "null") nil colname2)
              samples (get-dge-samples {:data-name data-name
                                        :filtered  filtered
                                        :colname1 colname1
                                        :colname2 colname2})
              clj-samples (r->clj samples)
              cols (vec (take 6 (ds/column-names clj-samples)))
              cs (ds/select-columns clj-samples cols)
              cs (clojure.set/rename-keys cs {:$row.names :sample})]
          (pr-str (ds/mapseq-reader cs))))

  (defn get-samples-for-data [{:keys [data-name]}]
        (let [samples (get-samples {:data-name data-name})
              clj-samples (r->clj samples)]
          (pr-str (ds/mapseq-reader clj-samples))))

  (defn select-scaled-counts [data-name contrasts filtered colname1 colname2]
        (let [dge (get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
              scaled-data (r->clj (utils/scale-dge dge))
              cols (ds/column-names scaled-data)
              cols (utils/contrasts2colnames cols (first contrasts))]
          (ds/select-columns scaled-data cols)))

  (defn get-levels [{:keys [data-name filtered colname1 colname2]}]
        (let [colname2 (if (= colname2 "null") nil colname2)
              filtered (if (or (= filtered "true") (= filtered true)) true false)
              dge (get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
              samples (get-samples {:data-name data-name})
              fit-method (db-utils/find-current-fit-method)
              fit (make-fit-dge dge samples colname1 colname2 fit-method)
              levels (freadLevels fit)]
          (pr-str (r->clj levels))))

  (defn samples-column [samples colname]
        ((r (str "function(samples){
        return(samples$" colname ")}")) samples))


(defn get-design [{:keys [data-name colname1 colname2] :or {colname2 nil}}]
  (let [colname2 (if (= colname2 "null") nil colname2)
        samples (get-samples {:data-name data-name})
        design (utils/get-design-matrix {:samples samples :colname1 colname1 :colname2 colname2})]
    (pr-str (ds/mapseq-reader (r->clj design)))))

  ;;;;;;;;;;;;;;;;;;;;; contrasts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;makeContrasts(groupHS.0h - groupLS.0h, levels = colnames(coef(fit)))

  (def fmakeContrasts
       (r '(<- fmakeContrasts
               (function [fit contrasts]
                         (do
                           (<- colnames (colnames (coef fit)))
                           (<- contr (makeContrasts :contrasts contrasts :levels colnames))
                           contr)
                         ))))

  (def festContrasts
       (r '(<- festContrasts
               (function [fit contr]
                         (contrasts.fit fit contr)))))

  (def feBayes
       (r '(<- feBayes
               (function [tmp trend]
                         (eBayes tmp :trend trend)))))

  (def fTreat
       (r '(<- fTreat
               (function [tmp trend]
                         (treat tmp :trend trend)))))

  (def fallGenes
       (r '(<- fallGenes
               (function [tmp]
                         (do
                           (conflict_prefer "topTable" "limma")
                           (<- genes (topTable tmp :sort.by "P" :n Inf)))))))

  (def ftopTable1
       (r '(<- ftopTable1
               (function [tmp]
                         (do
                           (conflict_prefer "topTable" "limma")
                           (<- top (topTable tmp :n Inf))
                           )))))

  (def ftopTable2
       (r '(<- ftopTable2
               (function [tmp fdr]
                         (do
                           (conflict_prefer "topTable" "limma")
                           (<- top (topTable tmp :n Inf))
                           (<- l (length (which (< ($ top adj.P.Val)  fdr))))
                           (head top l))))))

  (def ftopTreat1
       (r '(<- ftopTreat1
               (function [tmp]
                         (do
                           (<- top (topTreat tmp :n Inf))
                           )))))

  (def ftopTreat2
       (r '(<- ftopTreat2
               (function [tmp fdr]
                         (do
                           (<- top (topTreat tmp :n Inf))
                           (<- l (length (which (< ($ top adj.P.Val)  fdr))))
                           (head top l))))))




  ;;(defn make-contrasts [{:keys [data-name contrasts filtered colname1 colname2]}]
  ;;  "contrasts is a vector: f.ex. ['LL.4C - LL.2h', 'LL.4h - LL.2h']"
  ;;  (let [counts (get-counts {:data-name data-name :filtered filtered})
  ;;        samples (get-samples {:data-name data-name})
  ;;        fit-method (db-utils/find-current-fit-method)
  ;;        fit (utils/make-fit counts samples colname1 colname2 fit-method)
  ;;        ]
  ;;    (fmakeContrasts fit contrasts)))

  (defn process-gene-list-lmfit [glist]
        (let [gl (for [g glist] {:ID (:$row.names g )
                                 ;;:gene (first
                                 ;;        (get (:attributes
                                 ;;               (first (utils/find-cds-feature (:$row.names g)))) "locus_tag"))
                                 ;;:product
                                 ;;(first
                                 ;;  (get (:attributes
                                 ;;         (first (utils/find-cds-feature (:$row.names g)))) "product"))
                                 :logFC (if (not= nil (:logFC g)) (.floatValue (:logFC g)) (:logFC g))
                                 :AveExpr (if (not= nil (:AveExpr g)) (.floatValue (:AveExpr g)))
                                 :P.Value (.floatValue (:P.Value g))
                                 :adj.P.Val (.floatValue (:adj.P.Val g))
                                 :B (if (not= nil (:B g)) (.floatValue (:B g)))})]
          (pr-str gl)))

  (defn process-gene-list-glmfit [glist]
        (let [gl (for [g glist] {:ID (:$row.names g )
                                 ;;:gene (first
                                 ;;        (get (:attributes
                                 ;;               (first (utils/find-cds-feature (:$row.names g)))) "locus_tag"))
                                 ;;:product
                                 ;;(first
                                 ;;  (get (:attributes
                                 ;;         (first (utils/find-cds-feature (:$row.names g)))) "product"))
                                 :logFC (if (not= nil (:logFC g)) (.floatValue (:logFC g)) (:logFC g))
                                 :PValue (.floatValue (:PValue g))
                                 :FDR (.floatValue (:FDR g))
                                 :LR (.floatValue (:LR g))
                                 :logCPM (.floatValue (:logCPM g))
                                 })]
          (pr-str gl)))

  (defn process-gene-list2 [glist contrasts]
        (let [contr1 (first contrasts)
              contr2 (second contrasts)
              keyw1 (keyword (clojure.string/replace contr1 #"-" "."))
              keyw2 (keyword (clojure.string/replace contr2 #"-" "."))
              gl (for [g glist] {:ID (:$row.names g )
                                 ;;:product
                                 ;;(first
                                 ;;  (get (:attributes
                                 ;;         (first (utils/find-cds-feature (:$row.names g)))) "product"))
                                 :F (if (not= nil (:F g)) (.floatValue (:F g)) (:F g))
                                 keyw1 (.floatValue (get g keyw1))
                                 keyw2 (.floatValue (keyw2 g))
                                 :AveExpr (.floatValue (:AveExpr g))
                                 :P.Value (.floatValue (:P.Value g))
                                 :adj.P.Val (.floatValue (:adj.P.Val g))})]
          (pr-str gl)))

;;(defn make-locus-tag-list [glist]
;;        (let [gl (for [g glist] (first
;;                                 (get (:attributes
;;                                        (first (utils/find-cds-feature (:$row.names g)))) "locus_tag"))
;;                               )]
;;         (pr-str gl)))

  (defn make-tmp [fit contrasts trend trend-method]
        (let [contr (fmakeContrasts fit contrasts)
              tmp (festContrasts fit contr)
              tmp
              (cond
                (= "ebayes" trend-method) (feBayes tmp trend)
                (= "treat" trend-method) (fTreat tmp trend))
              ]
          (identity tmp)))

  (def ftopTags
       (r '(<- ftopTags
               (function [fit contrast fdr]
                         (do
                           (<- fitc (glmLRT fit :contrast contrast))
                           (<- top (topTags fitc
                                            :n Inf
                                            :adjust.method "BH"
                                            :sort.by "PValue"
                                            :p.value fdr))
                           (<- table ($ top table))
                           (<- l (length (which (< ($ table FDR)  fdr))))
                           (head top l)
                           )))))

  (defn toptags-glmfit [fit contrasts fdr]
        (let [contr (fmakeContrasts fit contrasts)
              fdr (if (or (= "null" fdr) (= "nil" fdr) (= nil fdr)) 5.0 fdr)
              top-tags ($ (ftopTags fit contr fdr) 'table)]
          (if (= nil (r->clj top-tags)) nil (identity top-tags))))

  (defn toptable-lmfit [fit contrasts fdr]
        (let [trend (db-utils/find-limma-trend)
              trendm (db-utils/find-limma-trend-method)
              tmp (make-tmp fit contrasts trend trendm)
              top-table
              (cond
                (and (nil? fdr) (= trendm "ebayes")) (ftopTable1 tmp)
                (and (not (nil? fdr)) (= trendm "ebayes")) (ftopTable2 tmp fdr)
                (and (nil? fdr) (= trendm "treat")) (ftopTreat1 tmp)
                (and (not (nil? fdr)) (= trendm "treat")) (ftopTreat2 tmp fdr))
              ]
          (if (= nil (r->clj top-table)) nil (identity top-table))))

  (defn get-top [{:keys [data-name contrasts fdr filtered colname1 colname2 fit-method]
                  :or {fdr nil filtered true fit-method nil}}]
        "contrasts is a vector: f.ex. ['LL.4h - LL.2h', 'LL.6h - LL.4h']"
        (let [filtered (if (or (= filtered "true") (= filtered true)) true false)
              dge (get-voom-dge {:data-name data-name
                                 :filtered filtered
                                 :colname1 colname1
                                 :colname2 colname2})
              samples (get-samples {:data-name data-name})
              fitm (if (not= nil fit-method) fit-method (db-utils/find-current-fit-method))
              fit (make-fit-dge dge samples colname1 colname2 fitm)
              top (cond
                    (= "lmFit" fitm) (toptable-lmfit fit contrasts fdr)
                    (= "glmFit" fitm) (toptags-glmfit fit contrasts fdr))]

          (identity top)))

  (defn get-top-names [{:keys [data-name contrasts fdr filtered colname1 colname2]}]
        (let [top (get-top {:data-name data-name
                            :contrasts contrasts
                            :fdr fdr
                            :filtered filtered
                            :colname1 colname1
                            :colname2 colname2})]
          (utils/row-names top)))

  (defn filter-counts-by-top [{:keys [counts data-name contrasts fdr filtered colname1 colname2]
                               :or {contrasts nil fdr nil}}]
        (let [names (get-top-names {:data-name data-name
                                    :contrasts contrasts
                                    :fdr fdr
                                    :filtered filtered
                                    :colname1 colname1
                                    :colname2 colname2})]
          (utils/filter-data-by-names counts names)))

  (defn get-top-table [{:keys [data-name contrasts fdr filtered colname1 colname2]
                        :or {fdr nil filtered nil}}]
        (let [colname2 (if (= colname2 "null") nil colname2)
              contrasts (read-string contrasts)
              fdr (read-string fdr)
              filtered (read-string filtered)
              fit-method (db-utils/find-current-fit-method)
              top-table (get-top {:data-name data-name
                                  :contrasts contrasts
                                  :fdr fdr
                                  :filtered filtered
                                  :colname1 colname1
                                  :colname2 colname2})
              ds-top (if (not= nil top-table) (ds/mapseq-reader (r->clj top-table)) [])
              pr-top (cond (> (count contrasts) 1) (process-gene-list2 ds-top contrasts)
                           (= fit-method "lmFit") (process-gene-list-lmfit ds-top)
                           (= fit-method "glmFit") (process-gene-list-glmfit ds-top))]
          (do ;;(println pr-top)
            (identity pr-top))))

;;(defn get-top-genes [{:keys [data-name contrasts fdr filtered colname1 colname2]
;;                        :or {fdr nil filtered true}}]
;;        (let [top-table (get-top {:data-name data-name
;;                                  :contrasts contrasts
;;                                  :fdr fdr
;;                                  :filtered filtered
;;                                  :colname1 colname1
;;                                  :colname2 colname2})]
;;          (make-locus-tag-list (ds/mapseq-reader (r->clj top-table)))))

  (defn save-fit-method [{:keys [fit-method]}]
        (db-utils/submit-fit-method fit-method))

  (defn get-fit-method [{:keys []}]
        (db-utils/find-current-fit-method))

  (defn save-limma-trend [{:keys [trend]}]
        (db-utils/submit-limma-trend (read-string trend)))

  (defn get-limma-trend []
        (db-utils/find-limma-trend))

  (defn save-limma-trend-method [{:keys [trend-method]}]
        (db-utils/submit-limma-trend-method trend-method))

  (defn get-limma-trend-method []
        (db-utils/find-limma-trend-method))
