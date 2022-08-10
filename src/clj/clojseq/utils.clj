(ns clojseq.utils
      (:require [clojure.java.io :refer [reader]]
                [clojure.string :as str]
                [clojure.math.combinatorics :as combo]
                [clojisr.v1.r :as r :refer
                 [r eval-r->java r->java java->r java->clj java->native-clj
                  clj->java r->clj clj->r ->code r+ colon]]
                [clojisr.v1.require :refer [require-r]]
                [clojisr.v1.applications.plotting :refer [plot->svg plot->file plot->buffered-image]]
                [tech.v3.dataset :as ds]
                [cljam.io.gff :as gff]))

  (require-r '[edgeR]
             '[dplyr]
             '[textshape]
             '[genefilter]
             '[doParallel]
             '[tibble]
    ;;'[caret]
             '[stats]
             '[conflicted]
             '[base :refer [$]])

  (defn scale-data-old [dataset]
        (let [f (r '(function [data]
                              (do
                                (<- scaled (as.data.frame(t(apply data 1 scale))))
                                (<- nID (which(is.nan(bra scaled nil 1))))
                                ;colnames(data_st) <- colnames(data)
                                (<- (colnames scaled) (colnames data))
                                (bra<- scaled nID nil :value 0 ))))]
          (->>
            dataset
            f
            r->clj
            )))

  ;dataID <- counts_xyz$ID
  ;data2 = as.matrix(counts_xyz[,-1])
  ;d = DGEList(counts=data2, lib.size=rep(1,12))
  ;cpm.data.new <- cpm(d, TRUE, TRUE)
  ;counts_xyz_n <- betweenLaneNormalization(cpm.data.new, which="full", round=FALSE)
  ;counts_xyz_n = as.data.frame(counts_xyz_n)
  ;counts_xyz_n$ID <- dataID
  ;counts_xyz_n = counts_xyz_n[,c(13,1:12)]
  ;counts_xyz_ns = as.data.frame(t(apply(as.matrix(counts_xyz_n[,-1]), 1, scale)))
  ;counts_xyz_ns$ID = as.character(counts_xyz_n$ID)
  ;counts_xyz_ns = counts_xyz_ns[,c(13,1:12)]
  ;colnames(counts_xyz_ns) = colnames(counts_xyz_n)
  ;nID = which(is.nan(counts_xyz_ns[,2]))
  ;counts_xyz_ns[nID,2:length(counts_xyz_ns)] = 0

  (defonce feats-cds
           (with-open [r (gff/reader "resources/data/NostocN6_ncbi.gff")]
             ;; Retrieve header
             (let [feats (gff/read-features r)]
               (doall (filter #(= (:type %) "CDS") feats)))))

  ;;(defonce feats-all (with-open [r (gff/reader "resources/data/NostocN6_ncbi.gff")]
  ;;                 ;; Retrieve header
  ;;                 (let [feats (gff/read-features r)]
  ;;                   (doall feats))))

  (def fDGEList
       (r '(function [counts group]
                     (do
                       (<- d0 (DGEList counts))
                       (DGEList :counts d0 :group group)))))


  ;y <- voom(d, mm, plot = T)
  ;fit <- lmFit(y, mm)
  ;ebayes <- eBayes(fit)
  (defn model-matrix [gr]
  ((r "function(gr){
       mm <- stats::model.matrix(~0 + gr)
       colnames(mm) <- gsub('gr', '', colnames(mm))
       return(mm)}") gr))

(defn model-matrix1 [cnd]
  "condition: f.ex. samples$cond"
  ((r "function(cnd){
       design <- stats::model.matrix(~0 + cnd)
       colnames(design) <- gsub('cnd', '', colnames(design))
       return(design)}") cnd))

(defn model-matrix2 [cnd1 cnd2]
  ((r "function(cnd1, cnd2){
       group <- base::interaction(cnd1,cnd2)
       design <- stats::model.matrix(~0 + group)
       colnames(design) <- gsub('group', '', colnames(design))
       colnames(design) <- gsub('[.]', '', colnames(design))
       return(design)}") cnd1 cnd2))


  (defn samples-column [samples colname]
        ((r (str "function(samples){
        return(samples$" colname ")}")) samples))

(defn get-design-matrix [{:keys [samples colname1 colname2] :or {colname2 nil}}]
  (let [colname2 (if (= colname2 "null") nil colname2)
        design
        (cond
          (not= nil colname2) (model-matrix2 (samples-column samples colname1) (samples-column samples colname2))
          :else (model-matrix1 (samples-column samples colname1)))]
    (identity design)))


  (defn get-voom-ebayes [dge group]
        ((r "function(dge, group){
       conflict_prefer('eBayes', 'limma')
       y1 <- DGEList(counts=dge$counts, group=group)
       design <- model.matrix(~0+group, data=y1$samples)
       colnames(design) <- gsub('group', '', colnames(design))
       y <- voom(y1, design, plot = F)
       fit <- lmFit(y, design)
       ebayes <- eBayes(fit, trend=FALSE)
       return(ebayes)}") dge group))

  (defn get-voom-treat [dge group]
        ((r "function(dge, group){
       y1 <- DGEList(counts=dge$counts, group=group)
       design <- model.matrix(~0+group, data=y1$samples)
       colnames(design) <- gsub('group', '', colnames(design))
       y <- voom(y1, design, plot = F)
       fit <- lmFit(y, design)
       fit <- treat(fit, trend=FALSE)
       return(fit)}") dge group))

  (defn get-limma-trend-ebayes [dge group]
        ((r "function(dge, group){
       conflict_prefer('eBayes', 'limma')
       y <- DGEList(counts=dge$counts, group=group)
       design <- model.matrix(~0+group, data=y$samples)
       colnames(design) <- gsub('group', '', colnames(design))
       logCPM <- cpm(y, log=TRUE, prior.count=3)
       fit <- lmFit(logCPM, design)
       ebayes <- eBayes(fit, trend=TRUE)
       return(ebayes)}") dge group))

  (defn get-limma-trend-treat [dge group]
        ((r "function(dge, group){
       y <- DGEList(counts=dge$counts, group=group)
       design <- model.matrix(~0+group, data=y$samples)
       colnames(design) <- gsub('group', '', colnames(design))
       logCPM <- cpm(y, log=TRUE, prior.count=3)
       fit <- lmFit(logCPM, design)
       fit <- treat(fit, trend=TRUE)
       return(fit)}") dge group))


  (defn get-glmfit [dge group]
        ((r "function(dge, group){
       y <- DGEList(counts=dge$counts, group=group)
       design <- model.matrix(~0+group, data=y$samples)
       colnames(design) <- gsub('group', '', colnames(design))
       y <- estimateDisp(y, design)
       fit <- glmFit(y, design)
       return(fit)}") dge group))

  (defn get-glmfit-counts [counts group ggroup]
        ((r "function(counts, group, ggroup){
       y = DGEList(counts=counts, group=group)
       Group <- factor(ggroup)
       design <- model.matrix(~0+group, data=y$samples)
       colnames(design) <- levels(Group)
       y <- estimateDisp(y, design)
       fit <- glmFit(y, design)
       return(fit)}") counts group ggroup))

  (defn get-fit-contr [dgelist modelmatrix contrasts]
        ((r (str "function(data, modelmatrix, contrasts){
       conflict_prefer('eBayes', 'limma')
       y <- voom(data, modelmatrix, plot = F)
       fit <- lmFit(y, modelmatrix)
       contr <- makeContrasts(" contrasts ", levels = colnames(coef(fit)))
       tmp <- contrasts.fit(fit,contr)
       tmp <- eBayes(tmp)
       return(tmp)}")) dgelist modelmatrix contrasts))

  (def getFitContr
       (r '(<- fGetFitContr
               (function [dge mm contrasts]
                         (do
                           (<- y (voom dge mm :plot F))
                           (<- fit (lmFit y mm))
                           (<- colnames (colnames (coef fit)))
                           (<- contr (makeContrasts :contrasts contrasts :levels colnames))
                           (<- tmp (contrasts.fit fit contr))
                           (<- tmp (eBayes tmp)))))))

  (defn scale-data [dataset]
        ((r "function(data){
       dataID <- data$ID
       ct <- dim(as.data.frame(colnames(data)))[1]
       data2 = as.matrix(data[,-1])
       d = DGEList(counts=data2, lib.size=colSums(data2))
       d <- calcNormFactors(d,'TMM')
       cpm.data.new <- cpm(d, TRUE, TRUE)
       data_n <- cpm(cpm.data.new, normalized.lib.sizes=TRUE, log=TRUE)
       data_n = as.data.frame(data_n)
       data_n$ID <- dataID
       data_n = data_n[,c(ct,1:(ct - 1))]
       data_ns = as.data.frame(t(apply(as.matrix(data_n[,-1]), 1, scale)))
       data_ns$ID = as.character(data_n$ID)
       data_ns = data_ns[,c(ct,1:(ct - 1))]
       colnames(data_ns) = colnames(data_n)
       nID = which(is.nan(data_ns[,2]))
       data_ns[nID,2:length(data_ns)] = 0
       return(data_ns)}") dataset))

  (defn scale-dge [dge]
        ((r "function(dge){
       cpm.data.new <- cpm(dge, TRUE, TRUE)
       data_n <- cpm(cpm.data.new, normalized.lib.sizes=TRUE, log=TRUE)
       data_n = as.data.frame(data_n)
       data_ns = as.data.frame(t(apply(as.matrix(data_n), 1, scale)))
       colnames(data_ns) = colnames(data_n)
       data_ns <- cbind(ID = rownames(data_ns), data_ns)
       nID = which(is.nan(data_ns[,2]))
       data_ns[nID,2:length(data_ns)] = 0
       fdata <- remove_rownames(data_ns)
       return(fdata)}") dge))

  (defn scale-dge2 [dge columnnames]
        ((r "function(dge, columnnames){
       cpm.data.new <- cpm(dge, TRUE, TRUE)
       data_n <- cpm(cpm.data.new, normalized.lib.sizes=TRUE, log=TRUE)
       data_n = as.data.frame(data_n)
       data_ns = as.data.frame(t(apply(as.matrix(data_n), 1, scale)))
       colnames(data_ns) = columnnames
       data_ns <- cbind(ID = rownames(data_ns), data_ns)
       nID = which(is.nan(data_ns[,2]))
       data_ns[nID,2:length(data_ns)] = 0
       fdata <- remove_rownames(data_ns)
       return(fdata)}") dge columnnames))


  (defn transform-data [dataset]
        ((r "function(data){
       dataID <- data$ID
       tdata <- t(data[,-1])
       colnames(tdata) = dataID
       textshape::column_to_rownames(tdata,loc = 1)
       return(tdata)}") dataset))


  ;       column_to_rownames(tdata,'ID')
  (def logData
       (r '(<- logData
               (function [data]
                         (do
                           (<- data (+ 1 data))
                           (<- logged (as.data.frame(t(apply data 1 log))))
                           (<- nID (which(is.nan(bra logged nil 1))))
                           (<- (colnames logged) (colnames data))
                           (bra<- logged nID nil :value 0 ))))))

  (defn log-data [dataset]
        (->>
          dataset
          logData
          r->clj
          ))


  (defn log-rest [dataset]
        ""
        (let [col-names (ds/column-names dataset)
              first-col-name (first col-names)
              first-col (dataset first-col-name)
              rest-dataset (ds/remove-column dataset first-col-name)
              log-data (log-data rest-dataset)
              ]
          (println "log-rest")
          (->>
            (distinct (conj col-names first-col-name)) ;;reorder
            (ds/select-columns (ds/add-column log-data first-col)))))

  (defn convert-raw [dt-raw]
        (let [dt (ds/->dataset (read-string dt-raw))
              cols (sort (ds/column-names dt))]
          (ds/select-columns dt cols)))

  (defn starts-with [s v]
        (= s (subs v 0 (count s))))

  (defn split-groups [groups]
        (clojure.string/split groups #"_"))

  (defn group2numbers [group]
        "group is a clj vector of strings"
        (let [ds (distinct group)
              ps (map #(re-pattern %) ds)
              ns
              (for [p ps]
                (count (filter #(not= nil %) (map #(re-find (re-pattern p) %) group))))]
          (vec (flatten (for [i (range 1 (inc (count ns)))]
                          (repeat (nth ns (dec i)) i))))))

  (defn filter-columns [cols groups]
        "groups f.ex. \"S1_S3\""
        (let [groups   (->>
                         groups
                         split-groups
                         (conj ["ID"])
                         flatten)]
          (flatten
            (for [group groups] (filter #(starts-with group (name %)) cols)))))



  (defn contrasts2colnames [cols contrasts]
        (let [contrasts (str/split contrasts #"-")
              s1 (str (first contrasts) ".*")
              s2 (str (second contrasts) ".*")
              re1 (re-pattern s1)
              re2 (re-pattern s2)
              ]
          (vec (sort (filter
                       #(or
                          (= % :ID)
                          (re-find re1 (name %))
                          (re-find re2 (name %))) cols)))))


  (defn make-classes-from-columns [cols]
        (let [cls (map #(name %) cols)]
          (vec (for [cl cls]
                 (first (str/split cl #"\."))))))


  (defn row-names [data]
        ((r "function(data){
      rnames <- rownames(data)
      return(rnames)}") data))

  (defn filter-data-by-names [data names]
        ((r "function(data, names){
      mdata <- as.data.frame(data)
      ndata <- mdata[names,]
      return(ndata)}") data names))

  (defn logcpm-data [data]
        ((r "function(data){
       fdata <- remove_rownames(data)
       fdata <- as.data.frame(fdata)
       tdata <- textshape::column_to_rownames(fdata,loc = 1)
       ndata <- cpm(tdata, log=TRUE)
       return(ndata)}") data))

  (defn make-rownames-data [data]
        "changes first column to rownames "
        ((r "function(data){
       fdata <- remove_rownames(data)
       tdata <- textshape::column_to_rownames(fdata,loc = 1)
       return(tdata)}") data))


  (def fFilterAndNormalizeDge
       (r '(function [dge group]
                     (do
                       (<- keep (filterByExpr dge :group group))
                       (<- d0 (bra dge keep nil))
                       (calcNormFactors d0 :method "TMM")))))

  (def fFilterAndNormalize
       (r '(function [counts group]
                     (do
                       (<- d0 (DGEList counts))
                       (<- dge (DGEList :counts d0 :group group))
                       (<- keep (filterByExpr dge :group group))
                       (<- d0 (bra dge keep nil))
                       (calcNormFactors d0 :method "TMM")))))

  (defn make-contrasts-list [samples col attach]
        (let [cs (distinct (r->clj (samples-column samples col)))
              combs (distinct (into [] (map #(vec (sort (vec %))) (combo/combinations cs  2))))]
          (cond
            (true? attach) (vec (map #(str col (first %) " - " col (second %)) combs))
            :else (vec (map #(str (first %) " - " (second %)) combs)))))

