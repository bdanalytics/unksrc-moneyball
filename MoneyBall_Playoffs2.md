# MoneyBall:Baseball-Reference.com: Playoffs classification:: Playoffs2
bdanalytics  

**  **    
**Date: (Sat) Jun 13, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/baseball.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")
#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/baseball.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "Playoffs2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
glb_split_newdata_condition <- NULL # or "is.na(<var>)"; "<var> <condition_operator> <value>"
glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 123               # or any integer
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_max_fitent_obs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- !glb_is_regression; 
glb_is_binomial <- TRUE # or TRUE or FALSE

glb_rsp_var_raw <- "Playoffs"

# for classification, the response variable has to be a factor
glb_rsp_var <- "Playoffs.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
    relevel(factor(ifelse(raw == 1, "Y", "N")), as.factor(c("Y", "N")), ref="N")
    #as.factor(paste0("B", raw))
    #as.factor(gsub(" ", "\\.", raw))    
}
glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0))
```

```
## [1] Y Y N N N
## Levels: N Y
```

```r
glb_map_rsp_var_to_raw <- function(var) {
    as.numeric(var) - 1
    #as.numeric(var)
    #gsub("\\.", " ", levels(var)[as.numeric(var)])
    #c(" <=50K", " >50K")[as.numeric(var)]
    #c(FALSE, TRUE)[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0)))
```

```
## [1] 1 1 0 0 0
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- NULL # or c("<var1>")
glb_category_vars <- NULL # or c("<var1>", "<var2>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
# glb_assign_pairs_lst[["<var1>"]] <- list(from=c(NA),
#                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

glb_transform_lst <- NULL;
glb_transform_lst[["RankSeason"]] <- list(
    mapfn=function(raw) { tfr_raw <- as.character(raw); 
                          tfr_raw[is.na(tfr_raw)] <- "NA.my";
                          return(as.factor(tfr_raw)) }
    , sfx=".my.fctr")
glb_transform_lst[["RankPlayoffs"]] <- glb_transform_lst[["RankSeason"]]
# glb_transform_lst[["<var>"]] <- list(
#     mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
#                           tfr_raw[is.na(tfr_raw)] <- "NA.my";
#                           return(as.factor(tfr_raw)) }
#     , sfx=".my.fctr")
# mapfn(glb_allobs_df$RankSeason)
# mapfn(glb_allobs_df$RankPlayoffs)
# Add logs of numerics that are not distributed normally ->  do automatically ???
glb_transform_vars <- names(glb_transform_lst)

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- c("OOBP", "OSLG", "Team.fctr") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](MoneyBall_Playoffs2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 7.652  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
#glb_chunks_df <- myadd_chunk(NULL, "import.data")

glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/baseball.csv..."
## [1] "dimensions of data in ./data/baseball.csv: 1,232 rows x 15 cols"
##   Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 1  ARI     NL 2012 734 688 81 0.328 0.418 0.259        0         NA
## 2  ATL     NL 2012 700 600 94 0.320 0.389 0.247        1          4
## 3  BAL     AL 2012 712 705 93 0.311 0.417 0.247        1          5
## 4  BOS     AL 2012 734 806 69 0.315 0.415 0.260        0         NA
## 5  CHC     NL 2012 613 759 61 0.302 0.378 0.240        0         NA
## 6  CHW     AL 2012 748 676 85 0.318 0.422 0.255        0         NA
##   RankPlayoffs   G  OOBP  OSLG
## 1           NA 162 0.317 0.415
## 2            5 162 0.306 0.378
## 3            4 162 0.315 0.403
## 4           NA 162 0.331 0.428
## 5           NA 162 0.335 0.424
## 6           NA 162 0.319 0.405
##      Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 43    KCR     AL 2011 730 762 71 0.329 0.415 0.275        0         NA
## 188   CLE     AL 2006 870 782 78 0.349 0.457 0.280        0         NA
## 628   NYM     NL 1989 683 595 87 0.311 0.385 0.246        0         NA
## 896   STL     NL 1978 600 657 69 0.303 0.358 0.249        0         NA
## 903   CHC     NL 1977 692 739 81 0.330 0.387 0.266        0         NA
## 1218  CLE     AL 1962 682 745 80 0.312 0.388 0.245        0         NA
##      RankPlayoffs   G  OOBP  OSLG
## 43             NA 162 0.337 0.425
## 188            NA 162 0.335 0.431
## 628            NA 162    NA    NA
## 896            NA 162    NA    NA
## 903            NA 162    NA    NA
## 1218           NA 162    NA    NA
##      Team League Year  RS  RA   W   OBP   SLG    BA Playoffs RankSeason
## 1227  NYY     AL 1962 817 680  96 0.337 0.426 0.267        1          2
## 1228  PHI     NL 1962 705 759  81 0.330 0.390 0.260        0         NA
## 1229  PIT     NL 1962 706 626  93 0.321 0.394 0.268        0         NA
## 1230  SFG     NL 1962 878 690 103 0.341 0.441 0.278        1          1
## 1231  STL     NL 1962 774 664  84 0.335 0.394 0.271        0         NA
## 1232  WSA     AL 1962 599 716  60 0.308 0.373 0.250        0         NA
##      RankPlayoffs   G OOBP OSLG
## 1227            1 162   NA   NA
## 1228           NA 161   NA   NA
## 1229           NA 161   NA   NA
## 1230            2 165   NA   NA
## 1231           NA 163   NA   NA
## 1232           NA 162   NA   NA
## 'data.frame':	1232 obs. of  15 variables:
##  $ Team        : chr  "ARI" "ATL" "BAL" "BOS" ...
##  $ League      : chr  "NL" "NL" "AL" "AL" ...
##  $ Year        : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
##  $ RS          : int  734 700 712 734 613 748 669 667 758 726 ...
##  $ RA          : int  688 600 705 806 759 676 588 845 890 670 ...
##  $ W           : int  81 94 93 69 61 85 97 68 64 88 ...
##  $ OBP         : num  0.328 0.32 0.311 0.315 0.302 0.318 0.315 0.324 0.33 0.335 ...
##  $ SLG         : num  0.418 0.389 0.417 0.415 0.378 0.422 0.411 0.381 0.436 0.422 ...
##  $ BA          : num  0.259 0.247 0.247 0.26 0.24 0.255 0.251 0.251 0.274 0.268 ...
##  $ Playoffs    : int  0 1 1 0 0 0 1 0 0 1 ...
##  $ RankSeason  : int  NA 4 5 NA NA NA 2 NA NA 6 ...
##  $ RankPlayoffs: int  NA 5 4 NA NA NA 4 NA NA 2 ...
##  $ G           : int  162 162 162 162 162 162 162 162 162 162 ...
##  $ OOBP        : num  0.317 0.306 0.315 0.331 0.335 0.319 0.305 0.336 0.357 0.314 ...
##  $ OSLG        : num  0.415 0.378 0.403 0.428 0.424 0.405 0.39 0.43 0.47 0.402 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
if (glb_is_separate_newent_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
## Loading required package: caTools
```

```
##    Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 4   BOS     AL 2012 734 806 69 0.315 0.415 0.260        0         NA
## 6   CHW     AL 2012 748 676 85 0.318 0.422 0.255        0         NA
## 8   CLE     AL 2012 667 845 68 0.324 0.381 0.251        0         NA
## 12  KCR     AL 2012 676 746 72 0.317 0.400 0.265        0         NA
## 15  MIA     NL 2012 609 724 69 0.308 0.382 0.244        0         NA
## 22  PIT     NL 2012 651 674 79 0.304 0.395 0.243        0         NA
##    RankPlayoffs   G  OOBP  OSLG
## 4            NA 162 0.331 0.428
## 6            NA 162 0.319 0.405
## 8            NA 162 0.336 0.430
## 12           NA 162 0.339 0.423
## 15           NA 162 0.327 0.399
## 22           NA 162 0.314 0.390
##     Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 52   PIT     NL 2011 610 712 72 0.309 0.368 0.244        0         NA
## 156  CHW     AL 2007 693 839 72 0.318 0.404 0.246        0         NA
## 342  FLA     NL 2001 742 744 76 0.326 0.423 0.264        0         NA
## 425  BOS     AL 1998 876 729 92 0.348 0.463 0.280        1          5
## 746  CAL     AL 1984 696 697 81 0.319 0.381 0.249        0         NA
## 854  CLE     AL 1979 760 805 81 0.340 0.384 0.258        0         NA
##     RankPlayoffs   G  OOBP  OSLG
## 52            NA 162 0.338 0.409
## 156           NA 162 0.338 0.435
## 342           NA 162 0.338 0.411
## 425            4 162    NA    NA
## 746           NA 162    NA    NA
## 854           NA 161    NA    NA
##      Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 1215  CHC     NL 1962 632 827 59 0.317 0.377 0.253        0         NA
## 1219  DET     AL 1962 758 692 85 0.330 0.411 0.248        0         NA
## 1224  MIN     AL 1962 798 713 91 0.338 0.412 0.260        0         NA
## 1225  MLN     NL 1962 730 665 86 0.326 0.403 0.252        0         NA
## 1228  PHI     NL 1962 705 759 81 0.330 0.390 0.260        0         NA
## 1229  PIT     NL 1962 706 626 93 0.321 0.394 0.268        0         NA
##      RankPlayoffs   G OOBP OSLG
## 1215           NA 162   NA   NA
## 1219           NA 161   NA   NA
## 1224           NA 163   NA   NA
## 1225           NA 162   NA   NA
## 1228           NA 161   NA   NA
## 1229           NA 161   NA   NA
## 'data.frame':	369 obs. of  15 variables:
##  $ Team        : chr  "BOS" "CHW" "CLE" "KCR" ...
##  $ League      : chr  "AL" "AL" "AL" "AL" ...
##  $ Year        : int  2012 2012 2012 2012 2012 2012 2012 2012 2011 2011 ...
##  $ RS          : int  734 748 667 676 609 651 765 716 731 641 ...
##  $ RA          : int  806 676 845 746 724 674 648 784 662 605 ...
##  $ W           : int  69 85 68 72 69 79 88 73 94 89 ...
##  $ OBP         : num  0.315 0.318 0.324 0.317 0.308 0.304 0.338 0.309 0.322 0.308 ...
##  $ SLG         : num  0.415 0.422 0.381 0.4 0.382 0.395 0.421 0.407 0.413 0.387 ...
##  $ BA          : num  0.26 0.255 0.251 0.265 0.244 0.243 0.271 0.245 0.25 0.243 ...
##  $ Playoffs    : int  0 0 0 0 0 0 1 0 1 0 ...
##  $ RankSeason  : int  NA NA NA NA NA NA 6 NA 5 NA ...
##  $ RankPlayoffs: int  NA NA NA NA NA NA 3 NA 4 NA ...
##  $ G           : int  162 162 162 162 162 162 162 162 162 162 ...
##  $ OOBP        : num  0.331 0.319 0.336 0.339 0.327 0.314 0.313 0.335 0.316 0.31 ...
##  $ OSLG        : num  0.428 0.405 0.43 0.423 0.399 0.39 0.387 0.438 0.409 0.361 ...
##  - attr(*, "comment")= chr "glb_newobs_df"
##   Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 1  ARI     NL 2012 734 688 81 0.328 0.418 0.259        0         NA
## 2  ATL     NL 2012 700 600 94 0.320 0.389 0.247        1          4
## 3  BAL     AL 2012 712 705 93 0.311 0.417 0.247        1          5
## 5  CHC     NL 2012 613 759 61 0.302 0.378 0.240        0         NA
## 7  CIN     NL 2012 669 588 97 0.315 0.411 0.251        1          2
## 9  COL     NL 2012 758 890 64 0.330 0.436 0.274        0         NA
##   RankPlayoffs   G  OOBP  OSLG
## 1           NA 162 0.317 0.415
## 2            5 162 0.306 0.378
## 3            4 162 0.315 0.403
## 5           NA 162 0.335 0.424
## 7            4 162 0.305 0.390
## 9           NA 162 0.357 0.470
##      Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 92    ATL     NL 2009 735 641 86 0.339 0.405 0.263        0         NA
## 368   CIN     NL 2000 825 765 85 0.343 0.447 0.274        0         NA
## 382   PHI     NL 2000 708 830 65 0.329 0.400 0.251        0         NA
## 530   SEA     AL 1993 734 731 82 0.339 0.406 0.260        0         NA
## 756   MIN     AL 1984 673 675 81 0.318 0.385 0.265        0         NA
## 1158  CIN     NL 1965 825 704 89 0.339 0.439 0.273        0         NA
##      RankPlayoffs   G  OOBP  OSLG
## 92             NA 162 0.323 0.390
## 368            NA 163 0.341 0.438
## 382            NA 162 0.343 0.448
## 530            NA 162    NA    NA
## 756            NA 162    NA    NA
## 1158           NA 162    NA    NA
##      Team League Year  RS  RA   W   OBP   SLG    BA Playoffs RankSeason
## 1223  LAD     NL 1962 842 697 102 0.337 0.400 0.268        0         NA
## 1226  NYM     NL 1962 617 948  40 0.318 0.361 0.240        0         NA
## 1227  NYY     AL 1962 817 680  96 0.337 0.426 0.267        1          2
## 1230  SFG     NL 1962 878 690 103 0.341 0.441 0.278        1          1
## 1231  STL     NL 1962 774 664  84 0.335 0.394 0.271        0         NA
## 1232  WSA     AL 1962 599 716  60 0.308 0.373 0.250        0         NA
##      RankPlayoffs   G OOBP OSLG
## 1223           NA 165   NA   NA
## 1226           NA 161   NA   NA
## 1227            1 162   NA   NA
## 1230            2 165   NA   NA
## 1231           NA 163   NA   NA
## 1232           NA 162   NA   NA
## 'data.frame':	863 obs. of  15 variables:
##  $ Team        : chr  "ARI" "ATL" "BAL" "CHC" ...
##  $ League      : chr  "NL" "NL" "AL" "NL" ...
##  $ Year        : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
##  $ RS          : int  734 700 712 613 669 758 726 583 767 637 ...
##  $ RA          : int  688 600 705 759 588 890 670 794 699 597 ...
##  $ W           : int  81 94 93 61 97 64 88 55 89 86 ...
##  $ OBP         : num  0.328 0.32 0.311 0.302 0.315 0.33 0.335 0.302 0.332 0.317 ...
##  $ SLG         : num  0.418 0.389 0.417 0.378 0.411 0.436 0.422 0.371 0.433 0.374 ...
##  $ BA          : num  0.259 0.247 0.247 0.24 0.251 0.274 0.268 0.236 0.274 0.252 ...
##  $ Playoffs    : int  0 1 1 0 1 0 1 0 0 0 ...
##  $ RankSeason  : int  NA 4 5 NA 2 NA 6 NA NA NA ...
##  $ RankPlayoffs: int  NA 5 4 NA 4 NA 2 NA NA NA ...
##  $ G           : int  162 162 162 162 162 162 162 162 162 162 ...
##  $ OOBP        : num  0.317 0.306 0.315 0.335 0.305 0.357 0.314 0.337 0.31 0.31 ...
##  $ OSLG        : num  0.415 0.378 0.403 0.424 0.39 0.47 0.402 0.427 0.403 0.364 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(glb_trnobs_df)
    glb_newobs_df$.rownames <- rownames(glb_newobs_df)    
    glb_id_var <- ".rownames"
}
```

```
## Warning: using .rownames as identifiers for observations
```

```r
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

# Combine trnent & newent into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"
glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 7.652 8.084   0.432
## 2 inspect.data          2          0 8.085    NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

```
## Loading required package: reshape2
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-1.png) 

```
##       Playoffs.0 Playoffs.1
## Test         296         73
## Train        692        171
##       Playoffs.0 Playoffs.1
## Test    0.802168   0.197832
## Train   0.801854   0.198146
## [1] "numeric data missing in glb_allobs_df: "
##   RankSeason RankPlayoffs         OOBP         OSLG 
##          988          988          812          812 
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Playoffs 
##      988 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      Team    League .rownames 
##         0         0         0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   Playoffs Playoffs.fctr  .n
## 1        0             N 988
## 2        1             Y 244
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-2.png) 

```
##       Playoffs.fctr.N Playoffs.fctr.Y
## Test              296              73
## Train             692             171
##       Playoffs.fctr.N Playoffs.fctr.Y
## Test         0.802168        0.197832
## Train        0.801854        0.198146
```

```r
#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

myextract_dates_df <- function(df, vars, id_vars, rsp_var) {
    keep_feats <- c(NULL)
    for (var in vars) {
        dates_df            <- df[, id_vars, FALSE]        
        dates_df[, rsp_var] <- df[, rsp_var, FALSE]
        #dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df <- cbind(dates_df, data.frame(.date=strptime(df[, var], 
            glb_date_fmts[[var]], tz=glb_date_tzs[[var]])))
#         print(dates_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", ".date")])
#         print(glb_allobs_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", "Date")])     
#         print(head(glb_allobs_df[grepl("4/7/02 .:..", glb_allobs_df$Date), c("ID", "Arrest.fctr", "Date")]))
#         print(head(strptime(glb_allobs_df[grepl("4/7/02 .:..", glb_allobs_df$Date), "Date"], "%m/%e/%y %H:%M"))
        # Wrong data during EST->EDT transition
#         tmp <- strptime("4/7/02 2:00","%m/%e/%y %H:%M:%S"); print(tmp); print(is.na(tmp))
#         dates_df[dates_df$ID == 2068197, .date] <- tmp
#         grep("(.*?) 2:(.*)", glb_allobs_df[is.na(dates_df$.date), "Date"], value=TRUE)
#         dates_df[is.na(dates_df$.date), ".date"] <- 
#             data.frame(.date=strptime(gsub("(.*?) 2:(.*)", "\\1 3:\\2",
#                 glb_allobs_df[is.na(dates_df$.date), "Date"]), "%m/%e/%y %H:%M"))$.date
        if (sum(is.na(dates_df$.date)) > 0) {
            stop("NA POSIX dates for ", var)
            print(df[is.na(dates_df$.date), c(id_vars, rsp_var, var)])
        }    
        
        .date <- dates_df$.date
        dates_df[, paste0(var, ".POSIX")] <- .date
        dates_df[, paste0(var, ".year")] <- as.numeric(format(.date, "%Y"))
        dates_df[, paste0(var, ".year.fctr")] <- as.factor(format(.date, "%Y")) 
        dates_df[, paste0(var, ".month")] <- as.numeric(format(.date, "%m"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(.date, "%m"))
        dates_df[, paste0(var, ".date")] <- as.numeric(format(.date, "%d"))
        dates_df[, paste0(var, ".date.fctr")] <- 
            cut(as.numeric(format(.date, "%d")), 5) # by month week  
        dates_df[, paste0(var, ".juliandate")] <- as.numeric(format(.date, "%j"))        
        
        # wkday Sun=0; Mon=1; ...; Sat=6
        dates_df[, paste0(var, ".wkday")] <- as.numeric(format(.date, "%w"))
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(.date, "%w"))
        
        # Get US Federal Holidays for relevant years
        require(XML)
        doc.html = htmlTreeParse('http://about.usps.com/news/events-calendar/2012-federal-holidays.htm', useInternal = TRUE)
        
#         # Extract all the paragraphs (HTML tag is p, starting at
#         # the root of the document). Unlist flattens the list to
#         # create a character vector.
#         doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
#         # Replace all \n by spaces
#         doc.text = gsub('\\n', ' ', doc.text)
#         # Join all the elements of the character vector into a single
#         # character string, separated by spaces
#         doc.text = paste(doc.text, collapse = ' ')
        
        # parse the tree by tables
        txt <- unlist(strsplit(xpathSApply(doc.html, "//*/table", xmlValue), "\n"))
        # do some clean up with regular expressions
        txt <- grep("day, ", txt, value=TRUE)
        txt <- trimws(gsub("(.*?)day, (.*)", "\\2", txt))
#         txt <- gsub("\t","",txt)
#         txt <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", txt, perl=TRUE)
#         txt <- txt[!(txt %in% c("", "|"))]
        hldays <- strptime(paste(txt, ", 2012", sep=""), "%B %e, %Y")
        dates_df[, paste0(var, ".hlday")] <- 
            ifelse(format(.date, "%Y-%m-%d") %in% hldays, 1, 0)
        
        # NYState holidays 1.9., 13.10., 11.11., 27.11., 25.12.
        
        dates_df[, paste0(var, ".wkend")] <- as.numeric(
            (dates_df[, paste0(var, ".wkday")] %in% c(0, 6)) | 
            dates_df[, paste0(var, ".hlday")] )
        
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(.date, "%H"))
        dates_df[, paste0(var, ".hour.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%H")))) <= 1)
                   vals else cut(vals, 3) # by work-shift    
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(.date, "%M")) 
        dates_df[, paste0(var, ".minute.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%M")))) <= 1)
                   vals else cut(vals, 4) # by quarter-hours    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(.date, "%S")) 
        dates_df[, paste0(var, ".second.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%S")))) <= 1)
                   vals else cut(vals, 4) # by quarter-minutes

        dates_df[, paste0(var, ".day.minutes")] <- 
            60 * dates_df[, paste0(var, ".hour")] + 
                 dates_df[, paste0(var, ".minute")]
        if ((unq_vals_n <- length(unique(dates_df[, paste0(var, ".day.minutes")]))) > 1) {
            max_degree <- min(unq_vals_n, 5)
            dates_df[, paste0(var, ".day.minutes.poly.", 1:max_degree)] <- 
                as.matrix(poly(dates_df[, paste0(var, ".day.minutes")], max_degree))
        } else max_degree <- 0   
        
#         print(gp <- myplot_box(df=dates_df, ycol_names="PubDate.day.minutes", 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name=".rownames", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.1", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.day.minutes", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var))
# 
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name=c("PubDate.day.minutes", "PubDate.day.minutes.poly.4"), 
#                         colorcol_name=rsp_var))
        
#         print(gp <- myplot_scatter(df=subset(dates_df, Popular.fctr=="Y"), 
#                                    xcol_name=paste0(var, ".juliandate"), 
#                         ycol_name=paste0(var, ".day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_box(df=dates_df, ycol_names=paste0(var, ".hour"), 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_bar(df=dates_df, ycol_names=paste0(var, ".hour.fctr"), 
#                                xcol_name=rsp_var, 
#                                colorcol_name=paste0(var, ".hour.fctr")))                
        keep_feats <- paste(var, 
            c(".POSIX", ".year.fctr", ".month.fctr", ".date.fctr", ".wkday.fctr", 
              ".wkend", ".hour.fctr", ".minute.fctr", ".second.fctr"), sep="")
        if (max_degree > 0)
            keep_feats <- union(keep_feats, paste(var, 
              paste0(".day.minutes.poly.", 1:max_degree), sep=""))
        keep_feats <- intersect(keep_feats, names(dates_df))        
    }
    #myprint_df(dates_df)
    return(dates_df[, keep_feats])
}

if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          paste(glb_date_vars, c("", ".POSIX"), sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}

# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: Year"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: RS"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: RA"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: W"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: OBP"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: SLG"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-8.png) 

```
## [1] "feat: BA"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-9.png) 

```
## [1] "feat: RankSeason"
```

```
## Warning in loop_apply(n, do.ply): Removed 988 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 988 rows containing missing
## values (stat_summary).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_text).
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-10.png) 

```
## [1] "feat: RankPlayoffs"
```

```
## Warning in loop_apply(n, do.ply): Removed 988 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 988 rows containing missing
## values (stat_summary).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_text).
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-11.png) 

```
## [1] "feat: G"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-12.png) 

```
## [1] "feat: .rnorm"
```

![](MoneyBall_Playoffs2_files/figure-html/inspect.data-13.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5))

rm(srt_allobs_df, last1, last10, last100, pd)
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object
## 'srt_allobs_df' not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last1'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last10'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last100'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'pd' not
## found
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0  8.085 15.449   7.364
## 3   scrub.data          2          1 15.450     NA      NA
```

### Step `2.1: scrub data`

```r
# Options:
#   1. Not fill missing vars
#   2. Fill missing numerics with a different algorithm
#   3. Fill missing chars with data based on clusters 

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##   RankSeason RankPlayoffs         OOBP         OSLG 
##          988          988          812          812 
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Playoffs 
##      988 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      Team    League .rownames 
##         0         0         0
```

```r
# if (!is.null(glb_force_0_to_NA_vars)) {
#     for (feat in glb_force_0_to_NA_vars) {
#         warning("Forcing ", sum(glb_allobs_df[, feat] == 0),
#                 " obs with ", feat, " 0s to NAs")
#         glb_allobs_df[glb_allobs_df[, feat] == 0, feat] <- NA
#     }
# }

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##   RankSeason RankPlayoffs         OOBP         OSLG 
##          988          988          812          812 
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Playoffs 
##      988 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      Team    League .rownames 
##         0         0         0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 15.450 16.977   1.527
## 4 transform.data          2          2 16.977     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}

### Transformations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_transform_vars) {
    new_feat <- paste0(feat, glb_transform_lst[[feat]]$sfx)
    print(sprintf("Applying mapping function for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_transform_lst[[feat]]$mapfn(glb_allobs_df[, feat])

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_transform_vars)
}
```

```
## [1] "Applying mapping function for: RankSeason -> RankSeason.my.fctr..."
## [1] "Applying mapping function for: RankPlayoffs -> RankPlayoffs.my.fctr..."
```

### Step `2.2: transform data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 4      transform.data          2          2 16.977 17.018   0.041
## 5 manage.missing.data          2          3 17.018     NA      NA
```

```r
# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##   RankSeason RankPlayoffs         OOBP         OSLG 
##          988          988          812          812 
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Playoffs 
##      988 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      Team    League .rownames 
##         0         0         0
```

```r
# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    # complete(mice()) changes attributes of factors even though values don't change
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col], inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
    
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##   RankSeason RankPlayoffs         OOBP         OSLG 
##          988          988          812          812 
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Playoffs 
##      988 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      Team    League .rownames 
##         0         0         0
```

## Step `2.3: manage missing data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 5 manage.missing.data          2          3 17.018 17.108    0.09
## 6    extract.features          3          0 17.109     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 17.116  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 17.116 17.125
## 2 extract.features_factorize.str.vars          2          0 17.125     NA
##   elapsed
## 1   0.009
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##        Team      League   .rownames        .src 
##      "Team"    "League" ".rownames"      ".src"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               glb_exclude_vars_as_features)) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- factor(glb_allobs_df[, var], 
                        as.factor(unique(glb_allobs_df[, var])))
#         glb_trnobs_df[, paste0(var, ".fctr")] <- factor(glb_trnobs_df[, var], 
#                         as.factor(unique(glb_allobs_df[, var])))
#         glb_newobs_df[, paste0(var, ".fctr")] <- factor(glb_newobs_df[, var], 
#                         as.factor(unique(glb_allobs_df[, var])))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}
```

```
## Warning: Creating factors of string variable: Team: # of unique values: 39
```

```
## Warning: Creating factors of string variable: League: # of unique values: 2
```

```r
if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(re_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(re_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }
    #tmp_freq_df <- chk_pattern_freq("\\bNew (\\w)+", ignore.case=FALSE)
    #subset(chk_pattern_freq("\\bNew (\\w)+", ignore.case=FALSE), grepl("New [[:upper:]]", pattern))
    #chk_pattern_freq("\\bnew (\\W)+")

    chk_subfn <- function(pos_ix) {
        re_str <- gsubfn_args_lst[["re_str"]][[pos_ix]]
        print("re_str:"); print(re_str)
        rp_frmla <- gsubfn_args_lst[["rp_frmla"]][[pos_ix]]        
        print("rp_frmla:"); print(rp_frmla, showEnv=FALSE)
        tmp_vctr <- grep(re_str, txt_vctr, value=TRUE, ignore.case=TRUE)[1:5]
        print("Before:")
        print(tmp_vctr)
        print("After:")            
        print(gsubfn(re_str, rp_frmla, tmp_vctr, ignore.case=TRUE))
    }
    #chk_subfn(1)

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                            length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                            nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#all.equal(sav_txt_lst[["Headline"]][1:2000], glb_txt_lst[["Headline"]][1:2000])
#all.equal(sav_txt_lst[["Headline"]][1:1000], glb_txt_lst[["Headline"]][1:1000])
#all.equal(sav_txt_lst[["Headline"]][1:500], glb_txt_lst[["Headline"]][1:500])
#all.equal(sav_txt_lst[["Headline"]][1:200], glb_txt_lst[["Headline"]][1:200])
#all.equal(sav_txt_lst[["Headline"]][1:100], glb_txt_lst[["Headline"]][1:100])
#chk.equal( 1, 100)
#chk.equal(51, 100)
#chk.equal(81, 100)
#chk.equal(81,  90)
#chk.equal(81,  85)
#chk.equal(86,  90)
#chk.equal(96, 100)

#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        print(tmp_vctr <- grep("[[:upper:]]\\.", txt_vctr, value=TRUE, ignore.case=FALSE))
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        # Create user-specified pattern vectors 
        #   <txt_var>.P.year.colon
        txt_X_df[, paste0(txt_var_pfx, ".P.year.colon")] <-
            as.integer(0 + mycount_pattern_occ("[0-9]{4}:", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.daily.clip.report")] <-
            as.integer(0 + mycount_pattern_occ("Daily Clip Report", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.fashion.week")] <-
            as.integer(0 + mycount_pattern_occ("Fashion Week", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.first.draft")] <-
            as.integer(0 + mycount_pattern_occ("First Draft", glb_allobs_df[, txt_var]))

#sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
        if (txt_var %in% c("Snippet", "Abstract")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
                as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
                                                   glb_allobs_df[, txt_var]))
        }

#sum(mycount_pattern_occ("[0-9]{4}:", glb_allobs_df$Headline) > 0)
#sum(mycount_pattern_occ("Quandary(.*)(?=:)", glb_allobs_df$Headline, perl=TRUE) > 0)
#sum(mycount_pattern_occ("No Comment(.*):", glb_allobs_df$Headline) > 0)
#sum(mycount_pattern_occ("Friday Night Music:", glb_allobs_df$Headline) > 0)
        if (txt_var %in% c("Headline")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.facts.figures")] <-
                as.integer(0 + mycount_pattern_occ("Facts & Figures:", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.friday.night.music")] <-
                as.integer(0 + mycount_pattern_occ("Friday Night Music", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.no.comment.colon")] <-
                as.integer(0 + mycount_pattern_occ("No Comment(.*):", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.on.this.day")] <-
                as.integer(0 + mycount_pattern_occ("On This Day", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.quandary")] <-
                as.integer(0 + mycount_pattern_occ("Quandary(.*)(?=:)", glb_allobs_df[, txt_var], perl=TRUE))
            txt_X_df[, paste0(txt_var_pfx, ".P.readers.respond")] <-
                as.integer(0 + mycount_pattern_occ("Readers Respond", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.recap.colon")] <-
                as.integer(0 + mycount_pattern_occ("Recap:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.s.notebook")] <-
                as.integer(0 + mycount_pattern_occ("s Notebook", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.politic")] <-
                as.integer(0 + mycount_pattern_occ("Today in Politic", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.smallbusiness")] <-
                as.integer(0 + mycount_pattern_occ("Today in Small Business:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.verbatim.colon")] <-
                as.integer(0 + mycount_pattern_occ("Verbatim:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.what.we.are")] <-
                as.integer(0 + mycount_pattern_occ("What We're", glb_allobs_df[, txt_var]))
        }

#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 17.125 17.147
## 3                extract.features_end          3          0 17.147     NA
##   elapsed
## 2   0.022
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 17.125 17.147
## 1                extract.features_bgn          1          0 17.116 17.125
##   elapsed duration
## 2   0.022    0.022
## 1   0.009    0.009
## [1] "Total Elapsed Time: 17.147 secs"
```

![](MoneyBall_Playoffs2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](MoneyBall_Playoffs2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 6 extract.features          3          0 17.109 18.484   1.375
## 7     cluster.data          4          0 18.485     NA      NA
```

## Step `4.0: cluster data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##             label step_major step_minor    bgn    end elapsed
## 7    cluster.data          4          0 18.485 18.778   0.293
## 8 select.features          5          0 18.778     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
## Warning in cor(data.matrix(entity_df[, sel_feats]), y =
## as.numeric(entity_df[, : the standard deviation is zero
```

```
##                                        id        cor.y exclude.as.feat
## Playoffs                         Playoffs  1.000000000               1
## RankSeason.my.fctr     RankSeason.my.fctr -0.949611992               0
## RankPlayoffs.my.fctr RankPlayoffs.my.fctr -0.938108317               0
## W                                       W  0.609288958               0
## RS                                     RS  0.414414732               0
## OOBP                                 OOBP -0.393803306               1
## OBP                                   OBP  0.388488569               0
## SLG                                   SLG  0.354718407               0
## OSLG                                 OSLG -0.329646512               1
## BA                                     BA  0.322610861               0
## RA                                     RA -0.236112764               0
## Year                                 Year  0.162277736               0
## Team.fctr                       Team.fctr  0.030703618               1
## League.fctr                   League.fctr  0.021977242               0
## G                                       G -0.007824161               0
## .rnorm                             .rnorm  0.002105000               0
## RankSeason                     RankSeason           NA               1
## RankPlayoffs                 RankPlayoffs           NA               1
##                        cor.y.abs
## Playoffs             1.000000000
## RankSeason.my.fctr   0.949611992
## RankPlayoffs.my.fctr 0.938108317
## W                    0.609288958
## RS                   0.414414732
## OOBP                 0.393803306
## OBP                  0.388488569
## SLG                  0.354718407
## OSLG                 0.329646512
## BA                   0.322610861
## RA                   0.236112764
## Year                 0.162277736
## Team.fctr            0.030703618
## League.fctr          0.021977242
## G                    0.007824161
## .rnorm               0.002105000
## RankSeason                    NA
## RankPlayoffs                  NA
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## [1] "cor(RS, SLG)=0.9229"
## [1] "cor(Playoffs.fctr, RS)=0.4144"
## [1] "cor(Playoffs.fctr, SLG)=0.3547"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified SLG as highly correlated with RS
```

```
## [1] "cor(RankPlayoffs.my.fctr, RankSeason.my.fctr)=0.9135"
## [1] "cor(Playoffs.fctr, RankPlayoffs.my.fctr)=-0.9381"
## [1] "cor(Playoffs.fctr, RankSeason.my.fctr)=-0.9496"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified RankPlayoffs.my.fctr as highly correlated with
## RankSeason.my.fctr
```

```
## [1] "cor(OBP, RS)=0.9076"
## [1] "cor(Playoffs.fctr, OBP)=0.3885"
## [1] "cor(Playoffs.fctr, RS)=0.4144"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified OBP as highly correlated with RS
```

```
## [1] "cor(BA, RS)=0.8423"
## [1] "cor(Playoffs.fctr, BA)=0.3226"
## [1] "cor(Playoffs.fctr, RS)=0.4144"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified BA as highly correlated with RS
```

```
##                      id        cor.y exclude.as.feat   cor.y.abs
## 8              Playoffs  1.000000000               1 1.000000000
## 17                    W  0.609288958               0 0.609288958
## 14                   RS  0.414414732               0 0.414414732
## 5                   OBP  0.388488569               0 0.388488569
## 15                  SLG  0.354718407               0 0.354718407
## 2                    BA  0.322610861               0 0.322610861
## 18                 Year  0.162277736               0 0.162277736
## 16            Team.fctr  0.030703618               1 0.030703618
## 4           League.fctr  0.021977242               0 0.021977242
## 1                .rnorm  0.002105000               0 0.002105000
## 3                     G -0.007824161               0 0.007824161
## 9                    RA -0.236112764               0 0.236112764
## 7                  OSLG -0.329646512               1 0.329646512
## 6                  OOBP -0.393803306               1 0.393803306
## 11 RankPlayoffs.my.fctr -0.938108317               0 0.938108317
## 13   RankSeason.my.fctr -0.949611992               0 0.949611992
## 10         RankPlayoffs           NA               1          NA
## 12           RankSeason           NA               1          NA
##            cor.high.X freqRatio percentUnique zeroVar   nzv myNearZV
## 8                <NA>  4.046784     0.2317497   FALSE FALSE    FALSE
## 17               <NA>  1.032258     6.9524913   FALSE FALSE    FALSE
## 14               <NA>  1.250000    39.6292005   FALSE FALSE    FALSE
## 5                  RS  1.000000     9.8493627   FALSE FALSE    FALSE
## 15                 RS  1.071429    17.8447277   FALSE FALSE    FALSE
## 2                  RS  1.000000     8.3429896   FALSE FALSE    FALSE
## 18               <NA>  1.000000     5.4461182   FALSE FALSE    FALSE
## 16               <NA>  1.027027     4.4032445   FALSE FALSE    FALSE
## 4                <NA>  1.054762     0.2317497   FALSE FALSE    FALSE
## 1                <NA>  1.000000   100.0000000   FALSE FALSE    FALSE
## 3                <NA>  6.564356     0.9269988   FALSE FALSE    FALSE
## 9                <NA>  1.000000    39.3974508   FALSE FALSE    FALSE
## 7                <NA>  1.714286    11.5874855   FALSE FALSE    FALSE
## 6                <NA>  1.100000     7.9953650   FALSE FALSE    FALSE
## 11 RankSeason.my.fctr 11.533333     0.6952491   FALSE FALSE    FALSE
## 13               <NA> 17.743590     1.0428737   FALSE FALSE    FALSE
## 10               <NA>  1.224490     0.5793743   FALSE FALSE    FALSE
## 12               <NA>  1.026316     0.9269988   FALSE FALSE    FALSE
##    is.cor.y.abs.low
## 8             FALSE
## 17            FALSE
## 14            FALSE
## 5             FALSE
## 15            FALSE
## 2             FALSE
## 18            FALSE
## 16            FALSE
## 4             FALSE
## 1             FALSE
## 3             FALSE
## 9             FALSE
## 7             FALSE
## 6             FALSE
## 11            FALSE
## 13            FALSE
## 10               NA
## 12               NA
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (geom_point).
```

![](MoneyBall_Playoffs2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
##   RankSeason RankPlayoffs         OOBP         OSLG 
##          988          988          812          812 
## [1] "numeric data w/ 0s in : "
## Playoffs 
##      988 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##      Team    League .rownames 
##         0         0         0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 18.778 19.491   0.713
## 9 partition.data.training          6          0 19.491     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for Playoffs.fctr; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitent_obs) && (nrow(glb_fitobs_df) > glb_max_fitent_obs)) {
    warning("glb_fitobs_df restricted to glb_max_fitent_obs: ", 
            format(glb_max_fitent_obs, big.mark=","))
    org_fitent_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitent_df[split <- sample.split(org_fitent_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitent_obs), ]
    org_fitent_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newent_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newent_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 18 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                          id exclude.as.feat rsp_var
## Playoffs.fctr Playoffs.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                          id cor.y exclude.as.feat cor.y.abs cor.high.X
## 8                  Playoffs     1            TRUE         1       <NA>
## Playoffs.fctr Playoffs.fctr    NA            TRUE        NA       <NA>
##               freqRatio percentUnique zeroVar   nzv myNearZV
## 8              4.046784     0.2317497   FALSE FALSE    FALSE
## Playoffs.fctr        NA            NA      NA    NA       NA
##               is.cor.y.abs.low interaction.feat rsp_var_raw rsp_var
## 8                        FALSE               NA        TRUE      NA
## Playoffs.fctr               NA               NA          NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 1232   24
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 863  23
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 863  23
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 369  23
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 369  23
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 19.491 19.816   0.325
## 10              fit.models          7          0 19.817     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##        N        Y 
## 0.801854 0.198146 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## [1] "in MFO.Classifier$prob"
##          N        Y
## 1 0.801854 0.198146
## 2 0.801854 0.198146
## 3 0.801854 0.198146
## 4 0.801854 0.198146
## 5 0.801854 0.198146
## 6 0.801854 0.198146
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.MFO.myMFO_classfr.N
## 1             N                                       692
## 2             Y                                       171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y 171   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.018540e-01   0.000000e+00   7.736689e-01   8.279601e-01   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   5.204474e-01   1.219840e-38 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in MFO.Classifier$prob"
##          N        Y
## 1 0.801854 0.198146
## 2 0.801854 0.198146
## 3 0.801854 0.198146
## 4 0.801854 0.198146
## 5 0.801854 0.198146
## 6 0.801854 0.198146
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.MFO.myMFO_classfr.N
## 1             N                                       296
## 2             Y                                        73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y  73   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.021680e-01   0.000000e+00   7.578069e-01   8.416029e-01   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   5.312631e-01   3.547532e-17 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.379                 0.002         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0         0.801854
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7736689             0.8279601             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0         0.802168
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7578069             0.8416029             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in Random.Classifier$prob"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 0.3307544
## 3        0.2 0.1839763
## 4        0.3 0.1839763
## 5        0.4 0.1839763
## 6        0.5 0.1839763
## 7        0.6 0.1839763
## 8        0.7 0.1839763
## 9        0.8 0.1839763
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.Random.myrandom_classfr.Y
## 1             N                                             692
## 2             Y                                             171
##          Prediction
## Reference   N   Y
##         N   0 692
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.981460e-01   0.000000e+00   1.720399e-01   2.263311e-01   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00  4.466608e-152 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 0.3303167
## 3        0.2 0.1679389
## 4        0.3 0.1679389
## 5        0.4 0.1679389
## 6        0.5 0.1679389
## 7        0.6 0.1679389
## 8        0.7 0.1679389
## 9        0.8 0.1679389
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.Random.myrandom_classfr.Y
## 1             N                                             296
## 2             Y                                              73
##          Prediction
## Reference   N   Y
##         N   0 296
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.978320e-01   0.000000e+00   1.583971e-01   2.421931e-01   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   6.672316e-66 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.229                 0.002   0.4930999
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.1       0.3307544         0.198146
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.1720399             0.2263311             0   0.4959506
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3303167         0.197832
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.1583971             0.2421931             0
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: RankSeason.my.fctr, W"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 1 on full training set
```

```
## Loading required package: rpart.plot
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 863 
## 
##   CP nsplit rel error
## 1  1      0         1
## 
## Node number 1: 863 observations
##   predicted class=N  expected loss=0.198146  P(node) =1
##     class counts:   692   171
##    probabilities: 0.802 0.198 
## 
## n= 863 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 863 171 N (0.8018540 0.1981460) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1             N                                          692
## 2             Y                                          171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y 171   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.018540e-01   0.000000e+00   7.736689e-01   8.279601e-01   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   5.204474e-01   1.219840e-38 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1             N                                          296
## 2             Y                                           73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y  73   0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.021680e-01   0.000000e+00   7.578069e-01   8.416029e-01   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   5.312631e-01   3.547532e-17 
##               model_id model_method                 feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart RankSeason.my.fctr, W               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.625                  0.03         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0         0.801854
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7736689             0.8279601             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0         0.802168
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7578069             0.8416029             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: RankSeason.my.fctr, W"
## Fitting cp = 0 on full training set
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 863 
## 
##   CP nsplit rel error
## 1  1      0         1
## 2  0      1         0
## 
## Variable importance
## RankSeason.my.fctrNA.my                       W     RankSeason.my.fctr4 
##                      46                      24                      10 
##     RankSeason.my.fctr2     RankSeason.my.fctr3     RankSeason.my.fctr5 
##                       8                       8                       4 
## 
## Node number 1: 863 observations,    complexity param=1
##   predicted class=N  expected loss=0.198146  P(node) =1
##     class counts:   692   171
##    probabilities: 0.802 0.198 
##   left son=2 (692 obs) right son=3 (171 obs)
##   Primary splits:
##       RankSeason.my.fctrNA.my < 0.5  to the right, improve=274.23410, (0 missing)
##       W                       < 89.5 to the left,  improve=136.46170, (0 missing)
##       RankSeason.my.fctr4     < 0.5  to the left,  improve= 51.11649, (0 missing)
##       RankSeason.my.fctr2     < 0.5  to the left,  improve= 41.34945, (0 missing)
##       RankSeason.my.fctr3     < 0.5  to the left,  improve= 39.96756, (0 missing)
##   Surrogate splits:
##       W                   < 91.5 to the left,  agree=0.905, adj=0.520, (0 split)
##       RankSeason.my.fctr4 < 0.5  to the left,  agree=0.846, adj=0.222, (0 split)
##       RankSeason.my.fctr2 < 0.5  to the left,  agree=0.838, adj=0.181, (0 split)
##       RankSeason.my.fctr3 < 0.5  to the left,  agree=0.837, adj=0.175, (0 split)
##       RankSeason.my.fctr5 < 0.5  to the left,  agree=0.818, adj=0.082, (0 split)
## 
## Node number 2: 692 observations
##   predicted class=N  expected loss=0  P(node) =0.801854
##     class counts:   692     0
##    probabilities: 1.000 0.000 
## 
## Node number 3: 171 observations
##   predicted class=Y  expected loss=0  P(node) =0.198146
##     class counts:     0   171
##    probabilities: 0.000 1.000 
## 
## n= 863 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 863 171 N (0.8018540 0.1981460)  
##   2) RankSeason.my.fctrNA.my>=0.5 692   0 N (1.0000000 0.0000000) *
##   3) RankSeason.my.fctrNA.my< 0.5 171   0 Y (0.0000000 1.0000000) *
## [1] "    calling mypredict_mdl for fit:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 1.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 1.0000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1             N                                               692
## 2             Y                                                NA
##   Playoffs.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                NA
## 2                                               171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-9.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 1.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 1.0000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1             N                                               296
## 2             Y                                                NA
##   Playoffs.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                NA
## 2                                                73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##                    model_id model_method                 feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart RankSeason.my.fctr, W
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.501                 0.029
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                      1               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                      1               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.9900528                     1             1
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: RankSeason.my.fctr, W"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.5 on full training set
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-11.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 863 
## 
##   CP nsplit rel error
## 1  1      0         1
## 2  0      1         0
## 
## Variable importance
## RankSeason.my.fctrNA.my                       W     RankSeason.my.fctr4 
##                      46                      24                      10 
##     RankSeason.my.fctr2     RankSeason.my.fctr3     RankSeason.my.fctr5 
##                       8                       8                       4 
## 
## Node number 1: 863 observations,    complexity param=1
##   predicted class=N  expected loss=0.198146  P(node) =1
##     class counts:   692   171
##    probabilities: 0.802 0.198 
##   left son=2 (692 obs) right son=3 (171 obs)
##   Primary splits:
##       RankSeason.my.fctrNA.my < 0.5  to the right, improve=274.23410, (0 missing)
##       W                       < 89.5 to the left,  improve=136.46170, (0 missing)
##       RankSeason.my.fctr4     < 0.5  to the left,  improve= 51.11649, (0 missing)
##       RankSeason.my.fctr2     < 0.5  to the left,  improve= 41.34945, (0 missing)
##       RankSeason.my.fctr3     < 0.5  to the left,  improve= 39.96756, (0 missing)
##   Surrogate splits:
##       W                   < 91.5 to the left,  agree=0.905, adj=0.520, (0 split)
##       RankSeason.my.fctr4 < 0.5  to the left,  agree=0.846, adj=0.222, (0 split)
##       RankSeason.my.fctr2 < 0.5  to the left,  agree=0.838, adj=0.181, (0 split)
##       RankSeason.my.fctr3 < 0.5  to the left,  agree=0.837, adj=0.175, (0 split)
##       RankSeason.my.fctr5 < 0.5  to the left,  agree=0.818, adj=0.082, (0 split)
## 
## Node number 2: 692 observations
##   predicted class=N  expected loss=0  P(node) =0.801854
##     class counts:   692     0
##    probabilities: 1.000 0.000 
## 
## Node number 3: 171 observations
##   predicted class=Y  expected loss=0  P(node) =0.198146
##     class counts:     0   171
##    probabilities: 0.000 1.000 
## 
## n= 863 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 863 171 N (0.8018540 0.1981460)  
##   2) RankSeason.my.fctrNA.my>=0.5 692   0 N (1.0000000 0.0000000) *
##   3) RankSeason.my.fctrNA.my< 0.5 171   0 Y (0.0000000 1.0000000) *
## [1] "    calling mypredict_mdl for fit:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-13.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 1.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-14.png) 

```
## [1] "Classifier Probability Threshold: 1.0000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.rpart.N
## 1             N                                     692
## 2             Y                                      NA
##   Playoffs.fctr.predict.Max.cor.Y.rpart.Y
## 1                                      NA
## 2                                     171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-15.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 1.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-16.png) 

```
## [1] "Classifier Probability Threshold: 1.0000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.rpart.N
## 1             N                                     296
## 2             Y                                      NA
##   Playoffs.fctr.predict.Max.cor.Y.rpart.Y
## 1                                      NA
## 2                                      73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##          model_id model_method                 feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart RankSeason.my.fctr, W               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.094                 0.029           1
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                      1               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                      1               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.9900528                     1             1
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.glm"
## [1] "    indep_vars: RankSeason.my.fctr, W"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: algorithm did not converge
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-17.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-18.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-19.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##        Min          1Q      Median          3Q         Max  
## -2.409e-06  -2.409e-06  -2.409e-06  -2.409e-06   2.409e-06  
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)
## (Intercept)              2.657e+01  1.478e+05   0.000    1.000
## RankSeason.my.fctr2      2.189e-07  8.588e+04   0.000    1.000
## RankSeason.my.fctr3      2.026e-07  8.692e+04   0.000    1.000
## RankSeason.my.fctr4      4.647e-06  8.215e+04   0.000    1.000
## RankSeason.my.fctr5      4.850e-06  1.118e+05   0.000    1.000
## RankSeason.my.fctr6      1.977e-07  1.225e+05   0.000    1.000
## RankSeason.my.fctr7      2.106e-07  1.474e+05   0.000    1.000
## RankSeason.my.fctr8      7.264e-09  3.611e+05   0.000    1.000
## RankSeason.my.fctrNA.my -5.313e+01  6.687e+04  -0.001    0.999
## W                       -3.179e-15  1.352e+03   0.000    1.000
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 8.5924e+02  on 862  degrees of freedom
## Residual deviance: 5.0068e-09  on 853  degrees of freedom
## AIC: 20
## 
## Number of Fisher Scoring iterations: 25
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-21.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-22.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.glm.N
## 1             N                                   692
## 2             Y                                    NA
##   Playoffs.fctr.predict.Max.cor.Y.glm.Y
## 1                                    NA
## 2                                   171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-23.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-24.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.glm.N
## 1             N                                   296
## 2             Y                                    NA
##   Playoffs.fctr.predict.Max.cor.Y.glm.Y
## 1                                    NA
## 2                                    73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##        model_id model_method                 feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm RankSeason.my.fctr, W               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.819                 0.067           1
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.9               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.9900528                     1             1          20
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.glm"
## [1] "    indep_vars: RankSeason.my.fctr, W, RankSeason.my.fctr:RS, RankSeason.my.fctr:RankSeason.my.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: not plotting observations with leverage one:
##   439
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-25.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-26.png) 

```
## Warning: not plotting observations with leverage one:
##   439
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-27.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##        Min          1Q      Median          3Q         Max  
## -2.409e-06  -2.409e-06  -2.409e-06  -2.409e-06   2.409e-06  
## 
## Coefficients: (1 not defined because of singularities)
##                                Estimate Std. Error z value Pr(>|z|)
## (Intercept)                   2.657e+01  6.383e+05       0        1
## RankSeason.my.fctr2          -1.817e-07  9.103e+05       0        1
## RankSeason.my.fctr3          -1.485e-08  8.622e+05       0        1
## RankSeason.my.fctr4           5.015e-09  8.321e+05       0        1
## RankSeason.my.fctr5          -1.964e-07  1.201e+06       0        1
## RankSeason.my.fctr6          -1.916e-06  1.553e+06       0        1
## RankSeason.my.fctr7           4.455e-06  2.158e+06       0        1
## RankSeason.my.fctr8          -1.904e-07  7.149e+05       0        1
## RankSeason.my.fctrNA.my      -5.313e+01  6.354e+05       0        1
## W                             1.417e-13  1.491e+03       0        1
## `RankSeason.my.fctr1:RS`     -7.594e-12  7.675e+02       0        1
## `RankSeason.my.fctr2:RS`     -1.644e-11  8.305e+02       0        1
## `RankSeason.my.fctr3:RS`      3.924e-12  7.644e+02       0        1
## `RankSeason.my.fctr4:RS`     -2.238e-11  7.127e+02       0        1
## `RankSeason.my.fctr5:RS`      1.113e-11  1.296e+03       0        1
## `RankSeason.my.fctr6:RS`      2.177e-09  1.736e+03       0        1
## `RankSeason.my.fctr7:RS`     -4.389e-11  2.682e+03       0        1
## `RankSeason.my.fctr8:RS`             NA         NA      NA       NA
## `RankSeason.my.fctrNA.my:RS` -6.714e-15  1.723e+02       0        1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 8.5924e+02  on 862  degrees of freedom
## Residual deviance: 5.0068e-09  on 845  degrees of freedom
## AIC: 36
## 
## Number of Fisher Scoring iterations: 25
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-28.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-29.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.Interact.High.cor.Y.glm.N
## 1             N                                             692
## 2             Y                                              NA
##   Playoffs.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                              NA
## 2                                             171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-30.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-31.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-32.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.Interact.High.cor.Y.glm.N
## 1             N                                             296
## 2             Y                                              NA
##   Playoffs.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                              NA
## 2                                              73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                 feats
## 1 RankSeason.my.fctr, W, RankSeason.my.fctr:RS, RankSeason.my.fctr:RankSeason.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.373                 0.088
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.9               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.9900528                     1             1          36
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.glm"
## [1] "    indep_vars: W, RS, Year, League.fctr, .rnorm, G, RA, RankSeason.my.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: not plotting observations with leverage one:
##   439
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-33.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-34.png) 

```
## Warning: not plotting observations with leverage one:
##   439
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-35.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-36.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##        Min          1Q      Median          3Q         Max  
## -2.409e-06  -2.409e-06  -2.409e-06  -2.409e-06   2.409e-06  
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)
## (Intercept)              2.657e+01  3.665e+06   0.000    1.000
## W                        2.894e-14  3.224e+03   0.000    1.000
## RS                      -5.564e-17  3.574e+02   0.000    1.000
## Year                    -2.651e-14  9.629e+02   0.000    1.000
## League.fctrAL           -2.541e-13  2.564e+04   0.000    1.000
## .rnorm                   4.921e-13  1.239e+04   0.000    1.000
## G                       -2.004e-13  1.923e+04   0.000    1.000
## RA                       3.221e-16  3.562e+02   0.000    1.000
## RankSeason.my.fctr2      2.303e-09  8.599e+04   0.000    1.000
## RankSeason.my.fctr3      4.826e-06  8.706e+04   0.000    1.000
## RankSeason.my.fctr4     -2.444e-08  8.236e+04   0.000    1.000
## RankSeason.my.fctr5      6.768e-09  1.127e+05   0.000    1.000
## RankSeason.my.fctr6      2.011e-07  1.233e+05   0.000    1.000
## RankSeason.my.fctr7      9.863e-09  1.484e+05   0.000    1.000
## RankSeason.my.fctr8      4.634e-06  3.625e+05   0.000    1.000
## RankSeason.my.fctrNA.my -5.313e+01  6.715e+04  -0.001    0.999
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 8.5924e+02  on 862  degrees of freedom
## Residual deviance: 5.0068e-09  on 847  degrees of freedom
## AIC: 32
## 
## Number of Fisher Scoring iterations: 25
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-37.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-38.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.Low.cor.X.glm.N
## 1             N                                   692
## 2             Y                                    NA
##   Playoffs.fctr.predict.Low.cor.X.glm.Y
## 1                                    NA
## 2                                   171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-39.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_0-40.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.Low.cor.X.glm.N
## 1             N                                   296
## 2             Y                                    NA
##   Playoffs.fctr.predict.Low.cor.X.glm.Y
## 1                                    NA
## 2                                    73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                         feats
## 1 W, RS, Year, League.fctr, .rnorm, G, RA, RankSeason.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.323                 0.084
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.9               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.9900528                     1             1          32
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 19.817 44.667   24.85
## 11 fit.models          7          1 44.667     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 48.163  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 48.163 48.178   0.015
## 2 fit.models_1_glm          2          0 48.178     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: W, RS, OBP, SLG, BA, Year, League.fctr, .rnorm, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: not plotting observations with leverage one:
##   439
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-1.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   439
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##        Min          1Q      Median          3Q         Max  
## -2.409e-06  -2.409e-06  -2.409e-06  -2.409e-06   2.409e-06  
## 
## Coefficients: (1 not defined because of singularities)
##                             Estimate Std. Error z value Pr(>|z|)
## (Intercept)                2.657e+01  3.778e+06   0.000        1
## W                         -7.815e-12  3.241e+03   0.000        1
## RS                         6.876e-13  5.852e+02   0.000        1
## OBP                       -2.070e-09  2.245e+06   0.000        1
## SLG                       -2.510e-09  1.102e+06   0.000        1
## BA                         1.181e-08  2.004e+06   0.000        1
## Year                      -2.403e-12  1.048e+03   0.000        1
## League.fctrAL              2.574e-10  2.570e+04   0.000        1
## .rnorm                    -3.438e-11  1.245e+04   0.000        1
## G                          8.925e-11  1.957e+04   0.000        1
## RA                        -2.701e-13  3.589e+02   0.000        1
## RankPlayoffs.my.fctr2     -1.245e-09  9.472e+04   0.000        1
## RankPlayoffs.my.fctr3      4.923e-08  8.496e+04   0.000        1
## RankPlayoffs.my.fctr4     -2.616e-07  8.809e+04   0.000        1
## RankPlayoffs.my.fctr5      1.289e-07  2.671e+05   0.000        1
## RankPlayoffs.my.fctrNA.my -5.313e+01  8.974e+04  -0.001        1
## RankSeason.my.fctr2       -2.687e-08  8.765e+04   0.000        1
## RankSeason.my.fctr3       -4.552e-08  8.843e+04   0.000        1
## RankSeason.my.fctr4       -2.463e-07  8.609e+04   0.000        1
## RankSeason.my.fctr5        1.578e-07  1.190e+05   0.000        1
## RankSeason.my.fctr6        4.712e-06  1.244e+05   0.000        1
## RankSeason.my.fctr7       -3.481e-08  1.509e+05   0.000        1
## RankSeason.my.fctr8        1.408e-07  3.662e+05   0.000        1
## RankSeason.my.fctrNA.my           NA         NA      NA       NA
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 8.5924e+02  on 862  degrees of freedom
## Residual deviance: 5.0068e-09  on 840  degrees of freedom
## AIC: 46
## 
## Number of Fisher Scoring iterations: 25
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-4.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.All.X.glm.N
## 1             N                               692
## 2             Y                                NA
##   Playoffs.fctr.predict.All.X.glm.Y
## 1                                NA
## 2                               171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-6.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.All.X.glm.N
## 1             N                               296
## 2             Y                                NA
##   Playoffs.fctr.predict.All.X.glm.Y
## 1                                NA
## 2                                73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                             feats
## 1 W, RS, OBP, SLG, BA, Year, League.fctr, .rnorm, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.231                  0.11
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.9               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.9900528                     1             1          46
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
##                   label step_major step_minor    bgn    end elapsed
## 2      fit.models_1_glm          2          0 48.178 52.608   4.431
## 3 fit.models_1_bayesglm          3          0 52.609     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: W, RS, OBP, SLG, BA, Year, League.fctr, .rnorm, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## Loading required package: Rcpp
## 
## arm (Version 1.8-5, built: 2015-05-13)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Lectures/LCTR2_MoneyBall
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.04005  -0.02455  -0.01998  -0.01420   0.06935  
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)
## (Intercept)                -4.190283 258.465496  -0.016    0.987
## W                           0.024309   0.094132   0.258    0.796
## RS                          0.001311   0.011433   0.115    0.909
## OBP                         6.568067  67.877618   0.097    0.923
## SLG                         2.797860  31.096879   0.090    0.928
## BA                          5.512166  79.032406   0.070    0.944
## Year                        0.003172   0.063762   0.050    0.960
## League.fctrAL              -0.017711   1.852666  -0.010    0.992
## .rnorm                      0.004912   0.913985   0.005    0.996
## G                          -0.016931   1.399459  -0.012    0.990
## RA                         -0.001542   0.010923  -0.141    0.888
## RankPlayoffs.my.fctr2       0.198330   2.195903   0.090    0.928
## RankPlayoffs.my.fctr3       0.323159   2.114767   0.153    0.879
## RankPlayoffs.my.fctr4       0.247170   2.168773   0.114    0.909
## RankPlayoffs.my.fctr5       0.016450   2.461040   0.007    0.995
## RankPlayoffs.my.fctrNA.my  -7.020024   4.769565  -1.472    0.141
## RankSeason.my.fctr2         0.159626   2.226667   0.072    0.943
## RankSeason.my.fctr3         0.172003   2.214511   0.078    0.938
## RankSeason.my.fctr4         0.230393   2.162852   0.107    0.915
## RankSeason.my.fctr5         0.099515   2.310484   0.043    0.966
## RankSeason.my.fctr6         0.073320   2.350123   0.031    0.975
## RankSeason.my.fctr7         0.054289   2.382802   0.023    0.982
## RankSeason.my.fctr8         0.006279   2.484760   0.003    0.998
## RankSeason.my.fctrNA.my    -7.020024   4.769565  -1.472    0.141
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 859.23986  on 862  degrees of freedom
## Residual deviance:   0.71789  on 839  degrees of freedom
## AIC: 48.718
## 
## Number of Fisher Scoring iterations: 30
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-9.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.All.X.bayesglm.N
## 1             N                                    692
## 2             Y                                     NA
##   Playoffs.fctr.predict.All.X.bayesglm.Y
## 1                                     NA
## 2                                    171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.All.X.bayesglm.N
## 1             N                                    296
## 2             Y                                     NA
##   Playoffs.fctr.predict.All.X.bayesglm.Y
## 1                                     NA
## 2                                     73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                             feats
## 1 W, RS, OBP, SLG, BA, Year, League.fctr, .rnorm, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.075                 0.186
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.9               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.9900528                     1             1    48.71789
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
##                   label step_major step_minor    bgn    end elapsed
## 3 fit.models_1_bayesglm          3          0 52.609 57.446   4.837
## 4    fit.models_1_rpart          4          0 57.447     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: W, RS, OBP, SLG, BA, Year, League.fctr, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.5 on full training set
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-13.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-14.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 863 
## 
##   CP nsplit rel error
## 1  1      0         1
## 2  0      1         0
## 
## Variable importance
## RankPlayoffs.my.fctrNA.my   RankSeason.my.fctrNA.my 
##                        30                        30 
##                         W     RankPlayoffs.my.fctr3 
##                        15                        10 
##     RankPlayoffs.my.fctr4       RankSeason.my.fctr4 
##                         8                         7 
## 
## Node number 1: 863 observations,    complexity param=1
##   predicted class=N  expected loss=0.198146  P(node) =1
##     class counts:   692   171
##    probabilities: 0.802 0.198 
##   left son=2 (692 obs) right son=3 (171 obs)
##   Primary splits:
##       RankPlayoffs.my.fctrNA.my < 0.5  to the right, improve=274.23410, (0 missing)
##       RankSeason.my.fctrNA.my   < 0.5  to the right, improve=274.23410, (0 missing)
##       W                         < 89.5 to the left,  improve=136.46170, (0 missing)
##       RankPlayoffs.my.fctr3     < 0.5  to the left,  improve= 82.92149, (0 missing)
##       RankPlayoffs.my.fctr4     < 0.5  to the left,  improve= 66.80409, (0 missing)
##   Surrogate splits:
##       RankSeason.my.fctrNA.my < 0.5  to the right, agree=1.000, adj=1.000, (0 split)
##       W                       < 91.5 to the left,  agree=0.905, adj=0.520, (0 split)
##       RankPlayoffs.my.fctr3   < 0.5  to the left,  agree=0.871, adj=0.351, (0 split)
##       RankPlayoffs.my.fctr4   < 0.5  to the left,  agree=0.859, adj=0.287, (0 split)
##       RankSeason.my.fctr4     < 0.5  to the left,  agree=0.846, adj=0.222, (0 split)
## 
## Node number 2: 692 observations
##   predicted class=N  expected loss=0  P(node) =0.801854
##     class counts:   692     0
##    probabilities: 1.000 0.000 
## 
## Node number 3: 171 observations
##   predicted class=Y  expected loss=0  P(node) =0.198146
##     class counts:     0   171
##    probabilities: 0.000 1.000 
## 
## n= 863 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 863 171 N (0.8018540 0.1981460)  
##   2) RankPlayoffs.my.fctrNA.my>=0.5 692   0 N (1.0000000 0.0000000) *
##   3) RankPlayoffs.my.fctrNA.my< 0.5 171   0 Y (0.0000000 1.0000000) *
## [1] "    calling mypredict_mdl for fit:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-15.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 1.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-16.png) 

```
## [1] "Classifier Probability Threshold: 1.0000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.All.X.no.rnorm.rpart.N
## 1             N                                          692
## 2             Y                                           NA
##   Playoffs.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                           NA
## 2                                          171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-17.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 1.0000000
```

```
## [1] "Classifier Probability Threshold: 1.0000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.All.X.no.rnorm.rpart.N
## 1             N                                          296
## 2             Y                                           NA
##   Playoffs.fctr.predict.All.X.no.rnorm.rpart.Y
## 1                                           NA
## 2                                           73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                     feats
## 1 W, RS, OBP, SLG, BA, Year, League.fctr, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.153                 0.059
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                      1               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                      1               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.9900528                     1             1
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
##                label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_rpart          4          0 57.447 61.964   4.517
## 5    fit.models_1_rf          5          0 61.964     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: W, RS, OBP, SLG, BA, Year, League.fctr, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-18.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 2 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-19.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-20.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted        863   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           1726   matrix     numeric  
## oob.times        863   -none-     numeric  
## classes            2   -none-     character
## importance        22   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y                863   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            22   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-21.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 0.9604863
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-22.png) 

```
## [1] "Classifier Probability Threshold: 0.8000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.All.X.no.rnorm.rf.N
## 1             N                                       692
## 2             Y                                        NA
##   Playoffs.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                        NA
## 2                                       171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-23.png) 

```
##    threshold   f.score
## 1        0.0 0.3303167
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 0.9931034
## 10       0.9 0.8854962
## 11       1.0 0.0000000
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_1-24.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   Playoffs.fctr Playoffs.fctr.predict.All.X.no.rnorm.rf.N
## 1             N                                       296
## 2             Y                                        NA
##   Playoffs.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                        NA
## 2                                        73
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.900528e-01   1.000000e+00   8.021680e-01 
## AccuracyPValue  McnemarPValue 
##   4.719677e-36            NaN 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                     feats
## 1 W, RS, OBP, SLG, BA, Year, League.fctr, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      4.564                 0.817
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.8               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9957346                     1             1           1
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7               1                1
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.9900528                     1             1
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
```

```r
# User specified
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df

    # easier to exclude features
#model_id_pfx <- "";
# indep_vars_vctr <- setdiff(names(glb_fitobs_df), 
#                         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#                                 c("<feat1_name>", "<feat2_name>")))
# method <- ""                                

    # easier to include features
#model_id <- "Rank9.2"; indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
# for (method in c("bayesglm")) {
#     ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                     n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
# #     csm_mdl_id <- paste0(model_id, ".", method)
# #     csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
# }
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                     feats
## MFO.myMFO_classfr                                                                                                  .rnorm
## Random.myrandom_classfr                                                                                            .rnorm
## Max.cor.Y.cv.0.rpart                                                                                RankSeason.my.fctr, W
## Max.cor.Y.cv.0.cp.0.rpart                                                                           RankSeason.my.fctr, W
## Max.cor.Y.rpart                                                                                     RankSeason.my.fctr, W
## Max.cor.Y.glm                                                                                       RankSeason.my.fctr, W
## Interact.High.cor.Y.glm               RankSeason.my.fctr, W, RankSeason.my.fctr:RS, RankSeason.my.fctr:RankSeason.my.fctr
## Low.cor.X.glm                                                 W, RS, Year, League.fctr, .rnorm, G, RA, RankSeason.my.fctr
## All.X.glm                 W, RS, OBP, SLG, BA, Year, League.fctr, .rnorm, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
## All.X.bayesglm            W, RS, OBP, SLG, BA, Year, League.fctr, .rnorm, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
## All.X.no.rnorm.rpart              W, RS, OBP, SLG, BA, Year, League.fctr, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
## All.X.no.rnorm.rf                 W, RS, OBP, SLG, BA, Year, League.fctr, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.379
## Random.myrandom_classfr                 0                      0.229
## Max.cor.Y.cv.0.rpart                    0                      0.625
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.501
## Max.cor.Y.rpart                         3                      1.094
## Max.cor.Y.glm                           1                      1.819
## Interact.High.cor.Y.glm                 1                      1.373
## Low.cor.X.glm                           1                      1.323
## All.X.glm                               1                      1.231
## All.X.bayesglm                          1                      2.075
## All.X.no.rnorm.rpart                    3                      1.153
## All.X.no.rnorm.rf                       3                      4.564
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.002   0.5000000
## Random.myrandom_classfr                   0.002   0.4930999
## Max.cor.Y.cv.0.rpart                      0.030   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.029   1.0000000
## Max.cor.Y.rpart                           0.029   1.0000000
## Max.cor.Y.glm                             0.067   1.0000000
## Interact.High.cor.Y.glm                   0.088   1.0000000
## Low.cor.X.glm                             0.084   1.0000000
## All.X.glm                                 0.110   1.0000000
## All.X.bayesglm                            0.186   1.0000000
## All.X.no.rnorm.rpart                      0.059   1.0000000
## All.X.no.rnorm.rf                         0.817   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.3307544
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    1.0       1.0000000
## Max.cor.Y.rpart                              1.0       1.0000000
## Max.cor.Y.glm                                0.9       1.0000000
## Interact.High.cor.Y.glm                      0.9       1.0000000
## Low.cor.X.glm                                0.9       1.0000000
## All.X.glm                                    0.9       1.0000000
## All.X.bayesglm                               0.9       1.0000000
## All.X.no.rnorm.rpart                         1.0       1.0000000
## All.X.no.rnorm.rf                            0.8       1.0000000
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                 0.801854             0.7736689
## Random.myrandom_classfr           0.198146             0.1720399
## Max.cor.Y.cv.0.rpart              0.801854             0.7736689
## Max.cor.Y.cv.0.cp.0.rpart         1.000000             0.9957346
## Max.cor.Y.rpart                   1.000000             0.9957346
## Max.cor.Y.glm                     1.000000             0.9957346
## Interact.High.cor.Y.glm           1.000000             0.9957346
## Low.cor.X.glm                     1.000000             0.9957346
## All.X.glm                         1.000000             0.9957346
## All.X.bayesglm                    1.000000             0.9957346
## All.X.no.rnorm.rpart              1.000000             0.9957346
## All.X.no.rnorm.rf                 1.000000             0.9957346
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.8279601             0   0.5000000
## Random.myrandom_classfr               0.2263311             0   0.4959506
## Max.cor.Y.cv.0.rpart                  0.8279601             0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             1.0000000             1   1.0000000
## Max.cor.Y.rpart                       1.0000000             1   1.0000000
## Max.cor.Y.glm                         1.0000000             1   1.0000000
## Interact.High.cor.Y.glm               1.0000000             1   1.0000000
## Low.cor.X.glm                         1.0000000             1   1.0000000
## All.X.glm                             1.0000000             1   1.0000000
## All.X.bayesglm                        1.0000000             1   1.0000000
## All.X.no.rnorm.rpart                  1.0000000             1   1.0000000
## All.X.no.rnorm.rf                     1.0000000             1   1.0000000
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.3303167
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    1.0       1.0000000
## Max.cor.Y.rpart                              1.0       1.0000000
## Max.cor.Y.glm                                0.9       1.0000000
## Interact.High.cor.Y.glm                      0.9       1.0000000
## Low.cor.X.glm                                0.9       1.0000000
## All.X.glm                                    0.9       1.0000000
## All.X.bayesglm                               0.9       1.0000000
## All.X.no.rnorm.rpart                         1.0       1.0000000
## All.X.no.rnorm.rf                            0.7       1.0000000
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                 0.802168             0.7578069
## Random.myrandom_classfr           0.197832             0.1583971
## Max.cor.Y.cv.0.rpart              0.802168             0.7578069
## Max.cor.Y.cv.0.cp.0.rpart         1.000000             0.9900528
## Max.cor.Y.rpart                   1.000000             0.9900528
## Max.cor.Y.glm                     1.000000             0.9900528
## Interact.High.cor.Y.glm           1.000000             0.9900528
## Low.cor.X.glm                     1.000000             0.9900528
## All.X.glm                         1.000000             0.9900528
## All.X.bayesglm                    1.000000             0.9900528
## All.X.no.rnorm.rpart              1.000000             0.9900528
## All.X.no.rnorm.rf                 1.000000             0.9900528
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.8416029             0
## Random.myrandom_classfr               0.2421931             0
## Max.cor.Y.cv.0.rpart                  0.8416029             0
## Max.cor.Y.cv.0.cp.0.rpart             1.0000000             1
## Max.cor.Y.rpart                       1.0000000             1
## Max.cor.Y.glm                         1.0000000             1
## Interact.High.cor.Y.glm               1.0000000             1
## Low.cor.X.glm                         1.0000000             1
## All.X.glm                             1.0000000             1
## All.X.bayesglm                        1.0000000             1
## All.X.no.rnorm.rpart                  1.0000000             1
## All.X.no.rnorm.rf                     1.0000000             1
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                            0               0          NA
## Max.cor.Y.glm                              0               0    20.00000
## Interact.High.cor.Y.glm                    0               0    36.00000
## Low.cor.X.glm                              0               0    32.00000
## All.X.glm                                  0               0    46.00000
## All.X.bayesglm                             0               0    48.71789
## All.X.no.rnorm.rpart                       0               0          NA
## All.X.no.rnorm.rf                          0               0          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5  fit.models_1_rf          5          0 61.964 69.593   7.629
## 6 fit.models_1_end          6          0 69.593     NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 11 fit.models          7          1 44.667 69.599  24.933
## 12 fit.models          7          2 69.600     NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
#     tmp_models_df <- orderBy(~model_id, glb_models_df)
#     rownames(tmp_models_df) <- seq(1, nrow(tmp_models_df))
#     all.equal(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr"),
#               subset(stats_df, model_id != "Random.myrandom_classfr"))
#     print(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])
#     print(subset(stats_df, model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])

    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id", grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df), grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
##                                                                                                                     feats
## MFO.myMFO_classfr                                                                                                  .rnorm
## Random.myrandom_classfr                                                                                            .rnorm
## Max.cor.Y.cv.0.rpart                                                                                RankSeason.my.fctr, W
## Max.cor.Y.cv.0.cp.0.rpart                                                                           RankSeason.my.fctr, W
## Max.cor.Y.rpart                                                                                     RankSeason.my.fctr, W
## Max.cor.Y.glm                                                                                       RankSeason.my.fctr, W
## Interact.High.cor.Y.glm               RankSeason.my.fctr, W, RankSeason.my.fctr:RS, RankSeason.my.fctr:RankSeason.my.fctr
## Low.cor.X.glm                                                 W, RS, Year, League.fctr, .rnorm, G, RA, RankSeason.my.fctr
## All.X.glm                 W, RS, OBP, SLG, BA, Year, League.fctr, .rnorm, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
## All.X.bayesglm            W, RS, OBP, SLG, BA, Year, League.fctr, .rnorm, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
## All.X.no.rnorm.rpart              W, RS, OBP, SLG, BA, Year, League.fctr, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
## All.X.no.rnorm.rf                 W, RS, OBP, SLG, BA, Year, League.fctr, G, RA, RankPlayoffs.my.fctr, RankSeason.my.fctr
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.4930999
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   1.0000000
## Max.cor.Y.rpart                         3   1.0000000
## Max.cor.Y.glm                           1   1.0000000
## Interact.High.cor.Y.glm                 1   1.0000000
## Low.cor.X.glm                           1   1.0000000
## All.X.glm                               1   1.0000000
## All.X.bayesglm                          1   1.0000000
## All.X.no.rnorm.rpart                    3   1.0000000
## All.X.no.rnorm.rf                       3   1.0000000
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.3307544
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    1.0       1.0000000
## Max.cor.Y.rpart                              1.0       1.0000000
## Max.cor.Y.glm                                0.9       1.0000000
## Interact.High.cor.Y.glm                      0.9       1.0000000
## Low.cor.X.glm                                0.9       1.0000000
## All.X.glm                                    0.9       1.0000000
## All.X.bayesglm                               0.9       1.0000000
## All.X.no.rnorm.rpart                         1.0       1.0000000
## All.X.no.rnorm.rf                            0.8       1.0000000
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                 0.801854             0   0.5000000
## Random.myrandom_classfr           0.198146             0   0.4959506
## Max.cor.Y.cv.0.rpart              0.801854             0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart         1.000000             1   1.0000000
## Max.cor.Y.rpart                   1.000000             1   1.0000000
## Max.cor.Y.glm                     1.000000             1   1.0000000
## Interact.High.cor.Y.glm           1.000000             1   1.0000000
## Low.cor.X.glm                     1.000000             1   1.0000000
## All.X.glm                         1.000000             1   1.0000000
## All.X.bayesglm                    1.000000             1   1.0000000
## All.X.no.rnorm.rpart              1.000000             1   1.0000000
## All.X.no.rnorm.rf                 1.000000             1   1.0000000
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.3303167
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    1.0       1.0000000
## Max.cor.Y.rpart                              1.0       1.0000000
## Max.cor.Y.glm                                0.9       1.0000000
## Interact.High.cor.Y.glm                      0.9       1.0000000
## Low.cor.X.glm                                0.9       1.0000000
## All.X.glm                                    0.9       1.0000000
## All.X.bayesglm                               0.9       1.0000000
## All.X.no.rnorm.rpart                         1.0       1.0000000
## All.X.no.rnorm.rf                            0.7       1.0000000
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                 0.802168             0
## Random.myrandom_classfr           0.197832             0
## Max.cor.Y.cv.0.rpart              0.802168             0
## Max.cor.Y.cv.0.cp.0.rpart         1.000000             1
## Max.cor.Y.rpart                   1.000000             1
## Max.cor.Y.glm                     1.000000             1
## Interact.High.cor.Y.glm           1.000000             1
## Low.cor.X.glm                     1.000000             1
## All.X.glm                         1.000000             1
## All.X.bayesglm                    1.000000             1
## All.X.no.rnorm.rpart              1.000000             1
## All.X.no.rnorm.rf                 1.000000             1
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                          2.6385224            500.000000
## Random.myrandom_classfr                    4.3668122            500.000000
## Max.cor.Y.cv.0.rpart                       1.6000000             33.333333
## Max.cor.Y.cv.0.cp.0.rpart                  1.9960080             34.482759
## Max.cor.Y.rpart                            0.9140768             34.482759
## Max.cor.Y.glm                              0.5497526             14.925373
## Interact.High.cor.Y.glm                    0.7283321             11.363636
## Low.cor.X.glm                              0.7558579             11.904762
## All.X.glm                                  0.8123477              9.090909
## All.X.bayesglm                             0.4819277              5.376344
## All.X.no.rnorm.rpart                       0.8673027             16.949153
## All.X.no.rnorm.rf                          0.2191060              1.223990
##                           inv.aic.fit
## MFO.myMFO_classfr                  NA
## Random.myrandom_classfr            NA
## Max.cor.Y.cv.0.rpart               NA
## Max.cor.Y.cv.0.cp.0.rpart          NA
## Max.cor.Y.rpart                    NA
## Max.cor.Y.glm              0.05000000
## Interact.High.cor.Y.glm    0.02777778
## Low.cor.X.glm              0.03125000
## All.X.glm                  0.02173913
## All.X.bayesglm             0.02052634
## All.X.no.rnorm.rpart               NA
## All.X.no.rnorm.rf                  NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 4 rows containing missing values
## (geom_path).
```

```
## Warning in loop_apply(n, do.ply): Removed 87 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually. if you must have them.
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
#print(mltdCI_models_df)
# castCI_models_df <- dcast(mltdCI_models_df, value ~ type, fun.aggregate=sum)
# print(castCI_models_df)
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 6              Max.cor.Y.glm         1.000000   1.0000000             1
## 8              Low.cor.X.glm         1.000000   1.0000000             1
## 7    Interact.High.cor.Y.glm         1.000000   1.0000000             1
## 9                  All.X.glm         1.000000   1.0000000             1
## 10            All.X.bayesglm         1.000000   1.0000000             1
## 4  Max.cor.Y.cv.0.cp.0.rpart         1.000000   1.0000000             1
## 5            Max.cor.Y.rpart         1.000000   1.0000000             1
## 11      All.X.no.rnorm.rpart         1.000000   1.0000000             1
## 12         All.X.no.rnorm.rf         1.000000   1.0000000             1
## 1          MFO.myMFO_classfr         0.802168   0.5000000             0
## 3       Max.cor.Y.cv.0.rpart         0.802168   0.5000000             0
## 2    Random.myrandom_classfr         0.197832   0.4959506             0
##    min.aic.fit opt.prob.threshold.OOB
## 6     20.00000                    0.9
## 8     32.00000                    0.9
## 7     36.00000                    0.9
## 9     46.00000                    0.9
## 10    48.71789                    0.9
## 4           NA                    1.0
## 5           NA                    1.0
## 11          NA                    1.0
## 12          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 2           NA                    0.1
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 38 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 12. Consider specifying shapes manually. if you must have them.
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.auc.OOB - max.Kappa.OOB + min.aic.fit - 
##     opt.prob.threshold.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: Max.cor.Y.glm"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
    if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
        warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
        glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
    }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-4.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-5.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-6.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##        Min          1Q      Median          3Q         Max  
## -2.409e-06  -2.409e-06  -2.409e-06  -2.409e-06   2.409e-06  
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)
## (Intercept)              2.657e+01  1.478e+05   0.000    1.000
## RankSeason.my.fctr2      2.189e-07  8.588e+04   0.000    1.000
## RankSeason.my.fctr3      2.026e-07  8.692e+04   0.000    1.000
## RankSeason.my.fctr4      4.647e-06  8.215e+04   0.000    1.000
## RankSeason.my.fctr5      4.850e-06  1.118e+05   0.000    1.000
## RankSeason.my.fctr6      1.977e-07  1.225e+05   0.000    1.000
## RankSeason.my.fctr7      2.106e-07  1.474e+05   0.000    1.000
## RankSeason.my.fctr8      7.264e-09  3.611e+05   0.000    1.000
## RankSeason.my.fctrNA.my -5.313e+01  6.687e+04  -0.001    0.999
## W                       -3.179e-15  1.352e+03   0.000    1.000
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 8.5924e+02  on 862  degrees of freedom
## Residual deviance: 5.0068e-09  on 853  degrees of freedom
## AIC: 20
## 
## Number of Fisher Scoring iterations: 25
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                           importance Max.cor.Y.glm.importance
## RankSeason.my.fctrNA.my 1.000000e+02             1.000000e+02
## RankSeason.my.fctr4     7.119420e-06             7.119420e-06
## RankSeason.my.fctr5     5.458865e-06             5.458865e-06
## RankSeason.my.fctr2     3.208167e-07             3.208167e-07
## RankSeason.my.fctr3     2.933364e-07             2.933364e-07
## RankSeason.my.fctr6     2.030839e-07             2.030839e-07
## RankSeason.my.fctr7     1.798635e-07             1.798635e-07
## RankSeason.my.fctr8     2.531894e-09             2.531894e-09
## W                       0.000000e+00             0.000000e+00
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-8.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames Playoffs.fctr Playoffs.fctr.predict.Max.cor.Y.glm.prob
## 1206      1206             N                             2.900701e-12
## 355        355             Y                             1.000000e+00
##      Playoffs.fctr.predict.Max.cor.Y.glm
## 1206                                   N
## 355                                    Y
##      Playoffs.fctr.predict.Max.cor.Y.glm.accurate
## 1206                                         TRUE
## 355                                          TRUE
##      Playoffs.fctr.predict.Max.cor.Y.glm.error .label
## 1206                                         0   1206
## 355                                          0    355
## [1] "Inaccurate: "
## [1] .rownames                                   
## [2] Playoffs.fctr                               
## [3] Playoffs.fctr.predict.Max.cor.Y.glm.prob    
## [4] Playoffs.fctr.predict.Max.cor.Y.glm         
## [5] Playoffs.fctr.predict.Max.cor.Y.glm.accurate
## [6] Playoffs.fctr.predict.Max.cor.Y.glm.error   
## <0 rows> (or 0-length row.names)
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_2-10.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
FN_OOB_ids <- c(4721, 4020, 693, 92)
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
```

```
## [1] Playoffs.fctr                               
## [2] Playoffs.fctr.predict.Max.cor.Y.glm.prob    
## [3] Playoffs.fctr.predict.Max.cor.Y.glm         
## [4] Playoffs.fctr.predict.Max.cor.Y.glm.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
## [1] Playoffs W        RS       OBP      SLG     
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
## data frame with 0 columns and 0 rows
```

```r
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 12 fit.models          7          2 69.600 82.676  13.077
## 13 fit.models          7          3 82.677     NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "Playoffs.fctr.predict.Max.cor.Y.glm.prob"    
## [2] "Playoffs.fctr.predict.Max.cor.Y.glm"         
## [3] "Playoffs.fctr.predict.Max.cor.Y.glm.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](MoneyBall_Playoffs2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 13        fit.models          7          3 82.677 86.839   4.162
## 14 fit.data.training          8          0 86.839     NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.glm"
## [1] "    indep_vars: RankSeason.my.fctr, W"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: algorithm did not converge
```

![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_0-1.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_0-2.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_0-3.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##        Min          1Q      Median          3Q         Max  
## -2.409e-06  -2.409e-06  -2.409e-06  -2.409e-06   2.409e-06  
## 
## Coefficients:
##                           Estimate Std. Error z value Pr(>|z|)
## (Intercept)              2.657e+01  1.478e+05   0.000    1.000
## RankSeason.my.fctr2      2.189e-07  8.588e+04   0.000    1.000
## RankSeason.my.fctr3      2.026e-07  8.692e+04   0.000    1.000
## RankSeason.my.fctr4      4.647e-06  8.215e+04   0.000    1.000
## RankSeason.my.fctr5      4.850e-06  1.118e+05   0.000    1.000
## RankSeason.my.fctr6      1.977e-07  1.225e+05   0.000    1.000
## RankSeason.my.fctr7      2.106e-07  1.474e+05   0.000    1.000
## RankSeason.my.fctr8      7.264e-09  3.611e+05   0.000    1.000
## RankSeason.my.fctrNA.my -5.313e+01  6.687e+04  -0.001    0.999
## W                       -3.179e-15  1.352e+03   0.000    1.000
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 8.5924e+02  on 862  degrees of freedom
## Residual deviance: 5.0068e-09  on 853  degrees of freedom
## AIC: 20
## 
## Number of Fisher Scoring iterations: 25
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_0-5.png) 

```
##    threshold   f.score
## 1        0.0 0.3307544
## 2        0.1 1.0000000
## 3        0.2 1.0000000
## 4        0.3 1.0000000
## 5        0.4 1.0000000
## 6        0.5 1.0000000
## 7        0.6 1.0000000
## 8        0.7 1.0000000
## 9        0.8 1.0000000
## 10       0.9 1.0000000
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Playoffs.fctr Playoffs.fctr.predict.Final.glm.N
## 1             N                               692
## 2             Y                                NA
##   Playoffs.fctr.predict.Final.glm.Y
## 1                                NA
## 2                               171
##          Prediction
## Reference   N   Y
##         N 692   0
##         Y   0 171
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   1.000000e+00   1.000000e+00   9.957346e-01   1.000000e+00   8.018540e-01 
## AccuracyPValue  McnemarPValue 
##   1.714916e-83            NaN
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_0-6.png) 

```
##    model_id model_method                 feats max.nTuningRuns
## 1 Final.glm          glm RankSeason.my.fctr, W               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.074                 0.058           1
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.9               1                1
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit min.aic.fit
## 1             0.9957346                     1             1          20
##   max.AccuracySD.fit max.KappaSD.fit
## 1                  0               0
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 14 fit.data.training          8          0 86.839 91.893   5.054
## 15 fit.data.training          8          1 91.894     NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.9
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                         Max.cor.Y.glm.importance   importance
## RankSeason.my.fctrNA.my             1.000000e+02 1.000000e+02
## RankSeason.my.fctr4                 7.119420e-06 7.119420e-06
## RankSeason.my.fctr5                 5.458865e-06 5.458865e-06
## RankSeason.my.fctr2                 3.208167e-07 3.208167e-07
## RankSeason.my.fctr3                 2.933364e-07 2.933364e-07
## RankSeason.my.fctr6                 2.030839e-07 2.030839e-07
## RankSeason.my.fctr7                 1.798635e-07 1.798635e-07
## RankSeason.my.fctr8                 2.531894e-09 2.531894e-09
## W                                   0.000000e+00 0.000000e+00
##                         Final.glm.importance
## RankSeason.my.fctrNA.my         1.000000e+02
## RankSeason.my.fctr4             7.119420e-06
## RankSeason.my.fctr5             5.458865e-06
## RankSeason.my.fctr2             3.208167e-07
## RankSeason.my.fctr3             2.933364e-07
## RankSeason.my.fctr6             2.030839e-07
## RankSeason.my.fctr7             1.798635e-07
## RankSeason.my.fctr8             2.531894e-09
## W                               0.000000e+00
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_1-1.png) ![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_1-2.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames Playoffs.fctr Playoffs.fctr.predict.Final.glm.prob
## 1046      1046             Y                         1.000000e+00
## 1226      1226             N                         2.900701e-12
##      Playoffs.fctr.predict.Final.glm
## 1046                               Y
## 1226                               N
##      Playoffs.fctr.predict.Final.glm.accurate
## 1046                                     TRUE
## 1226                                     TRUE
##      Playoffs.fctr.predict.Final.glm.error .label
## 1046                                     0   1046
## 1226                                     0   1226
## [1] "Inaccurate: "
## [1] .rownames                               
## [2] Playoffs.fctr                           
## [3] Playoffs.fctr.predict.Final.glm.prob    
## [4] Playoffs.fctr.predict.Final.glm         
## [5] Playoffs.fctr.predict.Final.glm.accurate
## [6] Playoffs.fctr.predict.Final.glm.error   
## <0 rows> (or 0-length row.names)
```

![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_1-3.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])
```

```
## [1] Playoffs.fctr                       
## [2] Playoffs.fctr.predict.Final.glm.prob
## [3] Playoffs.fctr.predict.Final.glm     
## <0 rows> (or 0-length row.names)
```

```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "Playoffs.fctr.predict.Final.glm.prob"
## [2] "Playoffs.fctr.predict.Final.glm"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](MoneyBall_Playoffs2_files/figure-html/fit.data.training_1-4.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 15 fit.data.training          8          1 91.894 95.216   3.323
## 16  predict.data.new          9          0 95.217     NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newobs_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.9
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](MoneyBall_Playoffs2_files/figure-html/predict.data.new-1.png) ![](MoneyBall_Playoffs2_files/figure-html/predict.data.new-2.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames Playoffs.fctr Playoffs.fctr.predict.Final.glm.prob
## 1206      1206             N                         2.900701e-12
## 355        355             Y                         1.000000e+00
##      Playoffs.fctr.predict.Final.glm
## 1206                               N
## 355                                Y
##      Playoffs.fctr.predict.Final.glm.accurate
## 1206                                     TRUE
## 355                                      TRUE
##      Playoffs.fctr.predict.Final.glm.error .label
## 1206                                     0   1206
## 355                                      0    355
## [1] "Inaccurate: "
## [1] .rownames                               
## [2] Playoffs.fctr                           
## [3] Playoffs.fctr.predict.Final.glm.prob    
## [4] Playoffs.fctr.predict.Final.glm         
## [5] Playoffs.fctr.predict.Final.glm.accurate
## [6] Playoffs.fctr.predict.Final.glm.error   
## <0 rows> (or 0-length row.names)
```

![](MoneyBall_Playoffs2_files/figure-html/predict.data.new-3.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
write.csv(submit_df, 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
           "_submit.csv"), row.names=FALSE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
```

```
## [1] 0.9
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Max.cor.Y.glm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.glm"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 863  23
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 6              Max.cor.Y.glm         1.000000   1.0000000             1
## 8              Low.cor.X.glm         1.000000   1.0000000             1
## 7    Interact.High.cor.Y.glm         1.000000   1.0000000             1
## 9                  All.X.glm         1.000000   1.0000000             1
## 10            All.X.bayesglm         1.000000   1.0000000             1
## 4  Max.cor.Y.cv.0.cp.0.rpart         1.000000   1.0000000             1
## 5            Max.cor.Y.rpart         1.000000   1.0000000             1
## 11      All.X.no.rnorm.rpart         1.000000   1.0000000             1
## 12         All.X.no.rnorm.rf         1.000000   1.0000000             1
## 1          MFO.myMFO_classfr         0.802168   0.5000000             0
## 3       Max.cor.Y.cv.0.rpart         0.802168   0.5000000             0
## 2    Random.myrandom_classfr         0.197832   0.4959506             0
##    min.aic.fit opt.prob.threshold.OOB
## 6     20.00000                    0.9
## 8     32.00000                    0.9
## 7     36.00000                    0.9
## 9     46.00000                    0.9
## 10    48.71789                    0.9
## 4           NA                    1.0
## 5           NA                    1.0
## 11          NA                    1.0
## 12          NA                    0.7
## 1           NA                    0.5
## 3           NA                    0.5
## 2           NA                    0.1
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_vars)) {
        stop("not implemented yet")
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
            pred_stats_df <- 
                mypredict_mdl(mdl=glb_models_lst[[glb_fin_mdl_id]], 
                              df=glb_newobs_df, 
                              rsp_var=glb_rsp_var, 
                              rsp_var_out=glb_rsp_var_out, 
                              model_id_method=glb_fin_mdl_id, 
                              label="new",
						      model_summaryFunction=glb_sel_mdl$control$summaryFunction, 
						      model_metric=glb_sel_mdl$metric,
						      model_metric_maximize=glb_sel_mdl$maximize,
						      ret_type="stats")        
            print(sprintf("%s prediction stats for glb_newobs_df:", glb_fin_mdl_id))
            print(pred_stats_df)
    }    
}    
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
}    
```

```
## [1] "Max.cor.Y.glm OOB confusion matrix & accuracy: "
##          Prediction
## Reference   N   Y
##         N 296   0
##         Y   0  73
```

```r
dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

if (glb_is_classification) {
    print("FN_OOB_ids:")
    print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        glb_txt_vars])
    print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_OOBobs_df),
                    grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
}
```

```
## [1] "FN_OOB_ids:"
## [1] Playoffs.fctr                               
## [2] Playoffs.fctr.predict.Max.cor.Y.glm.prob    
## [3] Playoffs.fctr.predict.Max.cor.Y.glm         
## [4] Playoffs.fctr.predict.Max.cor.Y.glm.accurate
## <0 rows> (or 0-length row.names)
## data frame with 0 columns and 0 rows
##        SLG RankSeason       OSLG 
##          0          0          0
```

```r
dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##                         Max.cor.Y.glm.importance   importance
## RankSeason.my.fctrNA.my             1.000000e+02 1.000000e+02
## RankSeason.my.fctr4                 7.119420e-06 7.119420e-06
## RankSeason.my.fctr5                 5.458865e-06 5.458865e-06
## RankSeason.my.fctr2                 3.208167e-07 3.208167e-07
## RankSeason.my.fctr3                 2.933364e-07 2.933364e-07
## RankSeason.my.fctr6                 2.030839e-07 2.030839e-07
## RankSeason.my.fctr7                 1.798635e-07 1.798635e-07
## RankSeason.my.fctr8                 2.531894e-09 2.531894e-09
## W                                   0.000000e+00 0.000000e+00
##                         Final.glm.importance
## RankSeason.my.fctrNA.my         1.000000e+02
## RankSeason.my.fctr4             7.119420e-06
## RankSeason.my.fctr5             5.458865e-06
## RankSeason.my.fctr2             3.208167e-07
## RankSeason.my.fctr3             2.933364e-07
## RankSeason.my.fctr6             2.030839e-07
## RankSeason.my.fctr7             1.798635e-07
## RankSeason.my.fctr8             2.531894e-09
## W                               0.000000e+00
```

```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor    bgn    end elapsed
## 16     predict.data.new          9          0 95.217 97.465   2.248
## 17 display.session.info         10          0 97.465     NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor    bgn    end elapsed
## 11              fit.models          7          1 44.667 69.599  24.933
## 10              fit.models          7          0 19.817 44.667  24.850
## 12              fit.models          7          2 69.600 82.676  13.077
## 2             inspect.data          2          0  8.085 15.449   7.364
## 14       fit.data.training          8          0 86.839 91.893   5.054
## 13              fit.models          7          3 82.677 86.839   4.162
## 15       fit.data.training          8          1 91.894 95.216   3.323
## 16        predict.data.new          9          0 95.217 97.465   2.248
## 3               scrub.data          2          1 15.450 16.977   1.527
## 6         extract.features          3          0 17.109 18.484   1.375
## 8          select.features          5          0 18.778 19.491   0.713
## 1              import.data          1          0  7.652  8.084   0.432
## 9  partition.data.training          6          0 19.491 19.816   0.325
## 7             cluster.data          4          0 18.485 18.778   0.293
## 5      manage.missing.data          2          3 17.018 17.108   0.090
## 4           transform.data          2          2 16.977 17.018   0.041
##    duration
## 11   24.932
## 10   24.850
## 12   13.076
## 2     7.364
## 14    5.054
## 13    4.162
## 15    3.322
## 16    2.248
## 3     1.527
## 6     1.375
## 8     0.713
## 1     0.432
## 9     0.325
## 7     0.293
## 5     0.090
## 4     0.041
## [1] "Total Elapsed Time: 97.465 secs"
```

![](MoneyBall_Playoffs2_files/figure-html/display.session.info-1.png) 

```
##                   label step_major step_minor    bgn    end elapsed
## 5       fit.models_1_rf          5          0 61.964 69.593   7.629
## 3 fit.models_1_bayesglm          3          0 52.609 57.446   4.837
## 4    fit.models_1_rpart          4          0 57.447 61.964   4.517
## 2      fit.models_1_glm          2          0 48.178 52.608   4.431
## 1      fit.models_1_bgn          1          0 48.163 48.178   0.015
##   duration
## 5    7.629
## 3    4.837
## 4    4.517
## 2    4.430
## 1    0.015
## [1] "Total Elapsed Time: 69.593 secs"
```

![](MoneyBall_Playoffs2_files/figure-html/display.session.info-2.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 arm_1.8-5          
##  [4] lme4_1.1-7          Rcpp_0.11.6         Matrix_1.2-1       
##  [7] MASS_7.3-40         rpart.plot_1.5.2    rpart_4.1-9        
## [10] ROCR_1.0-7          gplots_2.17.0       dplyr_0.4.1        
## [13] plyr_1.8.2          sqldf_0.4-10        RSQLite_1.0.0      
## [16] DBI_0.3.1           gsubfn_0.6-6        proto_0.3-10       
## [19] reshape2_1.4.1      caTools_1.17.1      doMC_1.3.3         
## [22] iterators_1.0.7     foreach_1.4.2       doBy_4.5-13        
## [25] survival_2.38-1     caret_6.0-47        ggplot2_1.0.1      
## [28] lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] class_7.3-12        gtools_3.5.0        assertthat_0.1     
##  [4] digest_0.6.8        BradleyTerry2_1.0-6 chron_2.3-45       
##  [7] evaluate_0.7        coda_0.17-1         e1071_1.6-4        
## [10] lazyeval_0.1.10     minqa_1.2.4         SparseM_1.6        
## [13] car_2.0-25          nloptr_1.0.4        rmarkdown_0.6.1    
## [16] labeling_0.3        splines_3.2.0       stringr_1.0.0      
## [19] munsell_0.4.2       compiler_3.2.0      mgcv_1.8-6         
## [22] htmltools_0.2.6     nnet_7.3-9          codetools_0.2-11   
## [25] brglm_0.5-9         bitops_1.0-6        nlme_3.1-120       
## [28] gtable_0.1.2        magrittr_1.5        formatR_1.2        
## [31] scales_0.2.4        KernSmooth_2.23-14  stringi_0.4-1      
## [34] RColorBrewer_1.1-2  tools_3.2.0         abind_1.4-3        
## [37] pbkrtest_0.4-2      yaml_2.1.13         colorspace_1.2-6   
## [40] knitr_1.10.5        quantreg_5.11
```
