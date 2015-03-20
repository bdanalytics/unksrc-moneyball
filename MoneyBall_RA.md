# MoneyBall: RA(Runs Allowed) regression
bdanalytics  

**  **    
**Date: (Fri) Mar 20, 2015**    

# Introduction:  

Data: 
Source: https://courses.edx.org/c4x/MITx/15.071x_2/asset/baseball.csv   
    Training:   Year < 2002  
    New:        Year >= 2002  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(reshape2))

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_predict_dataset <- FALSE    # or TRUE
glb_predct_var <- "RA"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("Team", "Year")                    # or NULL

# List vars that are missing in glb_entity_df
glb_exclude_vars_as_features <- c("RankSeason", "RankPlayoffs") # or NULL
# List chrs converted into factors 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("Team", "League", 
                                        "Team.fctr")    # or NULL
                                      )
# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("Playoffs", "W", "Runs.diff", 
                                        "RS", "OBP", "SLG", "BA")     # or NULL
                                      )

glb_is_regression <- TRUE; glb_is_classification <- !glb_is_regression

glb_mdl <- glb_sel_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_all_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/baseball.csv", 
    comment="glb_entity_all_df", print_diagn=TRUE)
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
##      Team League Year  RS  RA   W   OBP   SLG    BA Playoffs RankSeason
## 205   SFG     NL 2006 746 790  76 0.324 0.422 0.259        0         NA
## 561   ATL     NL 1991 749 644  94 0.328 0.393 0.258        1          3
## 889   NYY     AL 1978 735 582 100 0.329 0.388 0.267        1          1
## 937   MIL     AL 1976 570 655  66 0.311 0.340 0.246        0         NA
## 1079  KCR     AL 1969 586 688  69 0.309 0.338 0.240        0         NA
## 1090  SFG     NL 1969 713 636  90 0.334 0.361 0.242        0         NA
##      RankPlayoffs   G  OOBP  OSLG
## 205            NA 161 0.337 0.415
## 561             2 162    NA    NA
## 889             1 163    NA    NA
## 937            NA 161    NA    NA
## 1079           NA 163    NA    NA
## 1090           NA 162    NA    NA
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
##  - attr(*, "comment")= chr "glb_entity_all_df"
## NULL
```

```r
glb_entity_df <- subset(glb_entity_all_df, Year < 2002)
comment(glb_entity_df) <- "glb_entity_df"; myprint_df(glb_entity_df)
```

```
##     Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 331  ANA     AL 2001 691 730 75 0.327 0.405 0.261        0         NA
## 332  ARI     NL 2001 818 677 92 0.341 0.442 0.267        1          5
## 333  ATL     NL 2001 729 643 88 0.324 0.412 0.260        1          7
## 334  BAL     AL 2001 687 829 63 0.319 0.380 0.248        0         NA
## 335  BOS     AL 2001 772 745 82 0.334 0.439 0.266        0         NA
## 336  CHC     NL 2001 777 701 88 0.336 0.430 0.261        0         NA
##     RankPlayoffs   G  OOBP  OSLG
## 331           NA 162 0.331 0.412
## 332            1 162 0.311 0.404
## 333            3 162 0.314 0.384
## 334           NA 162 0.337 0.439
## 335           NA 161 0.329 0.393
## 336           NA 162 0.321 0.398
##      Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 362   ARI     NL 2000 792 754 85 0.333 0.429 0.265        0         NA
## 467   MON     NL 1997 691 740 78 0.316 0.425 0.258        0         NA
## 624   LAD     NL 1989 554 536 77 0.306 0.339 0.240        0         NA
## 789   SDP     NL 1983 653 653 81 0.311 0.351 0.250        0         NA
## 985   MIL     AL 1974 647 660 76 0.309 0.369 0.244        0         NA
## 1220  HOU     NL 1962 592 717 64 0.310 0.351 0.246        0         NA
##      RankPlayoffs   G  OOBP  OSLG
## 362            NA 162 0.326 0.424
## 467            NA 162    NA    NA
## 624            NA 160    NA    NA
## 789            NA 163    NA    NA
## 985            NA 162    NA    NA
## 1220           NA 162    NA    NA
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
```

```r
# if (glb_is_separate_predict_dataset) {
#     glb_predct_df <- myimport_data(
#         url="<prdct_url>", 
#         comment="glb_predct_df", print_diagn=TRUE)
# } else {
#     glb_predct_df <- glb_entity_df[sample(1:nrow(glb_entity_df), nrow(glb_entity_df) / 1000),]
    glb_predct_df <- subset(glb_entity_all_df, Year >= 2002)
    comment(glb_predct_df) <- "glb_predct_df"; myprint_df(glb_predct_df)
```

```
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
##     Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 1    ARI     NL 2012 734 688 81 0.328 0.418 0.259        0         NA
## 127  CIN     NL 2008 704 800 74 0.321 0.408 0.247        0         NA
## 129  COL     NL 2008 747 822 74 0.336 0.415 0.263        0         NA
## 131  FLA     NL 2008 770 767 84 0.326 0.433 0.254        0         NA
## 152  ATL     NL 2007 810 733 84 0.339 0.435 0.275        0         NA
## 243  ATL     NL 2004 803 668 96 0.343 0.434 0.270        1          4
##     RankPlayoffs   G  OOBP  OSLG
## 1             NA 162 0.317 0.415
## 127           NA 162 0.345 0.450
## 129           NA 162 0.344 0.431
## 131           NA 161 0.333 0.407
## 152           NA 162 0.327 0.415
## 243            4 162 0.329 0.400
##     Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 325  SEA     AL 2002 814 699 93 0.350 0.419 0.275        0         NA
## 326  SFG     NL 2002 783 616 95 0.344 0.442 0.267        1          6
## 327  STL     NL 2002 787 648 97 0.338 0.425 0.268        1          5
## 328  TBD     AL 2002 673 918 55 0.314 0.390 0.253        0         NA
## 329  TEX     AL 2002 843 882 72 0.338 0.455 0.269        0         NA
## 330  TOR     AL 2002 813 828 78 0.327 0.430 0.261        0         NA
##     RankPlayoffs   G  OOBP  OSLG
## 325           NA 162 0.315 0.410
## 326            2 162 0.319 0.372
## 327            3 162 0.323 0.386
## 328           NA 161 0.357 0.463
## 329           NA 162 0.355 0.451
## 330           NA 162 0.344 0.431
```

```r
# }         

script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
#   Playoffs:       1 if the team made it in that year / 0 if not
#   Wins:           # of total wins
#   RS:             Runs scored
#   RA:             Runs allowed
#   <Runs.diff>:    RS - RA
#   OBP:            on-base % (incl. walks)
#   SLG:            slugging %
#   BA:             batting average (excl. walks)
#   OOBP:           Opponent's OBP: many obs with NAs
#   OSLG:           Opponent's SLG: many obs with NAs

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Potential Enhancements:
#       One code chunk to cycle thru glb_entity_df & glb_predct_df ?
#           Use with / within ?
#           for (df in c(glb_entity_df, glb_predct_df)) cycles thru column names
#           for (df in list(glb_entity_df, glb_predct_df)) does not change the actual dataframes
#
#       Build splines   require(splines); bsBasis <- bs(training$age, df=3)

glb_entity_df <- mutate(glb_entity_df,
#     <col_name>.NA=is.na(<col_name>) 
    Team.fctr=factor(Team, 
                as.factor(union(glb_entity_df$Team, glb_predct_df$Team))),
#     Team.fctr.num=grep(Team, levels(Team.fctr)), # This doesn't work
    League.fctr=factor(League, 
                as.factor(union(glb_entity_df$League, glb_predct_df$League)))
#     
#     Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#     Year=year(Date.my),
#     Month=months(Date.my),
#     Weekday=weekdays(Date.my)
    
                    )

# If levels of a factor are different across glb_entity_df & glb_predct_df; predict.glm fails  
# Transformations not handled by mutate                    
glb_entity_df$Team.fctr.num <- sapply(1:nrow(glb_entity_df), 
    function(row_ix) grep(glb_entity_df[row_ix, "Team"],
                          levels(glb_entity_df[row_ix, "Team.fctr"])))

glb_predct_df <- mutate(glb_predct_df, 
    Team.fctr=factor(Team, 
                as.factor(union(glb_entity_df$Team, glb_predct_df$Team))),
    League.fctr=factor(League, 
                as.factor(union(glb_entity_df$League, glb_predct_df$League)))
                        
                    )

glb_predct_df$Team.fctr.num <- sapply(1:nrow(glb_predct_df), 
    function(row_ix) grep(glb_predct_df[row_ix, "Team"],
                          levels(glb_predct_df[row_ix, "Team.fctr"])))

print(summary(glb_entity_df))
```

```
##      Team              League               Year            RS        
##  Length:902         Length:902         Min.   :1962   Min.   : 463.0  
##  Class :character   Class :character   1st Qu.:1973   1st Qu.: 641.2  
##  Mode  :character   Mode  :character   Median :1983   Median : 695.0  
##                                        Mean   :1982   Mean   : 703.8  
##                                        3rd Qu.:1992   3rd Qu.: 761.8  
##                                        Max.   :2001   Max.   :1009.0  
##                                                                       
##        RA               W               OBP             SLG        
##  Min.   : 472.0   Min.   : 40.00   Min.   :0.277   Min.   :0.3010  
##  1st Qu.: 640.0   1st Qu.: 73.00   1st Qu.:0.314   1st Qu.:0.3680  
##  Median : 697.0   Median : 81.00   Median :0.324   Median :0.3880  
##  Mean   : 703.8   Mean   : 80.88   Mean   :0.325   Mean   :0.3904  
##  3rd Qu.: 763.0   3rd Qu.: 89.00   3rd Qu.:0.335   3rd Qu.:0.4118  
##  Max.   :1103.0   Max.   :116.00   Max.   :0.373   Max.   :0.4850  
##                                                                    
##        BA            Playoffs        RankSeason     RankPlayoffs  
##  Min.   :0.2140   Min.   :0.0000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:0.2500   1st Qu.:0.0000   1st Qu.:2.000   1st Qu.:2.000  
##  Median :0.2580   Median :0.0000   Median :2.500   Median :3.000  
##  Mean   :0.2582   Mean   :0.1707   Mean   :2.792   Mean   :2.454  
##  3rd Qu.:0.2670   3rd Qu.:0.0000   3rd Qu.:4.000   3rd Qu.:3.000  
##  Max.   :0.2940   Max.   :1.0000   Max.   :8.000   Max.   :4.000  
##                                    NA's   :748     NA's   :748    
##        G              OOBP             OSLG          Team.fctr  
##  Min.   :158.0   Min.   :0.3010   Min.   :0.3770   BAL    : 36  
##  1st Qu.:162.0   1st Qu.:0.3290   1st Qu.:0.4160   BOS    : 36  
##  Median :162.0   Median :0.3420   Median :0.4325   CHC    : 36  
##  Mean   :161.9   Mean   :0.3405   Mean   :0.4325   CHW    : 36  
##  3rd Qu.:162.0   3rd Qu.:0.3500   3rd Qu.:0.4508   CIN    : 36  
##  Max.   :165.0   Max.   :0.3840   Max.   :0.4990   CLE    : 36  
##                  NA's   :812      NA's   :812      (Other):686  
##  League.fctr Team.fctr.num 
##  AL:462      Min.   : 1.0  
##  NL:440      1st Qu.: 9.0  
##              Median :17.0  
##              Mean   :16.7  
##              3rd Qu.:24.0  
##              Max.   :36.0  
## 
```

```r
print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
```

```
##          Team        League          Year            RS            RA 
##             0             0             0             0             0 
##             W           OBP           SLG            BA      Playoffs 
##             0             0             0             0             0 
##    RankSeason  RankPlayoffs             G          OOBP          OSLG 
##           748           748             0           812           812 
##     Team.fctr   League.fctr Team.fctr.num 
##             0             0             0
```

```r
print(summary(glb_predct_df))
```

```
##      Team              League               Year            RS       
##  Length:330         Length:330         Min.   :2002   Min.   :513.0  
##  Class :character   Class :character   1st Qu.:2004   1st Qu.:696.2  
##  Mode  :character   Mode  :character   Median :2007   Median :741.0  
##                                        Mean   :2007   Mean   :745.9  
##                                        3rd Qu.:2010   3rd Qu.:801.0  
##                                        Max.   :2012   Max.   :968.0  
##                                                                      
##        RA              W               OBP              SLG        
##  Min.   :529.0   Min.   : 43.00   Min.   :0.2920   Min.   :0.3390  
##  1st Qu.:687.2   1st Qu.: 72.00   1st Qu.:0.3210   1st Qu.:0.3992  
##  Median :742.5   Median : 81.50   Median :0.3310   Median :0.4160  
##  Mean   :745.9   Mean   : 80.97   Mean   :0.3301   Mean   :0.4163  
##  3rd Qu.:801.0   3rd Qu.: 90.00   3rd Qu.:0.3390   3rd Qu.:0.4330  
##  Max.   :971.0   Max.   :105.00   Max.   :0.3660   Max.   :0.4910  
##                                                                    
##        BA            Playoffs        RankSeason     RankPlayoffs  
##  Min.   :0.2330   Min.   :0.0000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:0.2550   1st Qu.:0.0000   1st Qu.:2.000   1st Qu.:3.000  
##  Median :0.2630   Median :0.0000   Median :4.000   Median :4.000  
##  Mean   :0.2623   Mean   :0.2727   Mean   :3.689   Mean   :3.167  
##  3rd Qu.:0.2700   3rd Qu.:1.0000   3rd Qu.:5.000   3rd Qu.:4.000  
##  Max.   :0.2900   Max.   :1.0000   Max.   :7.000   Max.   :5.000  
##                                    NA's   :240     NA's   :240    
##        G            OOBP            OSLG          Team.fctr   League.fctr
##  Min.   :161   Min.   :0.294   Min.   :0.3460   ARI    : 11   AL:154     
##  1st Qu.:162   1st Qu.:0.319   1st Qu.:0.3990   ATL    : 11   NL:176     
##  Median :162   Median :0.329   Median :0.4150   BAL    : 11              
##  Mean   :162   Mean   :0.330   Mean   :0.4163   BOS    : 11              
##  3rd Qu.:162   3rd Qu.:0.340   3rd Qu.:0.4328   CHC    : 11              
##  Max.   :163   Max.   :0.372   Max.   :0.4830   CHW    : 11              
##                                                 (Other):264              
##  Team.fctr.num  
##  Min.   : 1.00  
##  1st Qu.: 9.00  
##  Median :16.00  
##  Mean   :17.08  
##  3rd Qu.:24.75  
##  Max.   :39.00  
## 
```

```r
print(sapply(names(glb_predct_df), function(col) sum(is.na(glb_predct_df[, col]))))
```

```
##          Team        League          Year            RS            RA 
##             0             0             0             0             0 
##             W           OBP           SLG            BA      Playoffs 
##             0             0             0             0             0 
##    RankSeason  RankPlayoffs             G          OOBP          OSLG 
##           240           240             0             0             0 
##     Team.fctr   League.fctr Team.fctr.num 
##             0             0             0
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_predct_df
# Check for glb_predct_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, glb_entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, glb_entity_df))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))


# Other plots:
# print(myplot_histogram(glb_entity_df, "<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>"))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# 
# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
glb_entity_df <- mutate(glb_entity_df,
    Runs.diff=RS - RA
                    )

glb_predct_df <- mutate(glb_predct_df,
    Runs.diff=RS - RA
                    )

print(summary(glb_entity_df))
```

```
##      Team              League               Year            RS        
##  Length:902         Length:902         Min.   :1962   Min.   : 463.0  
##  Class :character   Class :character   1st Qu.:1973   1st Qu.: 641.2  
##  Mode  :character   Mode  :character   Median :1983   Median : 695.0  
##                                        Mean   :1982   Mean   : 703.8  
##                                        3rd Qu.:1992   3rd Qu.: 761.8  
##                                        Max.   :2001   Max.   :1009.0  
##                                                                       
##        RA               W               OBP             SLG        
##  Min.   : 472.0   Min.   : 40.00   Min.   :0.277   Min.   :0.3010  
##  1st Qu.: 640.0   1st Qu.: 73.00   1st Qu.:0.314   1st Qu.:0.3680  
##  Median : 697.0   Median : 81.00   Median :0.324   Median :0.3880  
##  Mean   : 703.8   Mean   : 80.88   Mean   :0.325   Mean   :0.3904  
##  3rd Qu.: 763.0   3rd Qu.: 89.00   3rd Qu.:0.335   3rd Qu.:0.4118  
##  Max.   :1103.0   Max.   :116.00   Max.   :0.373   Max.   :0.4850  
##                                                                    
##        BA            Playoffs        RankSeason     RankPlayoffs  
##  Min.   :0.2140   Min.   :0.0000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:0.2500   1st Qu.:0.0000   1st Qu.:2.000   1st Qu.:2.000  
##  Median :0.2580   Median :0.0000   Median :2.500   Median :3.000  
##  Mean   :0.2582   Mean   :0.1707   Mean   :2.792   Mean   :2.454  
##  3rd Qu.:0.2670   3rd Qu.:0.0000   3rd Qu.:4.000   3rd Qu.:3.000  
##  Max.   :0.2940   Max.   :1.0000   Max.   :8.000   Max.   :4.000  
##                                    NA's   :748     NA's   :748    
##        G              OOBP             OSLG          Team.fctr  
##  Min.   :158.0   Min.   :0.3010   Min.   :0.3770   BAL    : 36  
##  1st Qu.:162.0   1st Qu.:0.3290   1st Qu.:0.4160   BOS    : 36  
##  Median :162.0   Median :0.3420   Median :0.4325   CHC    : 36  
##  Mean   :161.9   Mean   :0.3405   Mean   :0.4325   CHW    : 36  
##  3rd Qu.:162.0   3rd Qu.:0.3500   3rd Qu.:0.4508   CIN    : 36  
##  Max.   :165.0   Max.   :0.3840   Max.   :0.4990   CLE    : 36  
##                  NA's   :812      NA's   :812      (Other):686  
##  League.fctr Team.fctr.num    Runs.diff      
##  AL:462      Min.   : 1.0   Min.   :-331.00  
##  NL:440      1st Qu.: 9.0   1st Qu.: -70.75  
##              Median :17.0   Median :   3.00  
##              Mean   :16.7   Mean   :   0.00  
##              3rd Qu.:24.0   3rd Qu.:  69.75  
##              Max.   :36.0   Max.   : 309.00  
## 
```

```r
print(summary(glb_predct_df))
```

```
##      Team              League               Year            RS       
##  Length:330         Length:330         Min.   :2002   Min.   :513.0  
##  Class :character   Class :character   1st Qu.:2004   1st Qu.:696.2  
##  Mode  :character   Mode  :character   Median :2007   Median :741.0  
##                                        Mean   :2007   Mean   :745.9  
##                                        3rd Qu.:2010   3rd Qu.:801.0  
##                                        Max.   :2012   Max.   :968.0  
##                                                                      
##        RA              W               OBP              SLG        
##  Min.   :529.0   Min.   : 43.00   Min.   :0.2920   Min.   :0.3390  
##  1st Qu.:687.2   1st Qu.: 72.00   1st Qu.:0.3210   1st Qu.:0.3992  
##  Median :742.5   Median : 81.50   Median :0.3310   Median :0.4160  
##  Mean   :745.9   Mean   : 80.97   Mean   :0.3301   Mean   :0.4163  
##  3rd Qu.:801.0   3rd Qu.: 90.00   3rd Qu.:0.3390   3rd Qu.:0.4330  
##  Max.   :971.0   Max.   :105.00   Max.   :0.3660   Max.   :0.4910  
##                                                                    
##        BA            Playoffs        RankSeason     RankPlayoffs  
##  Min.   :0.2330   Min.   :0.0000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:0.2550   1st Qu.:0.0000   1st Qu.:2.000   1st Qu.:3.000  
##  Median :0.2630   Median :0.0000   Median :4.000   Median :4.000  
##  Mean   :0.2623   Mean   :0.2727   Mean   :3.689   Mean   :3.167  
##  3rd Qu.:0.2700   3rd Qu.:1.0000   3rd Qu.:5.000   3rd Qu.:4.000  
##  Max.   :0.2900   Max.   :1.0000   Max.   :7.000   Max.   :5.000  
##                                    NA's   :240     NA's   :240    
##        G            OOBP            OSLG          Team.fctr   League.fctr
##  Min.   :161   Min.   :0.294   Min.   :0.3460   ARI    : 11   AL:154     
##  1st Qu.:162   1st Qu.:0.319   1st Qu.:0.3990   ATL    : 11   NL:176     
##  Median :162   Median :0.329   Median :0.4150   BAL    : 11              
##  Mean   :162   Mean   :0.330   Mean   :0.4163   BOS    : 11              
##  3rd Qu.:162   3rd Qu.:0.340   3rd Qu.:0.4328   CHC    : 11              
##  Max.   :163   Max.   :0.372   Max.   :0.4830   CHW    : 11              
##                                                 (Other):264              
##  Team.fctr.num     Runs.diff      
##  Min.   : 1.00   Min.   :-337.00  
##  1st Qu.: 9.00   1st Qu.: -81.25  
##  Median :16.00   Median :   7.00  
##  Mean   :17.08   Mean   :   0.00  
##  3rd Qu.:24.75   3rd Qu.:  81.75  
##  Max.   :39.00   Max.   : 210.00  
## 
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- myselect_features())
```

```
##                          id       cor.y  cor.y.abs
## OOBP                   OOBP  0.90990783 0.90990783
## OSLG                   OSLG  0.89511011 0.89511011
## Year                   Year  0.48307166 0.48307166
## G                         G -0.03740167 0.03740167
## Team.fctr.num Team.fctr.num -0.02090139 0.02090139
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, 
                    merge(glb_feats_df, mydelete_cor_features(), all.x=TRUE)))
```

```
##                     OOBP        OSLG       Year           G Team.fctr.num
## OOBP           1.0000000  0.79652577 -0.3355334 -0.10054464    0.20854303
## OSLG           0.7965258  1.00000000 -0.1226680 -0.04543175    0.15911057
## Year          -0.3355334 -0.12266798  1.0000000 -0.02708490   -0.04891250
## G             -0.1005446 -0.04543175 -0.0270849  1.00000000    0.05287525
## Team.fctr.num  0.2085430  0.15911057 -0.0489125  0.05287525    1.00000000
##                    OOBP       OSLG      Year          G Team.fctr.num
## OOBP          0.0000000 0.79652577 0.3355334 0.10054464    0.20854303
## OSLG          0.7965258 0.00000000 0.1226680 0.04543175    0.15911057
## Year          0.3355334 0.12266798 0.0000000 0.02708490    0.04891250
## G             0.1005446 0.04543175 0.0270849 0.00000000    0.05287525
## Team.fctr.num 0.2085430 0.15911057 0.0489125 0.05287525    0.00000000
## [1] "cor(OOBP, OSLG)=0.7965"
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

![](MoneyBall_RA_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(RA, OOBP)=0.9099"
## [1] "cor(RA, OSLG)=0.8951"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 812 rows containing missing values (stat_smooth).
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 812 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 812 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 812 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

```
## Warning in mydelete_cor_features(): Dropping OSLG as a feature
```

![](MoneyBall_RA_files/figure-html/remove_correlated_features-2.png) 

```
##                          id       cor.y  cor.y.abs
## OOBP                   OOBP  0.90990783 0.90990783
## Year                   Year  0.48307166 0.48307166
## G                         G -0.03740167 0.03740167
## Team.fctr.num Team.fctr.num -0.02090139 0.02090139
##                     OOBP       Year           G Team.fctr.num
## OOBP           1.0000000 -0.3355334 -0.10054464    0.20854303
## Year          -0.3355334  1.0000000 -0.02708490   -0.04891250
## G             -0.1005446 -0.0270849  1.00000000    0.05287525
## Team.fctr.num  0.2085430 -0.0489125  0.05287525    1.00000000
##                    OOBP      Year          G Team.fctr.num
## OOBP          0.0000000 0.3355334 0.10054464    0.20854303
## Year          0.3355334 0.0000000 0.02708490    0.04891250
## G             0.1005446 0.0270849 0.00000000    0.05287525
## Team.fctr.num 0.2085430 0.0489125 0.05287525    0.00000000
##              id       cor.y  cor.y.abs cor.low
## 2          OOBP  0.90990783 0.90990783       1
## 3          OSLG  0.89511011 0.89511011      NA
## 5          Year  0.48307166 0.48307166       1
## 4 Team.fctr.num -0.02090139 0.02090139       1
## 1             G -0.03740167 0.03740167       1
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    
    # Highest cor.y
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=max_cor_y_x_var,
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)
    
    # Enhance Highest cor.y model with additions of interaction terms that were 
    #   dropped due to high correlations
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)    

    # Low correlated X
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=subset(glb_feats_df, cor.low == 1)[, "id"],
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)
    
    # All X that is not missing
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                             glb_predct_var),
                                                     glb_exclude_vars_as_features),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)
    
    # OOBP, OSLG
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=c("OOBP", "OSLG"),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)
    glb_sel_mdl <- glb_mdl
    
}    
```

```
## [1] 492009.7
## [1] 0.8652212
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -70.058 -31.189   5.995  25.791  83.258 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -865.95      81.51  -10.62   <2e-16 ***
## OOBP         4921.07     239.15   20.58   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.78 on 88 degrees of freedom
##   (812 observations deleted due to missingness)
## Multiple R-squared:  0.8279,	Adjusted R-squared:  0.826 
## F-statistic: 423.4 on 1 and 88 DF,  p-value: < 2.2e-16
## 
##   feats n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit  SSE.fit  SSE.OOB
## 1  OOBP   902 0.8279323 0.8652212    0.8279323 106459.4 492009.7
##   f.score.OOB
## 1          NA
## [1] 279942.6
## [1] 0.9233139
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -80.969 -16.261   0.251  17.183  62.253 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -193.12      98.05  -1.970   0.0521 .  
## OOBP         1029.22     481.82   2.136   0.0355 *  
## OOBP:OSLG    4420.10     509.37   8.678 2.05e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.61 on 87 degrees of freedom
##   (812 observations deleted due to missingness)
## Multiple R-squared:  0.9078,	Adjusted R-squared:  0.9056 
## F-statistic: 428.1 on 2 and 87 DF,  p-value: < 2.2e-16
## 
##             feats n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit   SSE.fit
## 2 OOBP, OOBP:OSLG   902 0.9077648 0.9233139    0.9077648  57066.49
## 1            OOBP   902 0.8279323 0.8652212    0.8279323 106459.36
##    SSE.OOB f.score.OOB
## 2 279942.6          NA
## 1 492009.7          NA
## [1] 1903117
## [1] 0.4786692
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -64.685 -27.186   4.041  26.117  86.167 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -1.445e+04  9.596e+03  -1.506    0.136    
## OOBP           5.086e+03  2.593e+02  19.611   <2e-16 ***
## Year           7.459e+00  4.758e+00   1.567    0.121    
## Team.fctr.num -4.632e-01  4.344e-01  -1.066    0.289    
## G             -8.546e+00  8.947e+00  -0.955    0.342    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34.59 on 85 degrees of freedom
##   (812 observations deleted due to missingness)
## Multiple R-squared:  0.8356,	Adjusted R-squared:  0.8279 
## F-statistic:   108 on 4 and 85 DF,  p-value: < 2.2e-16
## 
##                          feats n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit
## 2              OOBP, OOBP:OSLG   902 0.9077648 0.9233139    0.9077648
## 1                         OOBP   902 0.8279323 0.8652212    0.8279323
## 3 OOBP, Year, Team.fctr.num, G   902 0.8356310 0.4786692    0.8356310
##     SSE.fit   SSE.OOB f.score.OOB
## 2  57066.49  279942.6          NA
## 1 106459.36  492009.7          NA
## 3 101696.12 1903117.4          NA
## [1] 238194.8
## [1] 0.9347501
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -73.243 -15.203  -0.135  13.843  66.202 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2035.1639  7277.8400   0.280    0.780    
## Year            -0.7035     3.6087  -0.195    0.846    
## G               -8.9049     6.7289  -1.323    0.189    
## OOBP          2859.9897   319.4547   8.953 8.12e-14 ***
## OSLG          1525.6615   179.2729   8.510 6.24e-13 ***
## League.fctrNL   -8.5225     5.5780  -1.528    0.130    
## Team.fctr.num   -0.3739     0.3183  -1.174    0.244    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.32 on 83 degrees of freedom
##   (812 observations deleted due to missingness)
## Multiple R-squared:  0.914,	Adjusted R-squared:  0.9077 
## F-statistic: 146.9 on 6 and 83 DF,  p-value: < 2.2e-16
## 
##                                             feats n.fit  R.sq.fit
## 4 Year, G, OOBP, OSLG, League.fctr, Team.fctr.num   902 0.9139623
## 2                                 OOBP, OOBP:OSLG   902 0.9077648
## 1                                            OOBP   902 0.8279323
## 3                    OOBP, Year, Team.fctr.num, G   902 0.8356310
##    R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 4 0.9347501    0.9139623  53232.06  238194.8          NA
## 2 0.9233139    0.9077648  57066.49  279942.6          NA
## 1 0.8652212    0.8279323 106459.36  492009.7          NA
## 3 0.4786692    0.8356310 101696.12 1903117.4          NA
## [1] 272815.9
## [1] 0.9252661
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -82.397 -15.178  -0.129  17.679  60.955 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -837.38      60.26 -13.897  < 2e-16 ***
## OOBP         2913.60     291.97   9.979 4.46e-16 ***
## OSLG         1514.29     175.43   8.632 2.55e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.67 on 87 degrees of freedom
##   (812 observations deleted due to missingness)
## Multiple R-squared:  0.9073,	Adjusted R-squared:  0.9052 
## F-statistic: 425.8 on 2 and 87 DF,  p-value: < 2.2e-16
## 
##                                             feats n.fit  R.sq.fit
## 4 Year, G, OOBP, OSLG, League.fctr, Team.fctr.num   902 0.9139623
## 5                                      OOBP, OSLG   902 0.9073132
## 2                                 OOBP, OOBP:OSLG   902 0.9077648
## 1                                            OOBP   902 0.8279323
## 3                    OOBP, Year, Team.fctr.num, G   902 0.8356310
##    R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 4 0.9347501    0.9139623  53232.06  238194.8          NA
## 5 0.9252661    0.9073132  57345.89  272815.9          NA
## 2 0.9233139    0.9077648  57066.49  279942.6          NA
## 1 0.8652212    0.8279323 106459.36  492009.7          NA
## 3 0.4786692    0.8356310 101696.12 1903117.4          NA
```

```r
#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    
    # Highest cor.y
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=max_cor_y_x_var,
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)        

    # Enhance Highest cor.y model with additions of interaction terms that were 
    #   dropped due to high correlations
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)    

    # Low correlated X
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=subset(glb_feats_df, cor.low == 1)[, "id"],
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)        
    glb_sel_mdl <- glb_mdl
    
    # All X that is not missing
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                             glb_predct_var),
                                                     glb_exclude_vars_as_features),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)        
                            
}

if (glb_is_regression)
    print(myplot_scatter(glb_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats), data=glb_models_df, color="NavyBlue", 
                    size=3.5))
```

![](MoneyBall_RA_files/figure-html/run_models-1.png) 

```r
if (glb_is_classification)
    print(myplot_hbar(df=glb_models_df, xcol_name="feats", ycol_names="f.score.OOB"))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats())
```

```
##      Estimate Std. Error  t value         Pr.z   id
## OOBP 2913.599   291.9710 9.979072 4.460430e-16 OOBP
## OSLG 1514.286   175.4281 8.631948 2.545357e-13 OSLG
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
}    
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -82.397 -15.178  -0.129  17.679  60.955 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -837.38      60.26 -13.897  < 2e-16 ***
## OOBP         2913.60     291.97   9.979 4.46e-16 ***
## OSLG         1514.29     175.43   8.632 2.55e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.67 on 87 degrees of freedom
##   (812 observations deleted due to missingness)
## Multiple R-squared:  0.9073,	Adjusted R-squared:  0.9052 
## F-statistic: 425.8 on 2 and 87 DF,  p-value: < 2.2e-16
## 
##                                             feats n.fit  R.sq.fit
## 4 Year, G, OOBP, OSLG, League.fctr, Team.fctr.num   902 0.9139623
## 5                                      OOBP, OSLG   902 0.9073132
## 2                                 OOBP, OOBP:OSLG   902 0.9077648
## 1                                            OOBP   902 0.8279323
## 3                    OOBP, Year, Team.fctr.num, G   902 0.8356310
## 6                                      OOBP, OSLG   902 0.9073132
##    R.sq.OOB Adj.R.sq.fit   SSE.fit   SSE.OOB f.score.OOB
## 4 0.9347501    0.9139623  53232.06  238194.8          NA
## 5 0.9252661    0.9073132  57345.89  272815.9          NA
## 2 0.9233139    0.9077648  57066.49  279942.6          NA
## 1 0.8652212    0.8279323 106459.36  492009.7          NA
## 3 0.4786692    0.8356310 101696.12 1903117.4          NA
## 6        NA    0.9073132  57345.89        NA          NA
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning: Removed 812 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 812 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

![](MoneyBall_RA_files/figure-html/fit_training.all-1.png) 

```r
if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
    glb_entity_df[, glb_predct_var_name] <- (predict(glb_sel_mdl, 
                        newdata=glb_entity_df, type="response") >= 0.5) * 1.0
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_entity_df))
}    

print(glb_feats_df <- mymerge_feats_Pr.z())
```

```
##              id       cor.y  cor.y.abs cor.low         Pr.z
## 2          OOBP  0.90990783 0.90990783       1 4.460430e-16
## 3          OSLG  0.89511011 0.89511011      NA 2.545357e-13
## 1             G -0.03740167 0.03740167       1           NA
## 4 Team.fctr.num -0.02090139 0.02090139       1           NA
## 5          Year  0.48307166 0.48307166       1           NA
```

```r
for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
    plot_df <- melt(glb_entity_df, id.vars=var, 
                    measure.vars=c(glb_predct_var, glb_predct_var_name))
    print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))    
}
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

![](MoneyBall_RA_files/figure-html/fit_training.all-2.png) 

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

![](MoneyBall_RA_files/figure-html/fit_training.all-3.png) 

```r
if (glb_is_regression) {
    plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
    print(myplot_prediction_regression(glb_entity_df, 
                ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                       plot_vars_df$id[1]) + 
              geom_point(aes_string(color="League.fctr"))
              )
}    
```

```
##     Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 357  STL     NL 2001 814 684 93 0.339 0.441 0.270        1          4
## 408  MON     NL 1999 718 853 68 0.323 0.427 0.265        0         NA
## 360  TOR     AL 2001 767 753 80 0.325 0.430 0.263        0         NA
## 368  CIN     NL 2000 825 765 85 0.343 0.447 0.274        0         NA
## 409  NYM     NL 1999 853 711 97 0.363 0.434 0.279        1          4
##     RankPlayoffs   G  OOBP  OSLG Team.fctr League.fctr Team.fctr.num
## 357            4 162 0.328 0.428       STL          NL            27
## 408           NA 162 0.342 0.418       MON          NL            18
## 360           NA 162 0.339 0.435       TOR          AL            30
## 368           NA 163 0.341 0.438       CIN          NL             8
## 409            3 163 0.331 0.418       NYM          NL            19
##     Runs.diff RA.predict RA.predict.err   .label
## 357       130   766.3971       82.39713 STL:2001
## 408      -135   792.0447       60.95533 MON:1999
## 360        14   809.0467       56.04673 TOR:2001
## 368        60   819.4168       54.41679 CIN:2000
## 409       142   759.9951       48.99507 NYM:1999
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

```
## Warning: Removed 812 rows containing missing values (geom_text).
```

```
## Warning: Removed 812 rows containing missing values (geom_point).
```

![](MoneyBall_RA_files/figure-html/fit_training.all-4.png) 

```r
if (glb_is_classification) {
    plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
    print(myplot_prediction_classification(glb_entity_df, plot_vars_df$id[1],
                                           plot_vars_df$id[2]))
}    

script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression) 
    glb_predct_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_predct_df, type="response")

if (glb_is_classification)
    glb_predct_df[, glb_predct_var_name] <- (predict(glb_sel_mdl, 
                        newdata=glb_predct_df, type="response") >= 0.5) * 1.0
    
myprint_df(glb_predct_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##   Team Year  RA RA.predict
## 1  ARI 2012 688   714.6618
## 2  ATL 2012 600   626.5836
## 3  BAL 2012 705   690.6632
## 4  BOS 2012 806   775.1379
## 5  CHC 2012 759   780.7352
## 6  CHW 2012 676   705.3462
##     Team Year  RA RA.predict
## 60   WSN 2011 643   670.5176
## 107  MIN 2009 765   779.6808
## 149  TOR 2008 610   638.3530
## 230  OAK 2005 658   647.2088
## 314  KCR 2002 891   882.0970
## 315  LAD 2002 643   666.4346
##     Team Year  RA RA.predict
## 325  SEA 2002 699   701.2632
## 326  SFG 2002 616   655.3747
## 327  STL 2002 648   688.2291
## 328  TBD 2002 918   903.8915
## 329  TEX 2002 882   879.8929
## 330  TOR 2002 828   817.5576
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_predct_df[, glb_predct_var_name] - 
                        glb_predct_df[, glb_predct_var]) ^ 2)))
    print(myplot_scatter(glb_predct_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
}    
```

```
## [1] "Total SSE: 272815.9085"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](MoneyBall_RA_files/figure-html/predict_newdata-1.png) 

```r
if (glb_is_classification)
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_predct_df))
    
for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
    plot_df <- melt(glb_predct_df, id.vars=var, 
                    measure.vars=c(glb_predct_var, glb_predct_var_name))
    print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))    
}
```

![](MoneyBall_RA_files/figure-html/predict_newdata-2.png) ![](MoneyBall_RA_files/figure-html/predict_newdata-3.png) 

```r
# Add choose(, 2) functionality to select feature pairs to plot 
if (glb_is_regression) {
    plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
    print(myplot_prediction_regression(glb_predct_df, 
                ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                        plot_vars_df$id[1]) + 
              geom_point(aes_string(color="League.fctr"))
              )
}    
```

```
##     Team League Year  RS  RA  W   OBP   SLG    BA Playoffs RankSeason
## 289  NYM     NL 2003 642 754 66 0.314 0.374 0.247        0         NA
## 212  ATL     NL 2005 769 674 90 0.333 0.435 0.265        1          4
## 112  PIT     NL 2009 636 768 62 0.318 0.387 0.252        0         NA
## 323  PIT     NL 2002 641 730 72 0.319 0.381 0.244        0         NA
## 78   NYM     NL 2010 656 652 79 0.314 0.383 0.249        0         NA
##     RankPlayoffs   G  OOBP  OSLG Team.fctr League.fctr Team.fctr.num
## 289           NA 161 0.345 0.438       NYM          NL            19
## 212            4 162 0.333 0.406       ATL          NL             3
## 112           NA 161 0.346 0.442       PIT          NL            23
## 323           NA 161 0.342 0.423       PIT          NL            23
## 78            NA 162 0.330 0.394       NYM          NL            19
##     Runs.diff RA.predict RA.predict.err   .label
## 289      -112   831.0712       77.07118 NYM:2003
## 212        95   747.6508       73.65084 ATL:2005
## 112      -132   840.0419       72.04193 PIT:2009
## 323       -89   799.6161       69.61610 PIT:2002
## 78          4   720.7386       68.73861 NYM:2010
```

![](MoneyBall_RA_files/figure-html/predict_newdata-4.png) 

```r
if (glb_is_classification) {
    plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
    print(myplot_prediction_classification(glb_predct_df, plot_vars_df$id[1],
                                           plot_vars_df$id[2]))
}    
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] reshape2_1.4.1  plyr_1.8.1      doBy_4.5-13     survival_2.38-1
## [5] ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-5 digest_0.6.8     evaluate_0.5.5   formatR_1.0     
##  [5] grid_3.1.2       gtable_0.1.2     htmltools_0.2.6  knitr_1.9       
##  [9] labeling_0.3     lattice_0.20-30  MASS_7.3-39      Matrix_1.1-5    
## [13] munsell_0.4.2    proto_0.3-10     Rcpp_0.11.4      rmarkdown_0.5.1 
## [17] scales_0.2.4     splines_3.1.2    stringr_0.6.2    tcltk_3.1.2     
## [21] tools_3.1.2      yaml_2.1.13
```
