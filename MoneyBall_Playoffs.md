# MoneyBall: Playoffs classification
bdanalytics  

**  **    
**Date: (Thu) Mar 19, 2015**    

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
glb_predct_var <- "Playoffs"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("Team", "Year")                    # or NULL

# List vars that are missing in glb_entity_df
glb_exclude_vars_as_features <- c("RankSeason", "RankPlayoffs", "OOBP", "OSLG") # or NULL
# List chrs converted into factors 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("Team", "League", 
                                        "Team_fctr")
                                      ) # or NULL

glb_is_classification <- TRUE; glb_is_regression <- !glb_is_classification

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
# <col_name>:   <description>; <notes>

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
    Team_fctr=factor(Team, 
                as.factor(union(glb_entity_df$Team, glb_predct_df$Team))),
#     Team_fctr_num=grep(Team, levels(Team_fctr)), # This doesn't work
    League_fctr=factor(League, 
                as.factor(union(glb_entity_df$League, glb_predct_df$League)))
#     
#     Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#     Year=year(Date.my),
#     Month=months(Date.my),
#     Weekday=weekdays(Date.my)
    
                    )

# If levels of a factor are different across glb_entity_df & glb_predct_df; predict.glm fails  
# Transformations not handled by mutate                    
glb_entity_df$Team_fctr_num <- sapply(1:nrow(glb_entity_df), 
    function(row_ix) grep(glb_entity_df[row_ix, "Team"],
                          levels(glb_entity_df[row_ix, "Team_fctr"])))

glb_predct_df <- mutate(glb_predct_df, 
    Team_fctr=factor(Team, 
                as.factor(union(glb_entity_df$Team, glb_predct_df$Team))),
    Team_fctr_num=seq_along(levels(Team_fctr))[Team_fctr],
    League_fctr=factor(League, 
                as.factor(union(glb_entity_df$League, glb_predct_df$League)))
                        
                    )

glb_predct_df$Team_fctr_num <- sapply(1:nrow(glb_predct_df), 
    function(row_ix) grep(glb_predct_df[row_ix, "Team"],
                          levels(glb_predct_df[row_ix, "Team_fctr"])))

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
##        G              OOBP             OSLG          Team_fctr  
##  Min.   :158.0   Min.   :0.3010   Min.   :0.3770   BAL    : 36  
##  1st Qu.:162.0   1st Qu.:0.3290   1st Qu.:0.4160   BOS    : 36  
##  Median :162.0   Median :0.3420   Median :0.4325   CHC    : 36  
##  Mean   :161.9   Mean   :0.3405   Mean   :0.4325   CHW    : 36  
##  3rd Qu.:162.0   3rd Qu.:0.3500   3rd Qu.:0.4508   CIN    : 36  
##  Max.   :165.0   Max.   :0.3840   Max.   :0.4990   CLE    : 36  
##                  NA's   :812      NA's   :812      (Other):686  
##  League_fctr Team_fctr_num 
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
##     Team_fctr   League_fctr Team_fctr_num 
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
##        G            OOBP            OSLG          Team_fctr  
##  Min.   :161   Min.   :0.294   Min.   :0.3460   ARI    : 11  
##  1st Qu.:162   1st Qu.:0.319   1st Qu.:0.3990   ATL    : 11  
##  Median :162   Median :0.329   Median :0.4150   BAL    : 11  
##  Mean   :162   Mean   :0.330   Mean   :0.4163   BOS    : 11  
##  3rd Qu.:162   3rd Qu.:0.340   3rd Qu.:0.4328   CHC    : 11  
##  Max.   :163   Max.   :0.372   Max.   :0.4830   CHW    : 11  
##                                                 (Other):264  
##  Team_fctr_num   League_fctr
##  Min.   : 1.00   AL:154     
##  1st Qu.: 9.00   NL:176     
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
##     Team_fctr Team_fctr_num   League_fctr 
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
## Warning in myselect_features(): Ignoring features due to NAs:RankSeason,
## RankPlayoffs, OOBP, OSLG
```

```
##        id        cor.y
## Year Year  0.113013647
## RS     RS  0.371630900
## RA     RA -0.241213487
## W       W  0.588978323
## OBP   OBP  0.345529577
## SLG   SLG  0.300191209
## BA     BA  0.278771974
## G       G  0.009831081
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
print(glb_feats_df <- mydelete_cor_features())
```

```
## Warning in mydelete_cor_features(): Ignoring features due to NAs:
```

```
##              Year         RS          RA            W          OBP
## Year  1.000000000 0.48550228  0.48307166  0.002755645  0.474798914
## RS    0.485502279 1.00000000  0.41501351  0.507382436  0.904909154
## RA    0.483071657 0.41501351  1.00000000 -0.507771762  0.367105468
## W     0.002755645 0.50738244 -0.50777176  1.000000000  0.474079772
## OBP   0.474798914 0.90490915  0.36710547  0.474079772  1.000000000
## SLG   0.517827624 0.92638433  0.45951445  0.405972307  0.806153882
## BA    0.438729149 0.83162475  0.35041098  0.416391099  0.854054862
## G    -0.027084904 0.05620616 -0.03740167  0.108128075 -0.003826664
##             SLG           BA            G
## Year 0.51782762  0.438729149 -0.027084904
## RS   0.92638433  0.831624752  0.056206164
## RA   0.45951445  0.350410982 -0.037401665
## W    0.40597231  0.416391099  0.108128075
## OBP  0.80615388  0.854054862 -0.003826664
## SLG  1.00000000  0.814068123  0.012873458
## BA   0.81406812  1.000000000 -0.001805097
## G    0.01287346 -0.001805097  1.000000000
##             Year         RS         RA           W         OBP        SLG
## Year 0.000000000 0.48550228 0.48307166 0.002755645 0.474798914 0.51782762
## RS   0.485502279 0.00000000 0.41501351 0.507382436 0.904909154 0.92638433
## RA   0.483071657 0.41501351 0.00000000 0.507771762 0.367105468 0.45951445
## W    0.002755645 0.50738244 0.50777176 0.000000000 0.474079772 0.40597231
## OBP  0.474798914 0.90490915 0.36710547 0.474079772 0.000000000 0.80615388
## SLG  0.517827624 0.92638433 0.45951445 0.405972307 0.806153882 0.00000000
## BA   0.438729149 0.83162475 0.35041098 0.416391099 0.854054862 0.81406812
## G    0.027084904 0.05620616 0.03740167 0.108128075 0.003826664 0.01287346
##               BA           G
## Year 0.438729149 0.027084904
## RS   0.831624752 0.056206164
## RA   0.350410982 0.037401665
## W    0.416391099 0.108128075
## OBP  0.854054862 0.003826664
## SLG  0.814068123 0.012873458
## BA   0.000000000 0.001805097
## G    0.001805097 0.000000000
## [1] "cor(RS, SLG)=0.9264"
```

![](MoneyBall_Playoffs_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(Playoffs, RS)=0.3716"
## [1] "cor(Playoffs, SLG)=0.3002"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in mydelete_cor_features(): Dropping SLG as a feature
```

![](MoneyBall_Playoffs_files/figure-html/remove_correlated_features-2.png) 

```
##        id        cor.y
## Year Year  0.113013647
## RS     RS  0.371630900
## RA     RA -0.241213487
## W       W  0.588978323
## OBP   OBP  0.345529577
## BA     BA  0.278771974
## G       G  0.009831081
##              Year         RS          RA            W          OBP
## Year  1.000000000 0.48550228  0.48307166  0.002755645  0.474798914
## RS    0.485502279 1.00000000  0.41501351  0.507382436  0.904909154
## RA    0.483071657 0.41501351  1.00000000 -0.507771762  0.367105468
## W     0.002755645 0.50738244 -0.50777176  1.000000000  0.474079772
## OBP   0.474798914 0.90490915  0.36710547  0.474079772  1.000000000
## BA    0.438729149 0.83162475  0.35041098  0.416391099  0.854054862
## G    -0.027084904 0.05620616 -0.03740167  0.108128075 -0.003826664
##                BA            G
## Year  0.438729149 -0.027084904
## RS    0.831624752  0.056206164
## RA    0.350410982 -0.037401665
## W     0.416391099  0.108128075
## OBP   0.854054862 -0.003826664
## BA    1.000000000 -0.001805097
## G    -0.001805097  1.000000000
##             Year         RS         RA           W         OBP          BA
## Year 0.000000000 0.48550228 0.48307166 0.002755645 0.474798914 0.438729149
## RS   0.485502279 0.00000000 0.41501351 0.507382436 0.904909154 0.831624752
## RA   0.483071657 0.41501351 0.00000000 0.507771762 0.367105468 0.350410982
## W    0.002755645 0.50738244 0.50777176 0.000000000 0.474079772 0.416391099
## OBP  0.474798914 0.90490915 0.36710547 0.474079772 0.000000000 0.854054862
## BA   0.438729149 0.83162475 0.35041098 0.416391099 0.854054862 0.000000000
## G    0.027084904 0.05620616 0.03740167 0.108128075 0.003826664 0.001805097
##                G
## Year 0.027084904
## RS   0.056206164
## RA   0.037401665
## W    0.108128075
## OBP  0.003826664
## BA   0.001805097
## G    0.000000000
## [1] "cor(RS, OBP)=0.9049"
```

![](MoneyBall_Playoffs_files/figure-html/remove_correlated_features-3.png) 

```
## [1] "cor(Playoffs, RS)=0.3716"
## [1] "cor(Playoffs, OBP)=0.3455"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in mydelete_cor_features(): Dropping OBP as a feature
```

![](MoneyBall_Playoffs_files/figure-html/remove_correlated_features-4.png) 

```
##        id        cor.y
## Year Year  0.113013647
## RS     RS  0.371630900
## RA     RA -0.241213487
## W       W  0.588978323
## BA     BA  0.278771974
## G       G  0.009831081
##              Year         RS          RA            W           BA
## Year  1.000000000 0.48550228  0.48307166  0.002755645  0.438729149
## RS    0.485502279 1.00000000  0.41501351  0.507382436  0.831624752
## RA    0.483071657 0.41501351  1.00000000 -0.507771762  0.350410982
## W     0.002755645 0.50738244 -0.50777176  1.000000000  0.416391099
## BA    0.438729149 0.83162475  0.35041098  0.416391099  1.000000000
## G    -0.027084904 0.05620616 -0.03740167  0.108128075 -0.001805097
##                 G
## Year -0.027084904
## RS    0.056206164
## RA   -0.037401665
## W     0.108128075
## BA   -0.001805097
## G     1.000000000
##             Year         RS         RA           W          BA           G
## Year 0.000000000 0.48550228 0.48307166 0.002755645 0.438729149 0.027084904
## RS   0.485502279 0.00000000 0.41501351 0.507382436 0.831624752 0.056206164
## RA   0.483071657 0.41501351 0.00000000 0.507771762 0.350410982 0.037401665
## W    0.002755645 0.50738244 0.50777176 0.000000000 0.416391099 0.108128075
## BA   0.438729149 0.83162475 0.35041098 0.416391099 0.000000000 0.001805097
## G    0.027084904 0.05620616 0.03740167 0.108128075 0.001805097 0.000000000
## [1] "cor(RS, BA)=0.8316"
```

![](MoneyBall_Playoffs_files/figure-html/remove_correlated_features-5.png) 

```
## [1] "cor(Playoffs, RS)=0.3716"
## [1] "cor(Playoffs, BA)=0.2788"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 2.5e-05
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at -0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.005
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 1.01
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in mydelete_cor_features(): Dropping BA as a feature
```

![](MoneyBall_Playoffs_files/figure-html/remove_correlated_features-6.png) 

```
##        id        cor.y
## Year Year  0.113013647
## RS     RS  0.371630900
## RA     RA -0.241213487
## W       W  0.588978323
## G       G  0.009831081
##              Year         RS          RA            W           G
## Year  1.000000000 0.48550228  0.48307166  0.002755645 -0.02708490
## RS    0.485502279 1.00000000  0.41501351  0.507382436  0.05620616
## RA    0.483071657 0.41501351  1.00000000 -0.507771762 -0.03740167
## W     0.002755645 0.50738244 -0.50777176  1.000000000  0.10812807
## G    -0.027084904 0.05620616 -0.03740167  0.108128075  1.00000000
##             Year         RS         RA           W          G
## Year 0.000000000 0.48550228 0.48307166 0.002755645 0.02708490
## RS   0.485502279 0.00000000 0.41501351 0.507382436 0.05620616
## RA   0.483071657 0.41501351 0.00000000 0.507771762 0.03740167
## W    0.002755645 0.50738244 0.50777176 0.000000000 0.10812807
## G    0.027084904 0.05620616 0.03740167 0.108128075 0.00000000
##        id        cor.y
## Year Year  0.113013647
## RS     RS  0.371630900
## RA     RA -0.241213487
## W       W  0.588978323
## G       G  0.009831081
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
glb_models_df <- data.frame()

#   Regression:
#       Linear:

#       Prep non-numerical vars before runing this
# ret_lst <- myrun_mdl_lm(indep_vars_vctr=".", 
#                         fit_df=glb_entity_df, OOB_df=glb_predct_df)
# print(summary(mdl <- ret_lst$model)); 
# print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
#               glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))

if (glb_is_regression) {
    # Highest cor.y
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=glb_feats_df$id[1],
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)
    print(summary(mdl <- ret_lst$model)); 
    print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))

    # Uncorrelated X
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=glb_feats_df$id,
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)
    print(summary(mdl <- ret_lst$model)); 
    print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
    glb_sel_mdl <- mdl
    
    # All X that is not missing
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                             glb_predct_var),
                                                     glb_exclude_vars_as_features),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)
    print(summary(mdl <- ret_lst$model)); 
    print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
              glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))    
}    

#   Classification:
#       Logit Regression:

#       Need to change this to glm
#       Prep non-numerical vars before runing this
# ret_lst <- myrun_mdl_lm(indep_vars_vctr=".", 
#                         fit_df=glb_entity_df, OOB_df=glb_predct_df)
# print(summary(mdl <- ret_lst$model)); 
# print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, 
#               glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))

if (glb_is_classification) {
    # Highest cor.y
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=glb_feats_df$id[1],
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)        
    print(summary(mdl <- ret_lst$model)); 
    print(orderBy(~ -f.score.OOB, 
                  glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))

    # Uncorrelated X
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=glb_feats_df$id,
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)        
    print(summary(mdl <- ret_lst$model)); 
    print(orderBy(~ -f.score.OOB, 
                  glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
    glb_sel_mdl <- mdl
    
    # All X that is not missing
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                             glb_predct_var),
                                                     glb_exclude_vars_as_features),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)        
    print(summary(mdl <- ret_lst$model)); 
    print(orderBy(~ -f.score.OOB, 
                  glb_models_df <- rbind(glb_models_df, ret_lst$models_df)))
}
```

```
## [1] 90
## [1] -0.3064767
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.7529  -0.6537  -0.5721  -0.4885   2.1270  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -54.290680  15.662082  -3.466 0.000528 ***
## Year          0.026574   0.007891   3.367 0.000759 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 824.51  on 901  degrees of freedom
## Residual deviance: 812.85  on 900  degrees of freedom
## AIC: 816.85
## 
## Number of Fisher Scoring iterations: 4
## 
##   feats n.fit R.sq.fit   R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB
## 1  Year   902       NA -0.3064767           NA 6762.359      90
##   f.score.OOB
## 1          NA
## [1] 32
## [1] 0.535475
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -3.07586  -0.24255  -0.05477  -0.00572   3.04791  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.435e+02  4.680e+01  -3.066  0.00217 ** 
## Year         7.622e-02  1.610e-02   4.735 2.19e-06 ***
## RS           6.540e-03  4.288e-03   1.525  0.12718    
## RA          -6.595e-03  4.696e-03  -1.404  0.16023    
## W            3.374e-01  4.612e-02   7.314 2.58e-13 ***
## G           -2.439e-01  2.095e-01  -1.164  0.24443    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 824.51  on 901  degrees of freedom
## Residual deviance: 321.71  on 896  degrees of freedom
## AIC: 333.71
## 
## Number of Fisher Scoring iterations: 8
## 
##                feats n.fit R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit
## 2 Year, RS, RA, W, G   902       NA  0.5354750           NA 28470.053
## 1               Year   902       NA -0.3064767           NA  6762.359
##   SSE.OOB f.score.OOB
## 2      32   0.9490446
## 1      90          NA
## [1] 34
## [1] 0.5064421
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.1912  -0.2350  -0.0518  -0.0053   2.8905  
## 
## Coefficients:
##                 Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.313e+02  4.870e+01  -2.696  0.00701 ** 
## Year           7.736e-02  1.726e-02   4.482 7.39e-06 ***
## RS             1.208e-02  7.178e-03   1.682  0.09248 .  
## RA            -6.462e-03  4.711e-03  -1.372  0.17021    
## W              3.392e-01  4.642e-02   7.308 2.70e-13 ***
## OBP            1.771e+00  2.684e+01   0.066  0.94739    
## SLG           -1.755e+01  1.332e+01  -1.318  0.18747    
## BA             5.832e+00  2.379e+01   0.245  0.80633    
## G             -3.327e-01  2.188e-01  -1.520  0.12842    
## League_fctrNL  2.529e-01  3.203e-01   0.789  0.42988    
## Team_fctr_num  2.107e-02  1.762e-02   1.196  0.23187    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 824.51  on 901  degrees of freedom
## Residual deviance: 317.29  on 891  degrees of freedom
## AIC: 339.29
## 
## Number of Fisher Scoring iterations: 8
## 
##                                                          feats n.fit
## 2                                           Year, RS, RA, W, G   902
## 3 Year, RS, RA, W, OBP, SLG, BA, G, League_fctr, Team_fctr_num   902
## 1                                                         Year   902
##   R.sq.fit   R.sq.OOB Adj.R.sq.fit   SSE.fit SSE.OOB f.score.OOB
## 2       NA  0.5354750           NA 28470.053      32   0.9490446
## 3       NA  0.5064421           NA 38394.881      34   0.9456869
## 1       NA -0.3064767           NA  6762.359      90          NA
```

```r
if (glb_is_regression)
    print(myplot_scatter(glb_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats), data=glb_models_df, color="NavyBlue", 
                    size=3.5))

if (glb_is_classification)
    print(myplot_hbar(df=glb_models_df, xcol_name="feats", ycol_names="f.score.OOB"))
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

![](MoneyBall_Playoffs_files/figure-html/run_models-1.png) 

```r
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
if (glb_is_regression)
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=glb_feats_df$id, fit_df=glb_entity_df)

if (glb_is_classification)
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=glb_feats_df$id, fit_df=glb_entity_df)

print(summary(mdl <- ret_lst$model)); print(glb_sel_mdl_df <- ret_lst$models_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -3.07586  -0.24255  -0.05477  -0.00572   3.04791  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.435e+02  4.680e+01  -3.066  0.00217 ** 
## Year         7.622e-02  1.610e-02   4.735 2.19e-06 ***
## RS           6.540e-03  4.288e-03   1.525  0.12718    
## RA          -6.595e-03  4.696e-03  -1.404  0.16023    
## W            3.374e-01  4.612e-02   7.314 2.58e-13 ***
## G           -2.439e-01  2.095e-01  -1.164  0.24443    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 824.51  on 901  degrees of freedom
## Residual deviance: 321.71  on 896  degrees of freedom
## AIC: 333.71
## 
## Number of Fisher Scoring iterations: 8
```

```
##                feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB
## 1 Year, RS, RA, W, G   902       NA       NA           NA 28470.05      NA
```

```r
glb_sel_mdl <- mdl

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
##   Team Year Playoffs Playoffs.predict
## 1  ARI 2012        0                0
## 2  ATL 2012        1                1
## 3  BAL 2012        1                1
## 4  BOS 2012        0                0
## 5  CHC 2012        0                0
## 6  CHW 2012        0                0
##     Team Year Playoffs Playoffs.predict
## 60   WSN 2011        0                0
## 107  MIN 2009        1                0
## 149  TOR 2008        0                0
## 230  OAK 2005        0                1
## 314  KCR 2002        0                0
## 315  LAD 2002        0                1
##     Team Year Playoffs Playoffs.predict
## 325  SEA 2002        0                1
## 326  SFG 2002        1                1
## 327  STL 2002        1                1
## 328  TBD 2002        0                0
## 329  TEX 2002        0                0
## 330  TOR 2002        0                0
```

```r
if (glb_is_regression)
    print(myplot_scatter(glb_predct_df, glb_predct_var, glb_predct_var_name))

if (glb_is_classification)
#     print(table(glb_predct_df[, glb_predct_var], 
#                 glb_predct_df[, glb_predct_var_name]))
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_predct_df))
```

```
##         Playoffs.predict
## Playoffs   0   1
##        0 216  24
##        1   8  82
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
##  [9] lattice_0.20-30  MASS_7.3-39      Matrix_1.1-5     munsell_0.4.2   
## [13] proto_0.3-10     Rcpp_0.11.4      rmarkdown_0.5.1  scales_0.2.4    
## [17] splines_3.1.2    stringr_0.6.2    tcltk_3.1.2      tools_3.1.2     
## [21] yaml_2.1.13
```
