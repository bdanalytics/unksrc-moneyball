# MoneyBall-Champion: WorldSeries classification
bdanalytics  

**  **    
**Date: (Fri) Mar 27, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/baseball.csv  
    New:        https://courses.edx.org/c4x/MITx/15.071x_2/asset/baseball.csv  
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
#suppressPackageStartupMessages(require())

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_newent_dataset <- TRUE    # or TRUE
glb_split_entity_newent_datasets <- FALSE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size <- 0.25               # > 0 & < 1
glb_split_sample.seed <- 1000               # or any integer 

glb_predct_var <- "WorldSeries"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("Year", "Team")                # or NULL

glb_exclude_vars_as_features <- union(NULL, ".rnorm")     # or NULL                      
# List chrs (convert into factors if it's a valid feature); num/int transformed  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                    c("Team", "Team.fctr", "League", "Playoffs")     # or NULL
                                      )
# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                            c("WorldSeries.predict.proba", "WorldSeries.predict",
                              "RankPlayoffs"
                              ))

glb_impute_na_data <- FALSE            # or TRUE
glb_mice_complete.seed <- 144               # or any integer
glb_is_regression <- FALSE; glb_is_classification <- TRUE

glb_mdl <- glb_sel_mdl <- glb_dmy_mdl <- NULL
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
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/baseball.csv", 
    comment="glb_entity_df", force_header=TRUE,
    print_diagn=(glb_is_separate_newent_dataset | 
                !glb_split_entity_newent_datasets))
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
##  - attr(*, "comment")= chr "glb_entity_df"
## NULL
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(
        url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/baseball.csv", 
        comment="glb_newent_df", force_header=TRUE, print_diagn=TRUE)
} else {
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
                                          max(2, nrow(glb_entity_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newent_df <- do.call("subset", 
                list(glb_entity_df, parse(text=glb_split_newdata_condition)))
            glb_entity_df <- do.call("subset", 
                list(glb_entity_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_entity_df[, glb_predct_var], 
                                      SplitRatio=(1-glb_split_newdata_size))
                glb_newent_df <- glb_entity_df[!split, ] 
                glb_entity_df <- glb_entity_df[split ,]
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample')")   

    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_entity_df)
        str(glb_entity_df)        
    }
}         
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
## 187   CIN     NL 2006 749 801 80 0.336 0.432 0.257        0         NA
## 401   DET     AL 1999 747 882 69 0.326 0.443 0.261        0         NA
## 627   MON     NL 1989 632 630 81 0.319 0.361 0.247        0         NA
## 896   STL     NL 1978 600 657 69 0.303 0.358 0.249        0         NA
## 1217  CIN     NL 1962 802 685 98 0.332 0.417 0.270        0         NA
##      RankPlayoffs   G  OOBP  OSLG
## 43             NA 162 0.337 0.425
## 187            NA 162 0.337 0.457
## 401            NA 161 0.349 0.451
## 627            NA 162    NA    NA
## 896            NA 162    NA    NA
## 1217           NA 162    NA    NA
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
##  - attr(*, "comment")= chr "glb_newent_df"
## NULL
```

```r
glb_entity_df <- subset(glb_entity_df, Playoffs == 1)
glb_newent_df <- subset(glb_newent_df, Playoffs == 1)

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
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

print(PlayoffTable <- table(glb_entity_df$Year))
```

```
## 
## 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1973 1974 1975 1976 1977 
##    2    2    2    2    2    2    2    4    4    4    4    4    4    4    4 
## 1978 1979 1980 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 
##    4    4    4    4    4    4    4    4    4    4    4    4    4    4    4 
## 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
##    8    8    8    8    8    8    8    8    8    8    8    8    8    8    8 
## 2011 2012 
##    8   10
```

```r
print(length(PlayoffTable))
```

```
## [1] 47
```

```r
add_new_diag_feats <- function(obs_df, obs_twin_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

        Team.fctr=factor(Team, 
                    as.factor(union(obs_df$Team, obs_twin_df$Team))), 
        League.fctr=factor(League, 
                    as.factor(union(obs_df$League, obs_twin_df$League))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<max_n_val>"), 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>),
        NumCompetitors = PlayoffTable[as.character(Year)],
        WorldSeries = as.numeric(RankPlayoffs == 1),
        .rnorm=rnorm(1)
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_newent_df)
```

```
## Loading required package: plyr
```

```
##      Team              League               Year            RS        
##  Length:244         Length:244         Min.   :1962   Min.   : 583.0  
##  Class :character   Class :character   1st Qu.:1982   1st Qu.: 730.0  
##  Mode  :character   Mode  :character   Median :1998   Median : 780.5  
##                                        Mean   :1993   Mean   : 786.3  
##                                        3rd Qu.:2005   3rd Qu.: 836.0  
##                                        Max.   :2012   Max.   :1009.0  
##                                                                       
##        RA              W               OBP              SLG        
##  Min.   :472.0   Min.   : 82.00   Min.   :0.2980   Min.   :0.3350  
##  1st Qu.:614.0   1st Qu.: 91.00   1st Qu.:0.3280   1st Qu.:0.3990  
##  Median :661.5   Median : 95.00   Median :0.3380   Median :0.4200  
##  Mean   :666.1   Mean   : 95.12   Mean   :0.3373   Mean   :0.4191  
##  3rd Qu.:711.0   3rd Qu.: 98.00   3rd Qu.:0.3460   3rd Qu.:0.4373  
##  Max.   :903.0   Max.   :116.00   Max.   :0.3730   Max.   :0.4910  
##                                                                    
##        BA            Playoffs   RankSeason     RankPlayoffs  
##  Min.   :0.2350   Min.   :1   Min.   :1.000   Min.   :1.000  
##  1st Qu.:0.2597   1st Qu.:1   1st Qu.:2.000   1st Qu.:2.000  
##  Median :0.2670   Median :1   Median :3.000   Median :3.000  
##  Mean   :0.2668   Mean   :1   Mean   :3.123   Mean   :2.717  
##  3rd Qu.:0.2740   3rd Qu.:1   3rd Qu.:4.000   3rd Qu.:4.000  
##  Max.   :0.2930   Max.   :1   Max.   :8.000   Max.   :5.000  
##                                                              
##        G              OOBP             OSLG          Team.fctr  
##  Min.   :158.0   Min.   :0.2960   Min.   :0.3610   NYY    : 23  
##  1st Qu.:162.0   1st Qu.:0.3140   1st Qu.:0.3920   ATL    : 17  
##  Median :162.0   Median :0.3210   Median :0.4035   STL    : 16  
##  Mean   :161.9   Mean   :0.3214   Mean   :0.4038   OAK    : 14  
##  3rd Qu.:162.0   3rd Qu.:0.3280   3rd Qu.:0.4170   LAD    : 14  
##  Max.   :165.0   Max.   :0.3480   Max.   :0.4590   BOS    : 13  
##                  NA's   :130      NA's   :130      (Other):147  
##  League.fctr NumCompetitors   WorldSeries         .rnorm      
##  NL:122      Min.   : 2.00   Min.   :0.0000   Min.   :0.6301  
##  AL:122      1st Qu.: 4.00   1st Qu.:0.0000   1st Qu.:0.6301  
##              Median : 8.00   Median :0.0000   Median :0.6301  
##              Mean   : 6.23   Mean   :0.1926   Mean   :0.6301  
##              3rd Qu.: 8.00   3rd Qu.:0.0000   3rd Qu.:0.6301  
##              Max.   :10.00   Max.   :1.0000   Max.   :0.6301  
##                                                               
##           Team         League           Year             RS             RA 
##              0              0              0              0              0 
##              W            OBP            SLG             BA       Playoffs 
##              0              0              0              0              0 
##     RankSeason   RankPlayoffs              G           OOBP           OSLG 
##              0              0              0            130            130 
##      Team.fctr    League.fctr NumCompetitors    WorldSeries         .rnorm 
##              0              0              0              0              0
```

```r
glb_newent_df <- add_new_diag_feats(glb_newent_df, glb_entity_df)
```

```
##      Team              League               Year            RS        
##  Length:244         Length:244         Min.   :1962   Min.   : 583.0  
##  Class :character   Class :character   1st Qu.:1982   1st Qu.: 730.0  
##  Mode  :character   Mode  :character   Median :1998   Median : 780.5  
##                                        Mean   :1993   Mean   : 786.3  
##                                        3rd Qu.:2005   3rd Qu.: 836.0  
##                                        Max.   :2012   Max.   :1009.0  
##                                                                       
##        RA              W               OBP              SLG        
##  Min.   :472.0   Min.   : 82.00   Min.   :0.2980   Min.   :0.3350  
##  1st Qu.:614.0   1st Qu.: 91.00   1st Qu.:0.3280   1st Qu.:0.3990  
##  Median :661.5   Median : 95.00   Median :0.3380   Median :0.4200  
##  Mean   :666.1   Mean   : 95.12   Mean   :0.3373   Mean   :0.4191  
##  3rd Qu.:711.0   3rd Qu.: 98.00   3rd Qu.:0.3460   3rd Qu.:0.4373  
##  Max.   :903.0   Max.   :116.00   Max.   :0.3730   Max.   :0.4910  
##                                                                    
##        BA            Playoffs   RankSeason     RankPlayoffs  
##  Min.   :0.2350   Min.   :1   Min.   :1.000   Min.   :1.000  
##  1st Qu.:0.2597   1st Qu.:1   1st Qu.:2.000   1st Qu.:2.000  
##  Median :0.2670   Median :1   Median :3.000   Median :3.000  
##  Mean   :0.2668   Mean   :1   Mean   :3.123   Mean   :2.717  
##  3rd Qu.:0.2740   3rd Qu.:1   3rd Qu.:4.000   3rd Qu.:4.000  
##  Max.   :0.2930   Max.   :1   Max.   :8.000   Max.   :5.000  
##                                                              
##        G              OOBP             OSLG          Team.fctr  
##  Min.   :158.0   Min.   :0.2960   Min.   :0.3610   NYY    : 23  
##  1st Qu.:162.0   1st Qu.:0.3140   1st Qu.:0.3920   ATL    : 17  
##  Median :162.0   Median :0.3210   Median :0.4035   STL    : 16  
##  Mean   :161.9   Mean   :0.3214   Mean   :0.4038   OAK    : 14  
##  3rd Qu.:162.0   3rd Qu.:0.3280   3rd Qu.:0.4170   LAD    : 14  
##  Max.   :165.0   Max.   :0.3480   Max.   :0.4590   BOS    : 13  
##                  NA's   :130      NA's   :130      (Other):147  
##  League.fctr NumCompetitors   WorldSeries         .rnorm       
##  NL:122      Min.   : 2.00   Min.   :0.0000   Min.   :-0.2762  
##  AL:122      1st Qu.: 4.00   1st Qu.:0.0000   1st Qu.:-0.2762  
##              Median : 8.00   Median :0.0000   Median :-0.2762  
##              Mean   : 6.23   Mean   :0.1926   Mean   :-0.2762  
##              3rd Qu.: 8.00   3rd Qu.:0.0000   3rd Qu.:-0.2762  
##              Max.   :10.00   Max.   :1.0000   Max.   :-0.2762  
##                                                                
##           Team         League           Year             RS             RA 
##              0              0              0              0              0 
##              W            OBP            SLG             BA       Playoffs 
##              0              0              0              0              0 
##     RankSeason   RankPlayoffs              G           OOBP           OSLG 
##              0              0              0            130            130 
##      Team.fctr    League.fctr NumCompetitors    WorldSeries         .rnorm 
##              0              0              0              0              0
```

```r
print(nrow(subset(glb_entity_df, NumCompetitors == 8)))
```

```
## [1] 128
```

```r
# Histogram of predictor in glb_entity_df & glb_newent_df
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](MoneyBall_Champ_files/figure-html/inspect_explore_data_1-1.png) 

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

# Check for glb_newent_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
print(table(glb_entity_df$WorldSeries))
```

```
## 
##   0   1 
## 197  47
```

```r
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(table(sign(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(mycreate_xtab(glb_entity_df, <col1_name>))
# print(mycreate_xtab(glb_entity_df, c(<col1_name>, <col2_name>)))
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
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>"))

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
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function(entity_df, newent_df) {
    if (!glb_is_separate_newent_dataset) {
        # Combine entity & newent
        union_df <- rbind(mutate(entity_df, .src = "entity"),
                          mutate(newent_df, .src = "newent"))
        union_imputed_df <- union_df[, setdiff(setdiff(names(entity_df), 
                                                       glb_predct_var), 
                                               glb_exclude_vars_as_features)]
        print(summary(union_imputed_df))
    
        require(mice)
        set.seed(glb_mice_complete.seed)
        union_imputed_df <- complete(mice(union_imputed_df))
        print(summary(union_imputed_df))
    
        union_df[, names(union_imputed_df)] <- union_imputed_df[, names(union_imputed_df)]
        print(summary(union_df))
#         union_df$.rownames <- rownames(union_df)
#         union_df <- orderBy(~.rownames, union_df)
#         
#         imp_entity_df <- myimport_data(
#             url="<imputed_trnng_url>", 
#             comment="imp_entity_df", force_header=TRUE, print_diagn=TRUE)
#         print(all.equal(subset(union_df, select=-c(.src, .rownames, .rnorm)), 
#                         imp_entity_df))
        
        # Partition again
        glb_entity_df <<- subset(union_df, .src == "entity", select=-c(.src, .rownames))
        comment(glb_entity_df) <- "entity_df"
        glb_newent_df <<- subset(union_df, .src == "newent", select=-c(.src, .rownames))
        comment(glb_newent_df) <- "newent_df"
        
        # Generate summaries
        print(summary(entity_df))
        print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
        print(summary(newent_df))
        print(sapply(names(newent_df), function(col) sum(is.na(newent_df[, col]))))
    
    } else stop("Not implemented yet")
}

if (glb_impute_na_data) {
    if ((sum(sapply(names(glb_entity_df), 
                    function(col) sum(is.na(glb_entity_df[, col])))) > 0) | 
        (sum(sapply(names(glb_newent_df), 
                    function(col) sum(is.na(glb_newent_df[, col])))) > 0))
        glb_impute_missing_data(glb_entity_df, glb_newent_df)
}    

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
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))

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
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_newent_df <- mutate(glb_newent_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_newent_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

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
print(glb_feats_df <- 
    myselect_features(glb_entity_df, glb_exclude_vars_as_features, glb_predct_var))
```

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## Year                     Year -0.21348886 0.21348886
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## SLG                       SLG -0.12636418 0.12636418
## W                           W  0.12278172 0.12278172
## RS                         RS -0.08209385 0.08209385
## OBP                       OBP -0.06707138 0.06707138
## G                           G  0.04099881 0.04099881
## OSLG                     OSLG -0.02897498 0.02897498
## BA                         BA -0.01304152 0.01304152
## OOBP                     OOBP -0.01158587 0.01158587
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
print(glb_feats_df <- orderBy(~-cor.y, merge(glb_feats_df, 
          mydelete_cor_features(glb_feats_df, glb_entity_df, glb_predct_var, 
                                glb_exclude_vars_as_features), 
          all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##                NumCompetitors        Year         RA   RankSeason
## NumCompetitors     1.00000000  0.91395477  0.5136769  0.424739274
## Year               0.91395477  1.00000000  0.4762422  0.385219061
## RA                 0.51367693  0.47624216  1.0000000  0.399141259
## RankSeason         0.42473927  0.38521906  0.3991413  1.000000000
## SLG                0.59428908  0.53209625  0.7416220  0.126663174
## W                 -0.18500429 -0.21523262 -0.3755877 -0.755097968
## RS                 0.42438539  0.35067702  0.7384681 -0.027686978
## OBP                0.38655150  0.32152566  0.6447459  0.001611596
## G                  0.03702099  0.05709546  0.1113959  0.069221571
## OSLG              -0.17459618 -0.21875941  0.8384120  0.050110942
## BA                 0.28935051  0.23127163  0.5985474  0.035452231
## OOBP              -0.33556517 -0.37835120  0.8225277  0.097731338
##                       SLG           W          RS          OBP
## NumCompetitors 0.59428908 -0.18500429  0.42438539  0.386551499
## Year           0.53209625 -0.21523262  0.35067702  0.321525660
## RA             0.74162197 -0.37558768  0.73846809  0.644745931
## RankSeason     0.12666317 -0.75509797 -0.02768698  0.001611596
## SLG            1.00000000  0.08047943  0.88996256  0.743176804
## W              0.08047943  1.00000000  0.20129354  0.147780721
## RS             0.88996256  0.20129354  1.00000000  0.884996608
## OBP            0.74317680  0.14778072  0.88499661  1.000000000
## G              0.03327996 -0.05347649  0.05652635 -0.033628959
## OSLG           0.52069228 -0.22738914  0.52227393  0.451819140
## BA             0.72174009  0.08729380  0.78759238  0.809307129
## OOBP           0.51402429 -0.27611783  0.56060255  0.554631331
##                           G        OSLG           BA        OOBP
## NumCompetitors  0.037020990 -0.17459618  0.289350506 -0.33556517
## Year            0.057095457 -0.21875941  0.231271625 -0.37835120
## RA              0.111395871  0.83841197  0.598547374  0.82252766
## RankSeason      0.069221571  0.05011094  0.035452231  0.09773134
## SLG             0.033279958  0.52069228  0.721740095  0.51402429
## W              -0.053476490 -0.22738914  0.087293799 -0.27611783
## RS              0.056526345  0.52227393  0.787592379  0.56060255
## OBP            -0.033628959  0.45181914  0.809307129  0.55463133
## G               1.000000000  0.05405720 -0.006102516 -0.03912611
## OSLG            0.054057203  1.00000000  0.426570622  0.72887539
## BA             -0.006102516  0.42657062  1.000000000  0.42477064
## OOBP           -0.039126108  0.72887539  0.424770643  1.00000000
##                NumCompetitors       Year        RA  RankSeason        SLG
## NumCompetitors     0.00000000 0.91395477 0.5136769 0.424739274 0.59428908
## Year               0.91395477 0.00000000 0.4762422 0.385219061 0.53209625
## RA                 0.51367693 0.47624216 0.0000000 0.399141259 0.74162197
## RankSeason         0.42473927 0.38521906 0.3991413 0.000000000 0.12666317
## SLG                0.59428908 0.53209625 0.7416220 0.126663174 0.00000000
## W                  0.18500429 0.21523262 0.3755877 0.755097968 0.08047943
## RS                 0.42438539 0.35067702 0.7384681 0.027686978 0.88996256
## OBP                0.38655150 0.32152566 0.6447459 0.001611596 0.74317680
## G                  0.03702099 0.05709546 0.1113959 0.069221571 0.03327996
## OSLG               0.17459618 0.21875941 0.8384120 0.050110942 0.52069228
## BA                 0.28935051 0.23127163 0.5985474 0.035452231 0.72174009
## OOBP               0.33556517 0.37835120 0.8225277 0.097731338 0.51402429
##                         W         RS         OBP           G       OSLG
## NumCompetitors 0.18500429 0.42438539 0.386551499 0.037020990 0.17459618
## Year           0.21523262 0.35067702 0.321525660 0.057095457 0.21875941
## RA             0.37558768 0.73846809 0.644745931 0.111395871 0.83841197
## RankSeason     0.75509797 0.02768698 0.001611596 0.069221571 0.05011094
## SLG            0.08047943 0.88996256 0.743176804 0.033279958 0.52069228
## W              0.00000000 0.20129354 0.147780721 0.053476490 0.22738914
## RS             0.20129354 0.00000000 0.884996608 0.056526345 0.52227393
## OBP            0.14778072 0.88499661 0.000000000 0.033628959 0.45181914
## G              0.05347649 0.05652635 0.033628959 0.000000000 0.05405720
## OSLG           0.22738914 0.52227393 0.451819140 0.054057203 0.00000000
## BA             0.08729380 0.78759238 0.809307129 0.006102516 0.42657062
## OOBP           0.27611783 0.56060255 0.554631331 0.039126108 0.72887539
##                         BA       OOBP
## NumCompetitors 0.289350506 0.33556517
## Year           0.231271625 0.37835120
## RA             0.598547374 0.82252766
## RankSeason     0.035452231 0.09773134
## SLG            0.721740095 0.51402429
## W              0.087293799 0.27611783
## RS             0.787592379 0.56060255
## OBP            0.809307129 0.55463133
## G              0.006102516 0.03912611
## OSLG           0.426570622 0.72887539
## BA             0.000000000 0.42477064
## OOBP           0.424770643 0.00000000
## [1] "cor(NumCompetitors, Year)=0.9140"
```

```
## [1] "cor(WorldSeries, NumCompetitors)=-0.2239"
## [1] "cor(WorldSeries, Year)=-0.2135"
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-1.png) 

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
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping Year as a feature
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-2.png) 

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## SLG                       SLG -0.12636418 0.12636418
## W                           W  0.12278172 0.12278172
## RS                         RS -0.08209385 0.08209385
## OBP                       OBP -0.06707138 0.06707138
## G                           G  0.04099881 0.04099881
## OSLG                     OSLG -0.02897498 0.02897498
## BA                         BA -0.01304152 0.01304152
## OOBP                     OOBP -0.01158587 0.01158587
##                NumCompetitors         RA   RankSeason        SLG
## NumCompetitors     1.00000000  0.5136769  0.424739274 0.59428908
## RA                 0.51367693  1.0000000  0.399141259 0.74162197
## RankSeason         0.42473927  0.3991413  1.000000000 0.12666317
## SLG                0.59428908  0.7416220  0.126663174 1.00000000
## W                 -0.18500429 -0.3755877 -0.755097968 0.08047943
## RS                 0.42438539  0.7384681 -0.027686978 0.88996256
## OBP                0.38655150  0.6447459  0.001611596 0.74317680
## G                  0.03702099  0.1113959  0.069221571 0.03327996
## OSLG              -0.17459618  0.8384120  0.050110942 0.52069228
## BA                 0.28935051  0.5985474  0.035452231 0.72174009
## OOBP              -0.33556517  0.8225277  0.097731338 0.51402429
##                          W          RS          OBP            G
## NumCompetitors -0.18500429  0.42438539  0.386551499  0.037020990
## RA             -0.37558768  0.73846809  0.644745931  0.111395871
## RankSeason     -0.75509797 -0.02768698  0.001611596  0.069221571
## SLG             0.08047943  0.88996256  0.743176804  0.033279958
## W               1.00000000  0.20129354  0.147780721 -0.053476490
## RS              0.20129354  1.00000000  0.884996608  0.056526345
## OBP             0.14778072  0.88499661  1.000000000 -0.033628959
## G              -0.05347649  0.05652635 -0.033628959  1.000000000
## OSLG           -0.22738914  0.52227393  0.451819140  0.054057203
## BA              0.08729380  0.78759238  0.809307129 -0.006102516
## OOBP           -0.27611783  0.56060255  0.554631331 -0.039126108
##                       OSLG           BA        OOBP
## NumCompetitors -0.17459618  0.289350506 -0.33556517
## RA              0.83841197  0.598547374  0.82252766
## RankSeason      0.05011094  0.035452231  0.09773134
## SLG             0.52069228  0.721740095  0.51402429
## W              -0.22738914  0.087293799 -0.27611783
## RS              0.52227393  0.787592379  0.56060255
## OBP             0.45181914  0.809307129  0.55463133
## G               0.05405720 -0.006102516 -0.03912611
## OSLG            1.00000000  0.426570622  0.72887539
## BA              0.42657062  1.000000000  0.42477064
## OOBP            0.72887539  0.424770643  1.00000000
##                NumCompetitors        RA  RankSeason        SLG          W
## NumCompetitors     0.00000000 0.5136769 0.424739274 0.59428908 0.18500429
## RA                 0.51367693 0.0000000 0.399141259 0.74162197 0.37558768
## RankSeason         0.42473927 0.3991413 0.000000000 0.12666317 0.75509797
## SLG                0.59428908 0.7416220 0.126663174 0.00000000 0.08047943
## W                  0.18500429 0.3755877 0.755097968 0.08047943 0.00000000
## RS                 0.42438539 0.7384681 0.027686978 0.88996256 0.20129354
## OBP                0.38655150 0.6447459 0.001611596 0.74317680 0.14778072
## G                  0.03702099 0.1113959 0.069221571 0.03327996 0.05347649
## OSLG               0.17459618 0.8384120 0.050110942 0.52069228 0.22738914
## BA                 0.28935051 0.5985474 0.035452231 0.72174009 0.08729380
## OOBP               0.33556517 0.8225277 0.097731338 0.51402429 0.27611783
##                        RS         OBP           G       OSLG          BA
## NumCompetitors 0.42438539 0.386551499 0.037020990 0.17459618 0.289350506
## RA             0.73846809 0.644745931 0.111395871 0.83841197 0.598547374
## RankSeason     0.02768698 0.001611596 0.069221571 0.05011094 0.035452231
## SLG            0.88996256 0.743176804 0.033279958 0.52069228 0.721740095
## W              0.20129354 0.147780721 0.053476490 0.22738914 0.087293799
## RS             0.00000000 0.884996608 0.056526345 0.52227393 0.787592379
## OBP            0.88499661 0.000000000 0.033628959 0.45181914 0.809307129
## G              0.05652635 0.033628959 0.000000000 0.05405720 0.006102516
## OSLG           0.52227393 0.451819140 0.054057203 0.00000000 0.426570622
## BA             0.78759238 0.809307129 0.006102516 0.42657062 0.000000000
## OOBP           0.56060255 0.554631331 0.039126108 0.72887539 0.424770643
##                      OOBP
## NumCompetitors 0.33556517
## RA             0.82252766
## RankSeason     0.09773134
## SLG            0.51402429
## W              0.27611783
## RS             0.56060255
## OBP            0.55463133
## G              0.03912611
## OSLG           0.72887539
## BA             0.42477064
## OOBP           0.00000000
## [1] "cor(SLG, RS)=0.8900"
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-3.png) 

```
## [1] "cor(WorldSeries, SLG)=-0.1264"
## [1] "cor(WorldSeries, RS)=-0.0821"
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
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping RS as a feature
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-4.png) 

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## SLG                       SLG -0.12636418 0.12636418
## W                           W  0.12278172 0.12278172
## OBP                       OBP -0.06707138 0.06707138
## G                           G  0.04099881 0.04099881
## OSLG                     OSLG -0.02897498 0.02897498
## BA                         BA -0.01304152 0.01304152
## OOBP                     OOBP -0.01158587 0.01158587
##                NumCompetitors         RA   RankSeason        SLG
## NumCompetitors     1.00000000  0.5136769  0.424739274 0.59428908
## RA                 0.51367693  1.0000000  0.399141259 0.74162197
## RankSeason         0.42473927  0.3991413  1.000000000 0.12666317
## SLG                0.59428908  0.7416220  0.126663174 1.00000000
## W                 -0.18500429 -0.3755877 -0.755097968 0.08047943
## OBP                0.38655150  0.6447459  0.001611596 0.74317680
## G                  0.03702099  0.1113959  0.069221571 0.03327996
## OSLG              -0.17459618  0.8384120  0.050110942 0.52069228
## BA                 0.28935051  0.5985474  0.035452231 0.72174009
## OOBP              -0.33556517  0.8225277  0.097731338 0.51402429
##                          W          OBP            G        OSLG
## NumCompetitors -0.18500429  0.386551499  0.037020990 -0.17459618
## RA             -0.37558768  0.644745931  0.111395871  0.83841197
## RankSeason     -0.75509797  0.001611596  0.069221571  0.05011094
## SLG             0.08047943  0.743176804  0.033279958  0.52069228
## W               1.00000000  0.147780721 -0.053476490 -0.22738914
## OBP             0.14778072  1.000000000 -0.033628959  0.45181914
## G              -0.05347649 -0.033628959  1.000000000  0.05405720
## OSLG           -0.22738914  0.451819140  0.054057203  1.00000000
## BA              0.08729380  0.809307129 -0.006102516  0.42657062
## OOBP           -0.27611783  0.554631331 -0.039126108  0.72887539
##                          BA        OOBP
## NumCompetitors  0.289350506 -0.33556517
## RA              0.598547374  0.82252766
## RankSeason      0.035452231  0.09773134
## SLG             0.721740095  0.51402429
## W               0.087293799 -0.27611783
## OBP             0.809307129  0.55463133
## G              -0.006102516 -0.03912611
## OSLG            0.426570622  0.72887539
## BA              1.000000000  0.42477064
## OOBP            0.424770643  1.00000000
##                NumCompetitors        RA  RankSeason        SLG          W
## NumCompetitors     0.00000000 0.5136769 0.424739274 0.59428908 0.18500429
## RA                 0.51367693 0.0000000 0.399141259 0.74162197 0.37558768
## RankSeason         0.42473927 0.3991413 0.000000000 0.12666317 0.75509797
## SLG                0.59428908 0.7416220 0.126663174 0.00000000 0.08047943
## W                  0.18500429 0.3755877 0.755097968 0.08047943 0.00000000
## OBP                0.38655150 0.6447459 0.001611596 0.74317680 0.14778072
## G                  0.03702099 0.1113959 0.069221571 0.03327996 0.05347649
## OSLG               0.17459618 0.8384120 0.050110942 0.52069228 0.22738914
## BA                 0.28935051 0.5985474 0.035452231 0.72174009 0.08729380
## OOBP               0.33556517 0.8225277 0.097731338 0.51402429 0.27611783
##                        OBP           G       OSLG          BA       OOBP
## NumCompetitors 0.386551499 0.037020990 0.17459618 0.289350506 0.33556517
## RA             0.644745931 0.111395871 0.83841197 0.598547374 0.82252766
## RankSeason     0.001611596 0.069221571 0.05011094 0.035452231 0.09773134
## SLG            0.743176804 0.033279958 0.52069228 0.721740095 0.51402429
## W              0.147780721 0.053476490 0.22738914 0.087293799 0.27611783
## OBP            0.000000000 0.033628959 0.45181914 0.809307129 0.55463133
## G              0.033628959 0.000000000 0.05405720 0.006102516 0.03912611
## OSLG           0.451819140 0.054057203 0.00000000 0.426570622 0.72887539
## BA             0.809307129 0.006102516 0.42657062 0.000000000 0.42477064
## OOBP           0.554631331 0.039126108 0.72887539 0.424770643 0.00000000
## [1] "cor(RA, OSLG)=0.8384"
```

```
## Warning: Removed 130 rows containing missing values (geom_point).
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-5.png) 

```
## [1] "cor(WorldSeries, RA)=-0.1439"
## [1] "cor(WorldSeries, OSLG)=-0.0290"
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
## Warning: Removed 130 rows containing missing values (stat_smooth).
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
## Warning: Removed 130 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 130 rows containing missing values (geom_point).
```

```
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping OSLG as a feature
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-6.png) 

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## SLG                       SLG -0.12636418 0.12636418
## W                           W  0.12278172 0.12278172
## OBP                       OBP -0.06707138 0.06707138
## G                           G  0.04099881 0.04099881
## BA                         BA -0.01304152 0.01304152
## OOBP                     OOBP -0.01158587 0.01158587
##                NumCompetitors         RA   RankSeason        SLG
## NumCompetitors     1.00000000  0.5136769  0.424739274 0.59428908
## RA                 0.51367693  1.0000000  0.399141259 0.74162197
## RankSeason         0.42473927  0.3991413  1.000000000 0.12666317
## SLG                0.59428908  0.7416220  0.126663174 1.00000000
## W                 -0.18500429 -0.3755877 -0.755097968 0.08047943
## OBP                0.38655150  0.6447459  0.001611596 0.74317680
## G                  0.03702099  0.1113959  0.069221571 0.03327996
## BA                 0.28935051  0.5985474  0.035452231 0.72174009
## OOBP              -0.33556517  0.8225277  0.097731338 0.51402429
##                          W          OBP            G           BA
## NumCompetitors -0.18500429  0.386551499  0.037020990  0.289350506
## RA             -0.37558768  0.644745931  0.111395871  0.598547374
## RankSeason     -0.75509797  0.001611596  0.069221571  0.035452231
## SLG             0.08047943  0.743176804  0.033279958  0.721740095
## W               1.00000000  0.147780721 -0.053476490  0.087293799
## OBP             0.14778072  1.000000000 -0.033628959  0.809307129
## G              -0.05347649 -0.033628959  1.000000000 -0.006102516
## BA              0.08729380  0.809307129 -0.006102516  1.000000000
## OOBP           -0.27611783  0.554631331 -0.039126108  0.424770643
##                       OOBP
## NumCompetitors -0.33556517
## RA              0.82252766
## RankSeason      0.09773134
## SLG             0.51402429
## W              -0.27611783
## OBP             0.55463133
## G              -0.03912611
## BA              0.42477064
## OOBP            1.00000000
##                NumCompetitors        RA  RankSeason        SLG          W
## NumCompetitors     0.00000000 0.5136769 0.424739274 0.59428908 0.18500429
## RA                 0.51367693 0.0000000 0.399141259 0.74162197 0.37558768
## RankSeason         0.42473927 0.3991413 0.000000000 0.12666317 0.75509797
## SLG                0.59428908 0.7416220 0.126663174 0.00000000 0.08047943
## W                  0.18500429 0.3755877 0.755097968 0.08047943 0.00000000
## OBP                0.38655150 0.6447459 0.001611596 0.74317680 0.14778072
## G                  0.03702099 0.1113959 0.069221571 0.03327996 0.05347649
## BA                 0.28935051 0.5985474 0.035452231 0.72174009 0.08729380
## OOBP               0.33556517 0.8225277 0.097731338 0.51402429 0.27611783
##                        OBP           G          BA       OOBP
## NumCompetitors 0.386551499 0.037020990 0.289350506 0.33556517
## RA             0.644745931 0.111395871 0.598547374 0.82252766
## RankSeason     0.001611596 0.069221571 0.035452231 0.09773134
## SLG            0.743176804 0.033279958 0.721740095 0.51402429
## W              0.147780721 0.053476490 0.087293799 0.27611783
## OBP            0.000000000 0.033628959 0.809307129 0.55463133
## G              0.033628959 0.000000000 0.006102516 0.03912611
## BA             0.809307129 0.006102516 0.000000000 0.42477064
## OOBP           0.554631331 0.039126108 0.424770643 0.00000000
## [1] "cor(RA, OOBP)=0.8225"
```

```
## Warning: Removed 130 rows containing missing values (geom_point).
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-7.png) 

```
## [1] "cor(WorldSeries, RA)=-0.1439"
## [1] "cor(WorldSeries, OOBP)=-0.0116"
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
## Warning: Removed 130 rows containing missing values (stat_smooth).
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
## Warning: Removed 130 rows containing missing values (stat_smooth).
```

```
## Warning: Removed 130 rows containing missing values (geom_point).
```

```
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping OOBP as a feature
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-8.png) 

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## SLG                       SLG -0.12636418 0.12636418
## W                           W  0.12278172 0.12278172
## OBP                       OBP -0.06707138 0.06707138
## G                           G  0.04099881 0.04099881
## BA                         BA -0.01304152 0.01304152
##                NumCompetitors         RA   RankSeason        SLG
## NumCompetitors     1.00000000  0.5136769  0.424739274 0.59428908
## RA                 0.51367693  1.0000000  0.399141259 0.74162197
## RankSeason         0.42473927  0.3991413  1.000000000 0.12666317
## SLG                0.59428908  0.7416220  0.126663174 1.00000000
## W                 -0.18500429 -0.3755877 -0.755097968 0.08047943
## OBP                0.38655150  0.6447459  0.001611596 0.74317680
## G                  0.03702099  0.1113959  0.069221571 0.03327996
## BA                 0.28935051  0.5985474  0.035452231 0.72174009
##                          W          OBP            G           BA
## NumCompetitors -0.18500429  0.386551499  0.037020990  0.289350506
## RA             -0.37558768  0.644745931  0.111395871  0.598547374
## RankSeason     -0.75509797  0.001611596  0.069221571  0.035452231
## SLG             0.08047943  0.743176804  0.033279958  0.721740095
## W               1.00000000  0.147780721 -0.053476490  0.087293799
## OBP             0.14778072  1.000000000 -0.033628959  0.809307129
## G              -0.05347649 -0.033628959  1.000000000 -0.006102516
## BA              0.08729380  0.809307129 -0.006102516  1.000000000
##                NumCompetitors        RA  RankSeason        SLG          W
## NumCompetitors     0.00000000 0.5136769 0.424739274 0.59428908 0.18500429
## RA                 0.51367693 0.0000000 0.399141259 0.74162197 0.37558768
## RankSeason         0.42473927 0.3991413 0.000000000 0.12666317 0.75509797
## SLG                0.59428908 0.7416220 0.126663174 0.00000000 0.08047943
## W                  0.18500429 0.3755877 0.755097968 0.08047943 0.00000000
## OBP                0.38655150 0.6447459 0.001611596 0.74317680 0.14778072
## G                  0.03702099 0.1113959 0.069221571 0.03327996 0.05347649
## BA                 0.28935051 0.5985474 0.035452231 0.72174009 0.08729380
##                        OBP           G          BA
## NumCompetitors 0.386551499 0.037020990 0.289350506
## RA             0.644745931 0.111395871 0.598547374
## RankSeason     0.001611596 0.069221571 0.035452231
## SLG            0.743176804 0.033279958 0.721740095
## W              0.147780721 0.053476490 0.087293799
## OBP            0.000000000 0.033628959 0.809307129
## G              0.033628959 0.000000000 0.006102516
## BA             0.809307129 0.006102516 0.000000000
## [1] "cor(OBP, BA)=0.8093"
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-9.png) 

```
## [1] "cor(WorldSeries, OBP)=-0.0671"
## [1] "cor(WorldSeries, BA)=-0.0130"
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
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping BA as a feature
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-10.png) 

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## SLG                       SLG -0.12636418 0.12636418
## W                           W  0.12278172 0.12278172
## OBP                       OBP -0.06707138 0.06707138
## G                           G  0.04099881 0.04099881
##                NumCompetitors         RA   RankSeason        SLG
## NumCompetitors     1.00000000  0.5136769  0.424739274 0.59428908
## RA                 0.51367693  1.0000000  0.399141259 0.74162197
## RankSeason         0.42473927  0.3991413  1.000000000 0.12666317
## SLG                0.59428908  0.7416220  0.126663174 1.00000000
## W                 -0.18500429 -0.3755877 -0.755097968 0.08047943
## OBP                0.38655150  0.6447459  0.001611596 0.74317680
## G                  0.03702099  0.1113959  0.069221571 0.03327996
##                          W          OBP           G
## NumCompetitors -0.18500429  0.386551499  0.03702099
## RA             -0.37558768  0.644745931  0.11139587
## RankSeason     -0.75509797  0.001611596  0.06922157
## SLG             0.08047943  0.743176804  0.03327996
## W               1.00000000  0.147780721 -0.05347649
## OBP             0.14778072  1.000000000 -0.03362896
## G              -0.05347649 -0.033628959  1.00000000
##                NumCompetitors        RA  RankSeason        SLG          W
## NumCompetitors     0.00000000 0.5136769 0.424739274 0.59428908 0.18500429
## RA                 0.51367693 0.0000000 0.399141259 0.74162197 0.37558768
## RankSeason         0.42473927 0.3991413 0.000000000 0.12666317 0.75509797
## SLG                0.59428908 0.7416220 0.126663174 0.00000000 0.08047943
## W                  0.18500429 0.3755877 0.755097968 0.08047943 0.00000000
## OBP                0.38655150 0.6447459 0.001611596 0.74317680 0.14778072
## G                  0.03702099 0.1113959 0.069221571 0.03327996 0.05347649
##                        OBP          G
## NumCompetitors 0.386551499 0.03702099
## RA             0.644745931 0.11139587
## RankSeason     0.001611596 0.06922157
## SLG            0.743176804 0.03327996
## W              0.147780721 0.05347649
## OBP            0.000000000 0.03362896
## G              0.033628959 0.00000000
## [1] "cor(RankSeason, W)=-0.7551"
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-11.png) 

```
## [1] "cor(WorldSeries, RankSeason)=-0.1305"
## [1] "cor(WorldSeries, W)=0.1228"
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
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping W as a feature
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-12.png) 

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## SLG                       SLG -0.12636418 0.12636418
## OBP                       OBP -0.06707138 0.06707138
## G                           G  0.04099881 0.04099881
##                NumCompetitors        RA  RankSeason        SLG
## NumCompetitors     1.00000000 0.5136769 0.424739274 0.59428908
## RA                 0.51367693 1.0000000 0.399141259 0.74162197
## RankSeason         0.42473927 0.3991413 1.000000000 0.12666317
## SLG                0.59428908 0.7416220 0.126663174 1.00000000
## OBP                0.38655150 0.6447459 0.001611596 0.74317680
## G                  0.03702099 0.1113959 0.069221571 0.03327996
##                         OBP           G
## NumCompetitors  0.386551499  0.03702099
## RA              0.644745931  0.11139587
## RankSeason      0.001611596  0.06922157
## SLG             0.743176804  0.03327996
## OBP             1.000000000 -0.03362896
## G              -0.033628959  1.00000000
##                NumCompetitors        RA  RankSeason        SLG         OBP
## NumCompetitors     0.00000000 0.5136769 0.424739274 0.59428908 0.386551499
## RA                 0.51367693 0.0000000 0.399141259 0.74162197 0.644745931
## RankSeason         0.42473927 0.3991413 0.000000000 0.12666317 0.001611596
## SLG                0.59428908 0.7416220 0.126663174 0.00000000 0.743176804
## OBP                0.38655150 0.6447459 0.001611596 0.74317680 0.000000000
## G                  0.03702099 0.1113959 0.069221571 0.03327996 0.033628959
##                         G
## NumCompetitors 0.03702099
## RA             0.11139587
## RankSeason     0.06922157
## SLG            0.03327996
## OBP            0.03362896
## G              0.00000000
## [1] "cor(SLG, OBP)=0.7432"
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-13.png) 

```
## [1] "cor(WorldSeries, SLG)=-0.1264"
## [1] "cor(WorldSeries, OBP)=-0.0671"
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
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping OBP as a feature
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-14.png) 

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## SLG                       SLG -0.12636418 0.12636418
## G                           G  0.04099881 0.04099881
##                NumCompetitors        RA RankSeason        SLG          G
## NumCompetitors     1.00000000 0.5136769 0.42473927 0.59428908 0.03702099
## RA                 0.51367693 1.0000000 0.39914126 0.74162197 0.11139587
## RankSeason         0.42473927 0.3991413 1.00000000 0.12666317 0.06922157
## SLG                0.59428908 0.7416220 0.12666317 1.00000000 0.03327996
## G                  0.03702099 0.1113959 0.06922157 0.03327996 1.00000000
##                NumCompetitors        RA RankSeason        SLG          G
## NumCompetitors     0.00000000 0.5136769 0.42473927 0.59428908 0.03702099
## RA                 0.51367693 0.0000000 0.39914126 0.74162197 0.11139587
## RankSeason         0.42473927 0.3991413 0.00000000 0.12666317 0.06922157
## SLG                0.59428908 0.7416220 0.12666317 0.00000000 0.03327996
## G                  0.03702099 0.1113959 0.06922157 0.03327996 0.00000000
## [1] "cor(RA, SLG)=0.7416"
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-15.png) 

```
## [1] "cor(WorldSeries, RA)=-0.1439"
## [1] "cor(WorldSeries, SLG)=-0.1264"
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
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping SLG as a feature
```

![](MoneyBall_Champ_files/figure-html/remove_correlated_features-16.png) 

```
##                            id       cor.y  cor.y.abs
## NumCompetitors NumCompetitors -0.22390392 0.22390392
## RA                         RA -0.14385140 0.14385140
## RankSeason         RankSeason -0.13046813 0.13046813
## G                           G  0.04099881 0.04099881
##                NumCompetitors        RA RankSeason          G
## NumCompetitors     1.00000000 0.5136769 0.42473927 0.03702099
## RA                 0.51367693 1.0000000 0.39914126 0.11139587
## RankSeason         0.42473927 0.3991413 1.00000000 0.06922157
## G                  0.03702099 0.1113959 0.06922157 1.00000000
##                NumCompetitors        RA RankSeason          G
## NumCompetitors     0.00000000 0.5136769 0.42473927 0.03702099
## RA                 0.51367693 0.0000000 0.39914126 0.11139587
## RankSeason         0.42473927 0.3991413 0.00000000 0.06922157
## G                  0.03702099 0.1113959 0.06922157 0.00000000
##                id       cor.y  cor.y.abs cor.low
## 11              W  0.12278172 0.12278172      NA
## 2               G  0.04099881 0.04099881       1
## 5            OOBP -0.01158587 0.01158587      NA
## 1              BA -0.01304152 0.01304152      NA
## 6            OSLG -0.02897498 0.02897498      NA
## 4             OBP -0.06707138 0.06707138      NA
## 9              RS -0.08209385 0.08209385      NA
## 10            SLG -0.12636418 0.12636418      NA
## 8      RankSeason -0.13046813 0.13046813       1
## 7              RA -0.14385140 0.14385140       1
## 12           Year -0.21348886 0.21348886      NA
## 3  NumCompetitors -0.22390392 0.22390392       1
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
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Add dummy model - random variable
#   Potential Enhancements:
#       For classifiers, it shd generate proba/outcomes that mimics the freq
#           distribution of glb_predct_var values; Right now it always generates
#           0 (most frequent ?)
ret_lst <- myrun_mdl_fn(indep_vars_vctr=".rnorm",
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
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
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6542  -0.6542  -0.6542  -0.6542   1.8150  
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.4331     0.1623  -8.828   <2e-16 ***
## .rnorm            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 239.12  on 243  degrees of freedom
## AIC: 241.12
## 
## Number of Fisher Scoring iterations: 4
```

```r
glb_dmy_mdl <- glb_mdl

# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.8217  -0.6558  -0.6558  -0.6067   1.9637  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -29.2414    43.4401  -0.673    0.501
## G             0.1717     0.2682   0.640    0.522
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 238.70  on 242  degrees of freedom
## AIC: 242.7
## 
## Number of Fisher Scoring iterations: 4
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, 
              subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                        glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df)    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9365  -0.5488  -0.4096  -0.3239   2.4071  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)
## (Intercept)  1.552e+02  1.260e+02   1.232    0.218
## G           -9.521e-01  1.472e+00  -0.647    0.518
## G:W         -5.136e-04  5.303e-04  -0.968    0.333
## G:OOBP      -1.231e-01  2.983e-01  -0.413    0.680
## G:BA         2.825e-01  3.285e-01   0.860    0.390
## G:OSLG      -9.766e-02  1.462e-01  -0.668    0.504
## G:OBP        1.553e-01  3.628e-01   0.428    0.669
## G:RS        -6.200e-05  9.332e-05  -0.664    0.506
## G:SLG        1.868e-01  2.083e-01   0.897    0.370
## G:Year      -2.573e-05  5.293e-04  -0.049    0.961
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 84.926  on 113  degrees of freedom
## Residual deviance: 79.325  on 104  degrees of freedom
##   (130 observations deleted due to missingness)
## AIC: 99.325
## 
## Number of Fisher Scoring iterations: 5
```

```r
# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, 
                                               cor.low == 1)[, "id"],
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1802  -0.7460  -0.5124  -0.4534   2.2230  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)  
## (Intercept)    -27.768291  39.464874  -0.704   0.4817  
## G                0.175939   0.244503   0.720   0.4718  
## RankSeason      -0.069417   0.120186  -0.578   0.5635  
## RA              -0.001111   0.002757  -0.403   0.6871  
## NumCompetitors  -0.209709   0.091754  -2.286   0.0223 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 225.88  on 239  degrees of freedom
## AIC: 235.88
## 
## Number of Fisher Scoring iterations: 4
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(glb_predct_var, glb_exclude_vars_as_features)),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9857  -0.5542  -0.4303  -0.3222   2.4316  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)
## (Intercept)    179.913829 215.548891   0.835    0.404
## Year            -0.013721   0.098720  -0.139    0.889
## RS              -0.007852   0.016692  -0.470    0.638
## RA              -0.009999   0.014962  -0.668    0.504
## W               -0.136012   0.129791  -1.048    0.295
## OBP             24.036427  58.714522   0.409    0.682
## SLG             35.959472  35.962492   1.000    0.317
## BA              47.656335  54.278591   0.878    0.380
## RankSeason      -0.050991   0.274336  -0.186    0.853
## G               -0.977973   0.782633  -1.250    0.211
## OOBP           -10.411064  54.009027  -0.193    0.847
## OSLG            -4.586722  29.367012  -0.156    0.876
## League.fctrAL    0.146678   0.781071   0.188    0.851
## NumCompetitors  -0.155595   0.663693  -0.234    0.815
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 84.926  on 113  degrees of freedom
## Residual deviance: 78.755  on 100  degrees of freedom
##   (130 observations deleted due to missingness)
## AIC: 106.75
## 
## Number of Fisher Scoring iterations: 5
```

```r
# User specified - easier to exclude features
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
#     union(union(glb_predct_var, glb_exclude_vars_as_features), c("<feat1_name>", "<feat2_name>"))),
#                         glb_predct_var, glb_predct_var_name,
#                         fit_df=glb_entity_df, OOB_df=glb_newent_df)

# User specified - easier to include features
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("<feat1_name>", "<feat2_name>"),
#                         glb_predct_var, glb_predct_var_name,
#                         fit_df=glb_entity_df, OOB_df=glb_newent_df)

for (feat in setdiff(names(glb_entity_df), 
                     union(glb_predct_var, glb_exclude_vars_as_features))) {
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=feat,
                            glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df)
    if (feat == "NumCompetitors")
        glb_sel_mdl <- glb_mdl
}    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0297  -0.6797  -0.5435  -0.4648   2.1504  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) 72.23602   22.64409    3.19  0.00142 **
## Year        -0.03700    0.01138   -3.25  0.00115 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 228.35  on 242  degrees of freedom
## AIC: 232.35
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.8254  -0.6819  -0.6363  -0.5561   2.0308  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept)  0.661226   1.636494   0.404    0.686
## RS          -0.002681   0.002098  -1.278    0.201
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 237.45  on 242  degrees of freedom
## AIC: 241.45
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9749  -0.6883  -0.6118  -0.4746   2.1577  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  1.888174   1.483831   1.272   0.2032  
## RA          -0.005053   0.002273  -2.223   0.0262 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 233.88  on 242  degrees of freedom
## AIC: 237.88
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0623  -0.6777  -0.6117  -0.5367   2.1254  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -6.85568    2.87620  -2.384   0.0171 *
## W            0.05671    0.02988   1.898   0.0577 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 235.51  on 242  degrees of freedom
## AIC: 239.51
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.8071  -0.6749  -0.6365  -0.5797   1.9753  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)    2.741      3.989   0.687    0.492
## OBP          -12.402     11.865  -1.045    0.296
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 238.02  on 242  degrees of freedom
## AIC: 242.02
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9498  -0.6953  -0.6088  -0.5197   2.1136  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)    3.200      2.358   1.357   0.1748  
## SLG          -11.130      5.689  -1.956   0.0504 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 235.23  on 242  degrees of freedom
## AIC: 239.23
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6797  -0.6592  -0.6513  -0.6389   1.8431  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -0.6392     3.8988  -0.164    0.870
## BA           -2.9765    14.6123  -0.204    0.839
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 239.08  on 242  degrees of freedom
## AIC: 243.08
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.7805  -0.7131  -0.5918  -0.4882   2.1781  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)  -0.8256     0.3268  -2.527   0.0115 *
## RankSeason   -0.2069     0.1027  -2.016   0.0438 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 234.75  on 242  degrees of freedom
## AIC: 238.75
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.8217  -0.6558  -0.6558  -0.6067   1.9637  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -29.2414    43.4401  -0.673    0.501
## G             0.1717     0.2682   0.640    0.522
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 238.70  on 242  degrees of freedom
## AIC: 242.7
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5318  -0.5176  -0.5106  -0.5023   2.0697  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -0.9306     8.3728  -0.111    0.912
## OOBP         -3.2233    26.0587  -0.124    0.902
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 84.926  on 113  degrees of freedom
## Residual deviance: 84.910  on 112  degrees of freedom
##   (130 observations deleted due to missingness)
## AIC: 88.91
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5610  -0.5209  -0.5088  -0.4902   2.1268  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -0.08725    6.07285  -0.014    0.989
## OSLG        -4.65992   15.06881  -0.309    0.757
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 84.926  on 113  degrees of freedom
## Residual deviance: 84.830  on 112  degrees of freedom
##   (130 observations deleted due to missingness)
## AIC: 88.83
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6772  -0.6772  -0.6306  -0.6306   1.8509  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    -1.5141     0.2355  -6.430 1.28e-10 ***
## League.fctrAL   0.1583     0.3252   0.487    0.626    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 238.88  on 242  degrees of freedom
## AIC: 242.88
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9871  -0.8017  -0.5089  -0.5089   2.2643  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     0.03868    0.43750   0.088 0.929559    
## NumCompetitors -0.25220    0.07422  -3.398 0.000678 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 226.96  on 242  degrees of freedom
## AIC: 230.96
## 
## Number of Fisher Scoring iterations: 4
```

```r
# Features that were significant in bivariate models
ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("Year", "RA", "RankSeason", "NumCompetitors"),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0336  -0.7689  -0.5139  -0.4583   2.2195  
## 
## Coefficients:
##                  Estimate Std. Error z value Pr(>|z|)
## (Intercept)    12.5874376 53.6474210   0.235    0.814
## Year           -0.0061425  0.0274665  -0.224    0.823
## RA             -0.0008238  0.0027391  -0.301    0.764
## RankSeason     -0.0685046  0.1203459  -0.569    0.569
## NumCompetitors -0.1794264  0.1815933  -0.988    0.323
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 226.37  on 239  degrees of freedom
## AIC: 236.37
## 
## Number of Fisher Scoring iterations: 4
```

```r
combn_mtrx <- combn(c("Year", "RA", "RankSeason", "NumCompetitors"), 2)
for (combn_ix in 1:ncol(combn_mtrx))
    #print(indep_vars_vctr <- combn_mtrx[, combn_ix])
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=combn_mtrx[, combn_ix],
                            glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0402  -0.6878  -0.5298  -0.4785   2.1370  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) 63.610741  25.654830   2.479   0.0132 *
## Year        -0.032084   0.013323  -2.408   0.0160 *
## RA          -0.001766   0.002585  -0.683   0.4945  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 227.88  on 241  degrees of freedom
## AIC: 233.88
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0560  -0.6957  -0.5379  -0.4528   2.2673  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) 63.64855   24.37063   2.612  0.00901 **
## Year        -0.03254    0.01231  -2.643  0.00822 **
## RankSeason  -0.10064    0.11352  -0.887  0.37534   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 227.55  on 241  degrees of freedom
## AIC: 233.55
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0050  -0.7823  -0.5115  -0.4970   2.2552  
## 
## Coefficients:
##                 Estimate Std. Error z value Pr(>|z|)
## (Intercept)    13.350467  53.481896   0.250    0.803
## Year           -0.006802   0.027328  -0.249    0.803
## NumCompetitors -0.212610   0.175520  -1.211    0.226
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 226.90  on 241  degrees of freedom
## AIC: 232.9
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9374  -0.6933  -0.5936  -0.4564   2.1979  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept)  1.487461   1.506143   0.988    0.323
## RA          -0.003815   0.002441  -1.563    0.118
## RankSeason  -0.140824   0.110908  -1.270    0.204
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 232.22  on 241  degrees of freedom
## AIC: 238.22
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0433  -0.7826  -0.5133  -0.4701   2.2208  
## 
## Coefficients:
##                 Estimate Std. Error z value Pr(>|z|)   
## (Intercept)     0.716895   1.528736   0.469  0.63911   
## RA             -0.001233   0.002661  -0.463  0.64313   
## NumCompetitors -0.229385   0.088399  -2.595  0.00946 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 226.74  on 241  degrees of freedom
## AIC: 232.74
## 
## Number of Fisher Scoring iterations: 4
## 
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0090  -0.7592  -0.5204  -0.4501   2.2562  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)   
## (Intercept)     0.12277    0.45737   0.268  0.78837   
## RankSeason     -0.07697    0.11711  -0.657  0.51102   
## NumCompetitors -0.22784    0.08201  -2.778  0.00546 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 226.52  on 241  degrees of freedom
## AIC: 232.52
## 
## Number of Fisher Scoring iterations: 4
```

```r
# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
if (glb_is_regression) {
    print(orderBy(~ -R.sq.OOB -Adj.R.sq.fit, glb_models_df))
    print(myplot_scatter(plot_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}    

if (glb_is_classification) {
    # Lower AIC is better
    print(orderBy(~ -auc.OOB +AIC.fit, glb_models_df))    
    plot_models_df[, "inv.AIC.fit"] <- 1.0 / plot_models_df[, "AIC.fit"] 
    print(myplot_scatter(plot_models_df, "inv.AIC.fit", "auc.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}
```

```
##                                                                                    feats
## 5  Year, RS, RA, W, OBP, SLG, BA, RankSeason, G, OOBP, OSLG, League.fctr, NumCompetitors
## 3                               G, G:W, G:OOBP, G:BA, G:OSLG, G:OBP, G:RS, G:SLG, G:Year
## 4                                                      G, RankSeason, RA, NumCompetitors
## 19                                                  Year, RA, RankSeason, NumCompetitors
## 25                                                            RankSeason, NumCompetitors
## 24                                                                    RA, NumCompetitors
## 21                                                                      Year, RankSeason
## 20                                                                              Year, RA
## 18                                                                        NumCompetitors
## 6                                                                                   Year
## 22                                                                  Year, NumCompetitors
## 23                                                                        RA, RankSeason
## 8                                                                                     RA
## 13                                                                            RankSeason
## 9                                                                                      W
## 11                                                                                   SLG
## 7                                                                                     RS
## 10                                                                                   OBP
## 16                                                                                  OSLG
## 17                                                                           League.fctr
## 2                                                                                      G
## 14                                                                                     G
## 15                                                                                  OOBP
## 1                                                                                 .rnorm
## 12                                                                                    BA
##    n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB   AIC.fit
## 5    244       NA       NA           NA 1257.465      NA 106.75451
## 3    244       NA       NA           NA 1200.730      NA  99.32490
## 4    244       NA       NA           NA 1975.622      NA 235.88179
## 19   244       NA       NA           NA 1921.940      NA 236.37068
## 25   244       NA       NA           NA 1936.682      NA 232.52395
## 24   244       NA       NA           NA 1898.333      NA 232.74295
## 21   244       NA       NA           NA 1907.232      NA 233.55245
## 20   244       NA       NA           NA 1821.496      NA 233.87659
## 18   244       NA       NA           NA 1910.431      NA 230.95923
## 6    244       NA       NA           NA 1823.756      NA 232.35084
## 22   244       NA       NA           NA 1906.284      NA 232.89717
## 23   244       NA       NA           NA 1764.661      NA 238.21799
## 8    244       NA       NA           NA 1722.433      NA 237.88393
## 13   244       NA       NA           NA 1660.391      NA 238.75458
## 9    244       NA       NA           NA 1678.456      NA 239.51047
## 11   244       NA       NA           NA 1681.857      NA 239.22826
## 7    244       NA       NA           NA 1618.296      NA 241.45431
## 10   244       NA       NA           NA 1612.413      NA 242.02089
## 16   244       NA       NA           NA 1060.740      NA  88.82951
## 17   244       NA       NA           NA 1574.189      NA 242.88464
## 2    244       NA       NA           NA 1574.242      NA 242.69883
## 14   244       NA       NA           NA 1574.242      NA 242.69883
## 15   244       NA       NA           NA 1058.513      NA  88.91025
## 1    244       NA       NA           NA 1568.937      NA 241.12195
## 12   244       NA       NA           NA 1570.239      NA 243.08044
##      auc.fit   auc.OOB
## 5  0.6957143 0.6957143
## 3  0.6885714 0.6885714
## 4  0.6693487 0.6693487
## 19 0.6597905 0.6597905
## 25 0.6575764 0.6575764
## 24 0.6572524 0.6572524
## 21 0.6530403 0.6530403
## 20 0.6514202 0.6514202
## 18 0.6453721 0.6453721
## 6  0.6453721 0.6453721
## 22 0.6453721 0.6453721
## 23 0.6225294 0.6225294
## 8  0.6109731 0.6109731
## 13 0.5899665 0.5899665
## 9  0.5812723 0.5812723
## 11 0.5767902 0.5767902
## 7  0.5545955 0.5545955
## 10 0.5494114 0.5494114
## 16 0.5425000 0.5425000
## 17 0.5197646 0.5197646
## 2  0.5166325 0.5166325
## 14 0.5166325 0.5166325
## 15 0.5128571 0.5128571
## 1  0.5000000 0.5000000
## 12 0.4945998 0.4945998
```

![](MoneyBall_Champ_files/figure-html/run_models-1.png) 

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
print(mdl_feats_df <- myextract_mdl_feats(lcl_sel_mdl=glb_sel_mdl, 
                                          lcl_entity_df=glb_entity_df))
```

```
##                  Estimate Std. Error  z value         Pr.z             id
## NumCompetitors -0.2521967 0.07421589 -3.39815 0.0006784323 NumCompetitors
##                fit.feat
## NumCompetitors     TRUE
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl        
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_entity_df, type="response")

    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_entity_df[, glb_predct_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    thresholds_df <- data.frame(threshold=seq(0.0, 1.0, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     thresholds_df[row_ix, "threshold"], 
                                     glb_predct_var, glb_predct_var_name))
    print(thresholds_df)
    print(myplot_line(thresholds_df, "threshold", "f.score"))
    
    glb_clf_proba_threshold <- thresholds_df[which.max(thresholds_df$f.score), 
                                             "threshold"]
    # This should change to maximize f.score.OOB ???
    print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.fit",
                  glb_clf_proba_threshold))

    glb_entity_df[, glb_predct_var_name] <- 
        (glb_entity_df[, paste0(glb_predct_var_name, ".proba")] >= 
             glb_clf_proba_threshold) * 1.0
    print(mycreate_xtab(glb_entity_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     glb_clf_proba_threshold, 
                                     glb_predct_var, glb_predct_var_name)))    
}    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9871  -0.8017  -0.5089  -0.5089   2.2643  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)     0.03868    0.43750   0.088 0.929559    
## NumCompetitors -0.25220    0.07422  -3.398 0.000678 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 239.12  on 243  degrees of freedom
## Residual deviance: 226.96  on 242  degrees of freedom
## AIC: 230.96
## 
## Number of Fisher Scoring iterations: 4
```

![](MoneyBall_Champ_files/figure-html/fit_training.all-1.png) 

```
##    threshold   f.score
## 1        0.0 0.3230241
## 2        0.1 0.3274021
## 3        0.2 0.3921569
## 4        0.3 0.2295082
## 5        0.4 0.0000000
## 6        0.5 0.0000000
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](MoneyBall_Champ_files/figure-html/fit_training.all-2.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   WorldSeries WorldSeries.predict.0 WorldSeries.predict.1
## 1           0                   121                    76
## 2           1                    17                    30
## [1] "f.score=0.3922"
```

```r
print(glb_feats_df <- mymerge_feats_Pr.z(glb_feats_df, glb_sel_mdl, glb_entity_df))
```

```
##                id       cor.y  cor.y.abs cor.low         Pr.z
## 3  NumCompetitors -0.22390392 0.22390392       1 0.0006784323
## 1              BA -0.01304152 0.01304152      NA           NA
## 2               G  0.04099881 0.04099881       1           NA
## 4             OBP -0.06707138 0.06707138      NA           NA
## 5            OOBP -0.01158587 0.01158587      NA           NA
## 6            OSLG -0.02897498 0.02897498      NA           NA
## 7              RA -0.14385140 0.14385140       1           NA
## 8      RankSeason -0.13046813 0.13046813       1           NA
## 9              RS -0.08209385 0.08209385      NA           NA
## 10            SLG -0.12636418 0.12636418      NA           NA
## 11              W  0.12278172 0.12278172      NA           NA
## 12           Year -0.21348886 0.21348886      NA           NA
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name)
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        if (nrow(plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)) == 0)
            warning("No coefficients in selected model are statistically significant")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], 
                              ".rownames"),
                                               feat_y=plot_vars_df$id[1],
                    lcl_predct_var=glb_predct_var, 
                    lcl_predct_var_name=glb_predct_var_name, 
                    lcl_id_vars=glb_id_vars)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_entity_df)
```

![](MoneyBall_Champ_files/figure-html/fit_training.all-3.png) 

```
##      Team League Year  RS  RA   W   OBP   SLG    BA Playoffs RankSeason
## 2     ATL     NL 2012 700 600  94 0.320 0.389 0.247        1          4
## 25    SFG     NL 2012 718 649  94 0.327 0.397 0.269        1          4
## 998   BAL     AL 1973 754 561  97 0.345 0.389 0.266        1          2
## 1101  DET     AL 1968 671 492 103 0.307 0.385 0.235        1          1
## 1111  STL     NL 1968 583 472  97 0.298 0.346 0.249        1          2
## 1115  BOS     AL 1967 722 614  92 0.321 0.395 0.255        1          2
## 1144  LAD     NL 1966 606 490  95 0.314 0.362 0.256        1          2
## 1164  MIN     AL 1965 774 600 102 0.324 0.399 0.254        1          1
## 1187  NYY     AL 1964 730 577  99 0.317 0.387 0.253        1          1
## 1207  NYY     AL 1963 714 547 104 0.309 0.403 0.252        1          1
## 1230  SFG     NL 1962 878 690 103 0.341 0.441 0.278        1          1
##      RankPlayoffs   G  OOBP  OSLG Team.fctr League.fctr NumCompetitors
## 2               5 162 0.306 0.378       ATL          NL             10
## 25              1 162 0.313 0.393       SFG          NL             10
## 998             3 162    NA    NA       BAL          AL              4
## 1101            1 164    NA    NA       DET          AL              2
## 1111            2 162    NA    NA       STL          NL              2
## 1115            2 162    NA    NA       BOS          AL              2
## 1144            2 162    NA    NA       LAD          NL              2
## 1164            2 162    NA    NA       MIN          AL              2
## 1187            2 164    NA    NA       NYY          AL              2
## 1207            2 161    NA    NA       NYY          AL              2
## 1230            2 165    NA    NA       SFG          NL              2
##      WorldSeries    .rnorm WorldSeries.predict.proba WorldSeries.predict
## 2              0 0.6300986                0.07703779                   0
## 25             1 0.6300986                0.07703779                   0
## 998            0 0.6300986                0.27485822                   1
## 1101           1 0.6300986                0.38563015                   1
## 1111           0 0.6300986                0.38563015                   1
## 1115           0 0.6300986                0.38563015                   1
## 1144           0 0.6300986                0.38563015                   1
## 1164           0 0.6300986                0.38563015                   1
## 1187           0 0.6300986                0.38563015                   1
## 1207           0 0.6300986                0.38563015                   1
## 1230           0 0.6300986                0.38563015                   1
##      .rownames WorldSeries.fctr WorldSeries.predict.accurate   .label
## 2            2                0                         TRUE 2012:ATL
## 25          25                1                        FALSE 2012:SFG
## 998        998                0                        FALSE 1973:BAL
## 1101      1101                1                         TRUE 1968:DET
## 1111      1111                0                        FALSE 1968:STL
## 1115      1115                0                        FALSE 1967:BOS
## 1144      1144                0                        FALSE 1966:LAD
## 1164      1164                0                        FALSE 1965:MIN
## 1187      1187                0                        FALSE 1964:NYY
## 1207      1207                0                        FALSE 1963:NYY
## 1230      1230                0                        FALSE 1962:SFG
```

![](MoneyBall_Champ_files/figure-html/fit_training.all-4.png) 

```r
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
    glb_newent_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_newent_df, type="response")

if (glb_is_classification) {
    # Compute selected model predictions
    glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, glb_predct_var_name] <- 
        (predict(glb_sel_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0

    # Compute dummy model predictions
    glb_newent_df[, paste0(glb_predct_var, ".preddmy.proba")] <- 
        predict(glb_dmy_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, paste0(glb_predct_var, ".preddmy")] <- 
        (predict(glb_dmy_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0
}
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```r
myprint_df(glb_newent_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##    Year Team WorldSeries WorldSeries.predict
## 2  2012  ATL           0                   0
## 3  2012  BAL           0                   0
## 7  2012  CIN           0                   0
## 10 2012  DET           0                   0
## 19 2012  NYY           0                   0
## 20 2012  OAK           0                   0
##     Year Team WorldSeries WorldSeries.predict
## 62  2010  ATL           0                   0
## 350 2001  NYY           0                   0
## 606 1990  PIT           0                   1
## 687 1987  SFG           0                   1
## 770 1983  BAL           1                   1
## 966 1975  OAK           0                   1
##      Year Team WorldSeries WorldSeries.predict
## 1187 1964  NYY           0                   1
## 1191 1964  STL           1                   1
## 1203 1963  LAD           1                   1
## 1207 1963  NYY           0                   1
## 1227 1962  NYY           1                   1
## 1230 1962  SFG           0                   1
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2) / nrow(glb_newent_df)) ^ 0.5))                        
    print(myplot_scatter(glb_newent_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
                         
    glb_newent_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_newent_df[, glb_predct_var_name] - glb_newent_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_newent_df)))                                                      

#     glb_newent_df[, "<Output Pred variable>"] <- func(glb_newent_df[, glb_pred_var_name])                         
}                         

if (glb_is_classification) {
    ROCRpred <- prediction(glb_newent_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_newent_df[, glb_predct_var])
    print(sprintf("auc=%0.4f", auc <- as.numeric(performance(ROCRpred, "auc")@y.values)))   
    
    print(sprintf("probability threshold=%0.4f", glb_clf_proba_threshold))
    print(newent_conf_df <- mycreate_xtab(glb_newent_df, 
                                        c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score.sel=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=glb_predct_var_name)))
    print(sprintf("sensitivity=%0.4f", newent_conf_df[2, 3] / 
                      (newent_conf_df[2, 3] + newent_conf_df[2, 2])))
    print(sprintf("specificity=%0.4f", newent_conf_df[1, 2] / 
                      (newent_conf_df[1, 2] + newent_conf_df[1, 3])))
    print(sprintf("accuracy=%0.4f", (newent_conf_df[1, 2] + newent_conf_df[2, 3]) / 
                      (newent_conf_df[1, 2] + newent_conf_df[2, 3] + 
                       newent_conf_df[1, 3] + newent_conf_df[2, 2])))
    
    print(mycreate_xtab(glb_newent_df, c(glb_predct_var, paste0(glb_predct_var, ".preddmy"))))
    print(sprintf("f.score.dmy=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_dmy_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=paste0(glb_predct_var, ".preddmy"))))
}    
```

```
## [1] "auc=0.6454"
## [1] "probability threshold=0.2000"
##   WorldSeries WorldSeries.predict.0 WorldSeries.predict.1
## 1           0                   121                    76
## 2           1                    17                    30
## [1] "f.score.sel=0.3922"
## [1] "sensitivity=0.6383"
## [1] "specificity=0.6142"
## [1] "accuracy=0.6189"
##   WorldSeries WorldSeries.preddmy.0
## 1           0                   197
## 2           1                    47
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## [1] "f.score.dmy=0.0000"
```

```r
glb_analytics_diag_plots(glb_newent_df)
```

![](MoneyBall_Champ_files/figure-html/predict_newdata-1.png) 

```
##      Team League Year  RS  RA   W   OBP   SLG    BA Playoffs RankSeason
## 2     ATL     NL 2012 700 600  94 0.320 0.389 0.247        1          4
## 25    SFG     NL 2012 718 649  94 0.327 0.397 0.269        1          4
## 998   BAL     AL 1973 754 561  97 0.345 0.389 0.266        1          2
## 1101  DET     AL 1968 671 492 103 0.307 0.385 0.235        1          1
## 1111  STL     NL 1968 583 472  97 0.298 0.346 0.249        1          2
## 1115  BOS     AL 1967 722 614  92 0.321 0.395 0.255        1          2
## 1144  LAD     NL 1966 606 490  95 0.314 0.362 0.256        1          2
## 1164  MIN     AL 1965 774 600 102 0.324 0.399 0.254        1          1
## 1187  NYY     AL 1964 730 577  99 0.317 0.387 0.253        1          1
## 1207  NYY     AL 1963 714 547 104 0.309 0.403 0.252        1          1
## 1230  SFG     NL 1962 878 690 103 0.341 0.441 0.278        1          1
##      RankPlayoffs   G  OOBP  OSLG Team.fctr League.fctr NumCompetitors
## 2               5 162 0.306 0.378       ATL          NL             10
## 25              1 162 0.313 0.393       SFG          NL             10
## 998             3 162    NA    NA       BAL          AL              4
## 1101            1 164    NA    NA       DET          AL              2
## 1111            2 162    NA    NA       STL          NL              2
## 1115            2 162    NA    NA       BOS          AL              2
## 1144            2 162    NA    NA       LAD          NL              2
## 1164            2 162    NA    NA       MIN          AL              2
## 1187            2 164    NA    NA       NYY          AL              2
## 1207            2 161    NA    NA       NYY          AL              2
## 1230            2 165    NA    NA       SFG          NL              2
##      WorldSeries     .rnorm WorldSeries.predict.proba WorldSeries.predict
## 2              0 -0.2761841                0.07703779                   0
## 25             1 -0.2761841                0.07703779                   0
## 998            0 -0.2761841                0.27485822                   1
## 1101           1 -0.2761841                0.38563015                   1
## 1111           0 -0.2761841                0.38563015                   1
## 1115           0 -0.2761841                0.38563015                   1
## 1144           0 -0.2761841                0.38563015                   1
## 1164           0 -0.2761841                0.38563015                   1
## 1187           0 -0.2761841                0.38563015                   1
## 1207           0 -0.2761841                0.38563015                   1
## 1230           0 -0.2761841                0.38563015                   1
##      WorldSeries.preddmy.proba WorldSeries.preddmy .rownames
## 2                     0.192623                   0         2
## 25                    0.192623                   0        25
## 998                   0.192623                   0       998
## 1101                  0.192623                   0      1101
## 1111                  0.192623                   0      1111
## 1115                  0.192623                   0      1115
## 1144                  0.192623                   0      1144
## 1164                  0.192623                   0      1164
## 1187                  0.192623                   0      1187
## 1207                  0.192623                   0      1207
## 1230                  0.192623                   0      1230
##      WorldSeries.fctr WorldSeries.predict.accurate   .label
## 2                   0                         TRUE 2012:ATL
## 25                  1                        FALSE 2012:SFG
## 998                 0                        FALSE 1973:BAL
## 1101                1                         TRUE 1968:DET
## 1111                0                        FALSE 1968:STL
## 1115                0                        FALSE 1967:BOS
## 1144                0                        FALSE 1966:LAD
## 1164                0                        FALSE 1965:MIN
## 1187                0                        FALSE 1964:NYY
## 1207                0                        FALSE 1963:NYY
## 1230                0                        FALSE 1962:SFG
```

![](MoneyBall_Champ_files/figure-html/predict_newdata-2.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.2 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ROCR_1.0-6      gplots_2.16.0   reshape2_1.4.1  plyr_1.8.1     
## [5] doBy_4.5-13     survival_2.38-1 ggplot2_1.0.1  
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6       caTools_1.17.1     colorspace_1.2-6  
##  [4] digest_0.6.8       evaluate_0.5.5     formatR_1.0       
##  [7] gdata_2.13.3       grid_3.1.3         gtable_0.1.2      
## [10] gtools_3.4.1       htmltools_0.2.6    KernSmooth_2.23-14
## [13] knitr_1.9          labeling_0.3       lattice_0.20-30   
## [16] MASS_7.3-39        Matrix_1.1-5       munsell_0.4.2     
## [19] proto_0.3-10       Rcpp_0.11.5        rmarkdown_0.5.1   
## [22] scales_0.2.4       splines_3.1.3      stringr_0.6.2     
## [25] tools_3.1.3        yaml_2.1.13
```
