
R version 4.2.2 (2022-10-31) -- "Innocent and Trusting"
Copyright (C) 2022 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> # Script Settings and Resources
> file.path(getwd(), "week11-cluster.R")
[1] "/panfs/jay/groups/23/lande065/huan2384/psy8960-week11/week11-cluster/week11-cluster.R"
> library(tidyverse)
── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.1     ✔ readr     2.1.4
✔ forcats   1.0.0     ✔ stringr   1.5.0
✔ ggplot2   3.4.2     ✔ tibble    3.2.1
✔ lubridate 1.9.2     ✔ tidyr     1.3.0
✔ purrr     1.0.1     
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
> library(haven)
> library(caret)
Loading required package: lattice

Attaching package: ‘caret’

The following object is masked from ‘package:purrr’:

    lift

> library(tictoc)
> library(parallel)
> library(doParallel)
Loading required package: foreach

Attaching package: ‘foreach’

The following objects are masked from ‘package:purrr’:

    accumulate, when

Loading required package: iterators
> 
> # turn on parallel processing
> local_cluster <- makeCluster(14) #increase cores number
> registerDoParallel(local_cluster)
> 
> # Data Import and Cleaning
> gss_import_tbl <- read_sav("../data/GSS2016.sav") %>%
+   filter(!is.na(MOSTHRS))
> gss_tbl <- 
+   gss_import_tbl[, colSums(is.na(gss_import_tbl)) < .75 * nrow(gss_import_tbl)] %>%
+   rename(workhours = MOSTHRS) %>%
+   mutate(workhours = as.integer(workhours))
> # Remove HRS1 and HRS2 columns from the dataset
> gss_tbl <- gss_tbl[ , !(names(gss_tbl) %in% c("HRS1", "HRS2"))]
> 
> # Analysis
> train_cases <- sample(1:nrow(gss_tbl), .75*nrow(gss_tbl))
> 
> gss_train_tbl <- gss_tbl[train_cases, ]
> gss_test_tbl <- gss_tbl[-train_cases, ]
> 
> training_folds <- createFolds(gss_train_tbl$workhours,
+                               k=10)
> 
> # Function to time model fitting
> time_model <- function(model) {
+   tic()
+   fit <- model
+   toc()
+   return(fit)
+ }
> 
> # Fit models and time execution
> # fit model 1 without Parallelization
> model1<- time_model(train( 
+   workhours ~ .,
+   gss_train_tbl, 
+   method="lm",
+   na.action=na.pass,
+   preProcess=c("center", "scale", "nzv", "medianImpute"),
+   trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
+ ))
Aggregating results
Fitting final model on full training set
33.491 sec elapsed
> 
> model1_time <- model1$time
> model1_t_elapsed<-round(model1_time$everything["elapsed"],2) # extract execution time
> 
> # fit model 1 with Parallelization
> model1_par<- time_model(train(
+   workhours ~ .,
+   gss_train_tbl, 
+   method="lm",
+   na.action=na.pass,
+   preProcess=c("center", "scale", "nzv", "medianImpute"),
+   trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T),
+   allowParallel=TRUE
+ ))
Aggregating results
Fitting final model on full training set
4.815 sec elapsed
Warning message:
In lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) :
 extra argument ‘allowParallel’ will be disregarded 
> model1_par_time <- model1_par$time
> model1_p_elapsed<-round(model1_par_time$everything["elapsed"],2) # extract time execution
> 
> # Holdout correlation reveals the real effectiveness of our model
> hocv_cor_1 <- cor(
+   predict(model1, gss_test_tbl, na.action=na.pass),
+   gss_test_tbl$workhours
+ ) ^ 2
Warning message:
In predict.lm(modelFit, newdata) :
  prediction from a rank-deficient fit may be misleading
> 
> # fit model 2 without Parallelization
> model2<- time_model(train(
+   workhours ~ .,
+   gss_train_tbl, 
+   method="glmnet",
+   tuneLength=3,
+   na.action=na.pass,
+   preProcess=c("center", "scale", "nzv", "medianImpute"),
+   trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
+ ))
Aggregating results
Selecting tuning parameters
Fitting alpha = 0.1, lambda = 0.847 on full training set
14.904 sec elapsed
> model2_time <- model2$time
> model2_t_elapsed<-round(model2_time$everything["elapsed"],2) # extract execution time
> 
> # fit model 2 with Parallelization
> model2_par<- time_model(train(
+   workhours ~ .,
+   gss_train_tbl, 
+   method="glmnet",
+   tuneLength=3,
+   na.action=na.pass,
+   preProcess=c("center", "scale", "nzv", "medianImpute"),
+   trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T),
+   allowParallel=TRUE
+ ))
Aggregating results
Selecting tuning parameters
Fitting alpha = 0.1, lambda = 0.847 on full training set
11.904 sec elapsed
> model2_par_time <- model2_par$time
> model2_p_elapsed<-round(model2_par_time$everything["elapsed"],2) # extract execution time
> 
> # Holdout correlation reveals the real effectiveness of our model
> hocv_cor_2 <- cor(
+   predict(model2, gss_test_tbl, na.action=na.pass),
+   gss_test_tbl$workhours
+ ) ^ 2
> 
> # fit model 3 without Parallelization
> model3<- time_model(train(
+   workhours ~ .,
+   gss_train_tbl, 
+   method="ranger",
+   tuneLength=3,
+   na.action=na.pass,
+   preProcess=c("center", "scale", "nzv", "medianImpute"),
+   trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
+ ))
Aggregating results
Selecting tuning parameters
Fitting mtry = 503, splitrule = variance, min.node.size = 5 on full training set
299.986 sec elapsed
Warning message:
In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
  There were missing values in resampled performance measures.
> model3_time <- model3$time
> model3_t_elapsed<-round(model3_time$everything["elapsed"],2) # extract execution time
> 
> # fit model 3 with Parallelization
> model3_par<- time_model(train(
+   workhours ~ .,
+   gss_train_tbl, 
+   method="ranger",
+   tuneLength=3,
+   na.action=na.pass,
+   preProcess=c("center", "scale", "nzv", "medianImpute"),
+   trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T),
+   allowParallel=TRUE
+ ))
Aggregating results
Selecting tuning parameters
Fitting mtry = 503, splitrule = extratrees, min.node.size = 5 on full training set
280.042 sec elapsed
Warning messages:
1: In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
  There were missing values in resampled performance measures.
2: In ranger::ranger(dependent.variable.name = ".outcome", data = x,  :
  Unused arguments: allowParallel
> model3_par_time <- model3_par$time
> model3_p_elapsed<-round(model3_par_time$everything["elapsed"],2) # extract execution time
> 
> # Holdout correlation reveals the real effectiveness of our model
> hocv_cor_3 <- cor(
+   predict(model3, gss_test_tbl, na.action=na.pass),
+   gss_test_tbl$workhours
+ ) ^ 2
> 
> # fit model 4 without Parallelization
> model4<- time_model(train(
+   workhours ~ .,
+   gss_train_tbl, 
+   method="xgbTree",
+   tuneLength=3,
+   na.action=na.pass,
+   preProcess=c("center", "scale", "nzv", "medianImpute"),
+   trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
+ ))
Aggregating results
Selecting tuning parameters
Fitting nrounds = 150, max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree = 0.6, min_child_weight = 1, subsample = 1 on full training set
239.945 sec elapsed
> model4_time <- model4$time
> model4_t_elapsed<-round(model4_time$everything["elapsed"],2) # extract execution time
> 
> # fit model 4 with Parallelization
> model4_par<- time_model(train(
+   workhours ~ .,
+   gss_train_tbl, 
+   method="xgbTree",
+   tuneLength=3,
+   na.action=na.pass,
+   preProcess=c("center", "scale", "nzv", "medianImpute"),
+   trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T),
+   allowParallel=TRUE
+ ))
Aggregating results
Selecting tuning parameters
Fitting nrounds = 150, max_depth = 3, eta = 0.4, gamma = 0, colsample_bytree = 0.6, min_child_weight = 1, subsample = 0.75 on full training set
[12:55:19] WARNING: src/learner.cc:767: 
Parameters: { "allowParallel" } are not used.

236.933 sec elapsed
> model4_par_time <- model4_par$time
> model4_p_elapsed<-round(model4_par_time$everything["elapsed"],2) # extract execution time 
> 
> # Holdout correlation reveals the real effectiveness of our model
> hocv_cor_4 <- cor(
+   predict(model4, gss_test_tbl, na.action=na.pass),
+   gss_test_tbl$workhours
+ ) ^ 2
> 
> # Visualize k-fold cvs
> summary(resamples(list(model1, model2, model3, model4)))

Call:
summary.resamples(object = resamples(list(model1, model2, model3, model4)))

Models: Model1, Model2, Model3, Model4 
Number of resamples: 10 

MAE 
            Min.    1st Qu.     Median       Mean    3rd Qu.       Max. NA's
Model1 4.4935524 15.0464694 28.8495384 36.8204053 35.4318329 139.932341    0
Model2 3.5492218  3.9367772  4.2905450  4.3243562  4.6207046   5.563024    0
Model3 2.6427040  2.8419287  3.2329000  3.2758127  3.4461215   4.478985    3
Model4 0.4436276  0.4749785  0.5899338  0.7470405  0.8622836   1.514780    0

RMSE 
            Min.   1st Qu.     Median       Mean    3rd Qu.       Max. NA's
Model1 29.121548 51.428909 124.020958 142.811782 182.650777 443.375504    0
Model2  4.561984  5.390917   6.019881   6.199946   7.104158   7.829228    0
Model3  3.277472  4.118917   4.770415   4.674499   4.873580   6.688614    3
Model4  1.094781  1.210916   1.490212   2.252081   3.122935   4.895059    0

Rsquared 
              Min.    1st Qu.     Median       Mean   3rd Qu.      Max. NA's
Model1 0.006467252 0.01641012 0.02964188 0.08867969 0.1578799 0.2901130    0
Model2 0.808396458 0.84656054 0.89058078 0.88069881 0.9126468 0.9374455    0
Model3 0.880065510 0.92094612 0.93468436 0.93738317 0.9633633 0.9783135    3
Model4 0.922169412 0.96446390 0.99322700 0.97676661 0.9950573 0.9964955    0

> resample_sum <- summary(resamples(list(model1, model2, model3, model4)))
> dotplot(resamples(list(model1, model2, model3, model4)))
> 
> # turn off parallel processing
> stopCluster(local_cluster)
> registerDoSEQ()
> 
> # Publication
> table3<- tibble(
+   algo = c("lm","glmnet","ranger","xgbTree"),
+   cv_rsq = str_remove(round(
+     resample_sum$statistics$Rsquared[,"Mean"],2
+   ),"^0"),
+   ho_rsq = str_remove(c(
+     format(round(hocv_cor_1,2),nsmall=2),
+     format(round(hocv_cor_2,2),nsmall=2),
+     format(round(hocv_cor_3,2),nsmall=2),
+     format(round(hocv_cor_3,2),nsmall=2)
+   ),"^0")
+ )
> table3
# A tibble: 4 × 3
  algo    cv_rsq ho_rsq
  <chr>   <chr>  <chr> 
1 lm      .09    .00   
2 glmnet  .88    .45   
3 ranger  .94    .48   
4 xgbTree .98    .48   
> 
> #add a new tibble to display execution time for each model
> table4<- tibble( 
+   algo = c("lm", "glmnet", "ranger", "xgbTree"),
+   original = c(model1_t_elapsed, model2_t_elapsed, model3_t_elapsed, model4_t_elapsed),
+   parallelized = c(model1_p_elapsed, model2_p_elapsed, model3_p_elapsed, model4_p_elapsed)
+ )%>%
+   rename(supercomputer = original, supercomputer_14 = parallelized) #rename column name
> table4
# A tibble: 4 × 3
  algo    supercomputer supercomputer_14
  <chr>           <dbl>            <dbl>
1 lm               33.4             4.69
2 glmnet           14.8            11.8 
3 ranger          300.            280.  
4 xgbTree         240.            237.  
> 
> #save tables to the out file
> write_csv(table3, "../out/table3.csv") 
> write_csv(table4, "../out/table4.csv")
> 
> 
> 
> 
> proc.time()
    user   system  elapsed 
  97.143    1.113 1133.635 
