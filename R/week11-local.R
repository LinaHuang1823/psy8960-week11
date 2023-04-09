# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(parallel)
library(doParallel)

# turn on parallel processing
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)

# Data Import and Cleaning
gss_import_tbl <- read_sav("../data/GSS2016.sav") %>%
  filter(!is.na(MOSTHRS))
gss_tbl <- 
  gss_import_tbl[, colSums(is.na(gss_import_tbl)) < .75 * nrow(gss_import_tbl)] %>%
  rename(workhours = MOSTHRS) %>%
  mutate(workhours = as.integer(workhours))
# Remove HRS1 and HRS2 columns from the dataset
gss_tbl <- gss_tbl[ , !(names(gss_tbl) %in% c("HRS1", "HRS2"))]

# Visualization
ggplot(gss_tbl, aes(x=workhours)) + geom_histogram()

# Analysis
train_cases <- sample(1:nrow(gss_tbl), .75*nrow(gss_tbl))

gss_train_tbl <- gss_tbl[train_cases, ]
gss_test_tbl <- gss_tbl[-train_cases, ]

training_folds <- createFolds(gss_train_tbl$workhours,
                              k=10)

# Function to time model fitting
time_model <- function(model) {
  tic()
  fit <- model
  toc()
  return(fit)
}

# Fit models and time execution
# fit model 1 without Parallelization
model1<- time_model(train( 
  workhours ~ .,
  gss_train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
))

model1_time <- model1$time
model1_t_elapsed<-model1_time$everything["elapsed"] # extract execution time

# fit model 1 with Parallelization
model1_par<- time_model(train(
  workhours ~ .,
  gss_train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T),
  allowParallel=TRUE
))
model1_par_time <- model1_par$time
model1_p_elapsed<-model1_par_time$everything["elapsed"] # extract time execution

# Holdout correlation reveals the real effectiveness of our model
hocv_cor_1 <- cor(
  predict(model1, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

# fit model 2 without Parallelization
model2<- time_model(train(
  workhours ~ .,
  gss_train_tbl, 
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
))
model2_time <- model2$time
model2_t_elapsed<-model2_time$everything["elapsed"] # extract execution time

# fit model 2 with Parallelization
model2_par<- time_model(train(
  workhours ~ .,
  gss_train_tbl, 
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T),
  allowParallel=TRUE
))
model2_par_time <- model2_par$time
model2_p_elapsed<-model2_par_time$everything["elapsed"] # extract execution time

# Holdout correlation reveals the real effectiveness of our model
hocv_cor_2 <- cor(
  predict(model2, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

# fit model 3 without Parallelization
model3<- time_model(train(
  workhours ~ .,
  gss_train_tbl, 
  method="ranger",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
))
model3_time <- model3$time
model3_t_elapsed<-model3_time$everything["elapsed"] # extract execution time

# fit model 3 with Parallelization
model3_par<- time_model(train(
  workhours ~ .,
  gss_train_tbl, 
  method="ranger",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T),
  allowParallel=TRUE
))
model3_par_time <- model3_par$time
model3_p_elapsed<-model3_par_time$everything["elapsed"] # extract execution time

# Holdout correlation reveals the real effectiveness of our model
hocv_cor_3 <- cor(
  predict(model3, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

# fit model 4 without Parallelization
model4<- time_model(train(
  workhours ~ .,
  gss_train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
))
model4_time <- model4$time
model4_t_elapsed<-model4_time$everything["elapsed"] # extract execution time

# fit model 4 with Parallelization
model4_par<- time_model(train(
  workhours ~ .,
  gss_train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T),
  allowParallel=TRUE
))
model4_par_time <- model4_par$time
model4_p_elapsed<-model4_par_time$everything["elapsed"] # extract execution time 

# Holdout correlation reveals the real effectiveness of our model
hocv_cor_4 <- cor(
  predict(model4, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

# Visualize k-fold cvs
summary(resamples(list(model1, model2, model3, model4)))
resample_sum <- summary(resamples(list(model1, model2, model3, model4)))
dotplot(resamples(list(model1, model2, model3, model4)))

# turn off parallel processing
stopCluster(local_cluster)
registerDoSEQ()

# Publication
table1_tbl <- tibble(
  algo = c("lm","glmnet","ranger","xgbTree"),
  cv_rsq = str_remove(round(
    resample_sum$statistics$Rsquared[,"Mean"],2
  ),"^0"),
  ho_rsq = str_remove(c(
    format(round(hocv_cor_1,2),nsmall=2),
    format(round(hocv_cor_2,2),nsmall=2),
    format(round(hocv_cor_3,2),nsmall=2),
    format(round(hocv_cor_3,2),nsmall=2)
  ),"^0")
)
table1_tbl

#add a new tibble to display execution time for each model
table2_tbl <- tibble( 
  algo = c("lm", "glmnet", "ranger", "xgbTree"),
  original = c(model1_t_elapsed, model2_t_elapsed, model3_t_elapsed, model4_t_elapsed),
  parallelized = c(model1_p_elapsed, model2_p_elapsed, model3_p_elapsed, model4_p_elapsed)
)
table2_tbl

#Which models benefited most from parallelization and why?
#In this case, the lm model benefited the most from parallelization, as its 
#execution time decreased from 6.15 seconds to 2 seconds. Parallelization can 
#help speed up the training process by using multiple cores to train the model 
#simultaneously. The benefits of parallelization depend on the specific algorithm 
#and its implementation. In this case, the lm model has the most effective 
#parallel implementation among the four models.

#How big was the difference between the fastest and slowest parallelized model? Why?
#The difference between the fastest and slowest parallelized model is approximately 
#128 seconds (130 seconds for xgbTree - 2 seconds for lm). The difference 
#in execution time can be attributed to the underlying algorithms and their complexity. 
#The xgbTree algorithm is more complex than the lm algorithm, as it involves building 
#multiple decision trees and optimizing them, while lm fits a linear regression model, 
#which has a simpler and more computationally efficient implementation.

#If your supervisor asked you to pick a model for use in a production model, 
#which would you recommend and why? Consider both Table 1 and Table 2 when providing an answer.
#The ideal model for production should balance both good performance metrics and 
#reasonable execution time. Looking at Table 1, the xgbTree model has the 
#highest cv_rsq and ho_rsq scores, indicating that it has the best predictive performance. 
#However, it also has the longest execution time in both original and parallelized versions, 
#as seen in Table 2. Considering the trade-offs between performance and execution time, 
#the glmnet model seems to be a good compromise. It has reasonable performance 
#metrics(cv_rsq = .86, ho_rsq = .57) that are pretty close to the second-best performance 
#and much shorter execution times(original = 3.86 seconds, parallelized = 3.80 seconds)
#compared to the xgbTree model.

