# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(parallel)
library(doParallel)

# turn on parallel processing
local_cluster <- makeCluster(14) #increase cores number
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
model1_t_elapsed<-round(model1_time$everything["elapsed"],2) # extract execution time

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
model1_p_elapsed<-round(model1_par_time$everything["elapsed"],2) # extract time execution

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
model2_t_elapsed<-round(model2_time$everything["elapsed"],2) # extract execution time

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
model2_p_elapsed<-round(model2_par_time$everything["elapsed"],2) # extract execution time

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
model3_t_elapsed<-round(model3_time$everything["elapsed"],2) # extract execution time

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
model3_p_elapsed<-round(model3_par_time$everything["elapsed"],2) # extract execution time

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
model4_t_elapsed<-round(model4_time$everything["elapsed"],2) # extract execution time

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
model4_p_elapsed<-round(model4_par_time$everything["elapsed"],2) # extract execution time 

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
table3<- tibble(
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
table3

#add a new tibble to display execution time for each model
table4<- tibble( 
  algo = c("lm", "glmnet", "ranger", "xgbTree"),
  original = c(model1_t_elapsed, model2_t_elapsed, model3_t_elapsed, model4_t_elapsed),
  parallelized = c(model1_p_elapsed, model2_p_elapsed, model3_p_elapsed, model4_p_elapsed)
)%>%
  rename(supercomputer = original, supercomputer_14 = parallelized) #rename column name
table4

#save tables to the out file
write_csv(table3, "../out/table3.csv") 
write_csv(table4, "../out/table4.csv")

#Which models benefited most from moving to the supercomputer and why?
#By looking at the output at the week11-cluster.Rout, it appears that the 
#'lm' and 'ranger' models benefited the most from moving to the supercomputer because
#these two model experienced greatest reduction in processing time. 

#What is the relationship between time and the number of cores used?
#In general, using more cores can lead to a reduction in execution time.
#When a task can be divided into smaller sub-tasks that can be run in parallel, 
#using more cores can lead to a reduction in the overall execution time. 
#This is because each core can handle a separate sub-task simultaneously, 
#thus allowing the entire task to be completed faster.However, the 
#actual speedup achieved depends on factors such as the parallelism of the task, 
#overhead, resource contention, and load balancing. 

#If your supervisor asked you to pick a model for use in a production model, 
#would you recommend using the supercomputer and why? Consider all four tables 
#when providing an answer.
#Based on the information provided in all four tables, I would recommend using 
#the supercomputer supercomputer reduced execution times for all models without change
#the performance metrics.I would recommend the 'ranger' (Random Forest) model for use, 
#as it demonstrates a significant improvement in execution time when run on the supercomputer and 
#good performance in terms of CV_rsq and holdout R-squared.

