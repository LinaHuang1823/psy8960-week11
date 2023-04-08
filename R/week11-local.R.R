# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(parallel)
library(doParallel)

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

# turn on parallel processing
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)

# Analysis
train_cases <- sample(1:nrow(gss_tbl), .75*nrow(gss_tbl))

gss_train_tbl <- gss_tbl[train_cases, ]
gss_test_tbl <- gss_tbl[-train_cases, ]

training_folds <- createFolds(gss_train_tbl$workhours,
                              k=10)
model1 <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

#model1

hocv_cor_1 <- cor(
  predict(model1, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2


model2 <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

#model2

hocv_cor_2 <- cor(
  predict(model2, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2


model3 <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="ranger",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

#model3

hocv_cor_3 <- cor(
  predict(model3, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2
model4 <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
#model4

hocv_cor_4 <- cor( # Holdout correlation reveals the real effectiveness of our model
  predict(model4, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

summary(resamples(list(model1, model2, model3, model4)))
resample_sum <- summary(resamples(list(model1, model2, model3, model4)))
dotplot(resamples(list(model1, model2, model3, model4)))


# Model training functions
train_model <- function(method, parallel) {
  if (parallel) {
    ctrl <- trainControl(method = "cv", number = 10, indexOut = training_folds, verboseIter = T, allowParallel = TRUE)
  } else {
    ctrl <- trainControl(method = "cv", number = 10, indexOut = training_folds, verboseIter = T, allowParallel = FALSE)
  }
  
  train(
    workhours ~ .,
    gss_train_tbl, 
    method = method,
    na.action=na.pass,
    preProcess=c("center", "scale", "nzv", "medianImpute"),
    trControl = ctrl
  )
}

# Time model training
methods <- c("lm", "glmnet", "ranger","xgbTree")
exec_times <- numeric(length(methods))
exec_times_parallel <- numeric(length(methods))

for (i in seq_along(methods)) {
  method <- methods[i]
  
  tic()
  train_model(method, parallel = FALSE)
  exec_times[i] <- toc(log = TRUE)$elapsed
  
  tic()
  train_model(method, parallel = TRUE)
  exec_times_parallel[i] <- toc(log = TRUE)$elapsed
}
exec_times
exec_times_parallel
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
table2_tbl <- tibble(  # add a new tibble called table2_tbl 
  algo = c("lm", "glmnet", "ranger", "xgbTree"),
  original = exec_times,
  parallelized = exec_times_parallel
)
# Turn off parallel processing
stopCluster(local_cluster)
registerDoSEQ()