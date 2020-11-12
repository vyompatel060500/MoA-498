#!/bin/Rscript
print("Starting...")
library(glue)
library(tidyverse) 
library(MASS)
library(boot)
library(speedglm)
library(readr)
library(doParallel)
library(foreach)
library(caret)
library(e1071)
library(xgboost)
library(onehot)
#$//$ Define functions here: $//$


logloss<-function(predicted, actual)
{   #function to compute the Log-Loss
  
  # :param : actual- Ground truth (correct) 0-1 labels vector
  # :param : predicted- predicted values from the model
  # return: result- log-loss value
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}
important_features<-function(features, threshold){
  # returns all predictors with correlation less than threshold
  corr_matrix<-cor(features)
  columns<-rep(TRUE,nrow(corr_matrix))
  
  for(i in 1:length(columns) - 1){
    for(j in (i+1):length(columns)){
      if( length(corr_matrix[i,j]) > 0 && abs(corr_matrix[i,j])>= threshold){
        columns[j]<-FALSE
      }
    }
  }
  
  return (colnames(features)[columns])
}

convert_onehot<-function(x){
  input<-x
  trt_cnd = c('cp_type', 'cp_time', 'cp_dose')
  cp<-input[,trt_cnd]
  encoder<-onehot(cp)
  temp_onehot<-predict(encoder,cp)
  colnames(temp_onehot) <- c('type_ctl', 'type_cp', 'time_24', 'time_48', 'time_72', 'dose1', 'dose2')
  return(as_tibble(temp_onehot))
}


fix_names <- function(df) {
  names(df) <- gsub('-', '_', names(df))
  df
}

# path_why <- "./project498/MoA-498/"
path_why <- "/home/patel/project498/MoA-498/"

train_features <- read_csv(glue("{path_why}lish-moa/train_features.csv")) 
train_scores <- read_csv(glue("{path_why}lish-moa/train_targets_scored.csv"))
test_features_input <- read_csv(glue("{path_why}lish-moa/test_features.csv"))
sample_submission<-read_csv(glue("{path_why}lish-moa/sample_submission.csv"))
set.seed(420)
test = sample(1:nrow(train_features), nrow(train_features)/10)
train = -test
train_y<-train_scores[train,] %>% dplyr::select(-sig_id)
predictors = names(train_y)

test_x_sig_id<-train_features[test,] %>% dplyr::select(sig_id)
test_features_sig_id<-test_features_input %>% dplyr::select(sig_id)

train_x<-train_features[train,] %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time)) %>%dplyr::select(-sig_id)
test_x<-train_features[test,] %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time)) %>% dplyr::select(-sig_id)
test_features<-test_features_input %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time)) %>% dplyr::select(-sig_id)


test_y<-train_scores[test,]%>% dplyr::select(-sig_id)


#One-Hot encoding
train_x_onehot<-convert_onehot(train_x)
test_x_onehot<-convert_onehot(test_x)
test_features_onehot<-convert_onehot(test_features)

train_not_ctl = train_x_onehot$type_ctl != 1
test_not_ctl = test_x_onehot$type_ctl != 1
test_features_not_ctl = test_features_onehot$type_ctl != 1


train_x_cg<-train_x%>%dplyr::select(starts_with('c-') | starts_with('g'))
test_x_cg<-test_x%>%dplyr::select(starts_with('c-') | starts_with('g'))
test_features_cg<-test_features%>%dplyr::select(starts_with('c-') | starts_with('g'))

print(glue("Starting PCA..."))
pca_cg <- preProcess(train_x_cg, method = "pca", thresh = 0.8)
train_x_pca<-predict(pca_cg,train_x_cg)
test_x_pca<-predict(pca_cg, test_x_cg)
test_features_pca<-predict(pca_cg,test_features_cg)
print(glue("Completed PCA!"))

train_x_all<-(cbind(train_x_onehot, train_x_pca) %>% as_tibble())[train_not_ctl,-c(1,2)]
test_x_all<-(cbind(test_x_onehot, test_x_pca) %>% as_tibble())[,-c(1,2)]
test_features_all<-(cbind(test_features_onehot, test_features_pca) %>% as_tibble())[,-c(1,2)]


cl<-makeCluster(10)
registerDoParallel(cl)
start_time<-Sys.time()
print(glue("Started training models..."))

models<-foreach(i=1:length(predictors)  ,.packages=c("glue","dplyr","xgboost")) %dopar% {
  train_y_predictor<-train_y[train_not_ctl,] %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  datamatrix<-xgb.DMatrix(data = as.matrix(train_x_all), label = train_y_predictor)
  xgboost(data = datamatrix, max.depth = 2, eta = 0.2, nthread = 4, nrounds = 40, objective = "binary:logistic", tree_method = "gpu_hist")
}
end_time<-Sys.time()
diff=difftime(end_time,start_time,units="secs")
print(glue("Training Complete!"))
print(glue("Time taken for training models: {diff} seconds."))
stopCluster(cl)

print(glue("Starting predictions..."))
preds<-foreach(i=1:length(predictors)  ,.packages=c("glue","dplyr","xgboost")) %do% {
  pred<-predict(models[[i]],newdata = as.matrix(test_x_all))
}
print(glue("Prediction complete!\n"))


for(i in 1:length(preds)){
  preds[[i]][!test_not_ctl] = 0
}

print(glue("Starting logloss calculation..."))
loglosses<-foreach(i=1:length(predictors)  ,.packages=c("glue","dplyr","xgboost")) %do% {
  test_y_predictor<-test_y %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  
  temp <- pmax(pmin(as.numeric(preds[[i]]), 1 - 1e-15), 1e-15)
  logloss(temp,test_y_predictor)
}

print(glue("Logloss on test data: {mean(loglosses%>%unlist())}\n"))

for(i in 1:length(predictors)){
  pred = predict(models[[i]] , newdata = as.matrix(test_features_all))
  pred[!test_features_not_ctl] = 0
  sample_submission[[predictors[i]]] = pred
}

write_csv(sample_submission, 'submission.csv')

print("End...")