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


logloss <- function(predicted, actual)
{   #function to compute the Log-Loss
  
  # :param : actual- Ground truth (correct) 0-1 labels vector
  # :param : predicted- predicted values from the model
  # :return : result- log-loss value
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}

convert_onehot<-function(x){
  # function to convert the categorical features to
  # one-hot encoding

  # :param : x - input data.frame containing categorical features
  # :return : temp_onehot : a data.frame containing all the input features
  #                         converted to one-hot encoding 
  input<-x
  trt_cnd = c('cp_type', 'cp_time', 'cp_dose')
  cp<-input[,trt_cnd]
  encoder<-onehot(cp)
  temp_onehot<-predict(encoder,cp)
  colnames(temp_onehot) <- c('type_ctl', 'type_cp', 'time_24', 'time_48', 'time_72', 'dose1', 'dose2')
  return(as_tibble(temp_onehot))
}


# path_why <- "/home/patel/project498/MoA-498/"
path_why <- "./"

# Read the data
train_features <- read_csv(glue("{path_why}lish-moa/train_features.csv")) 
train_scores <- read_csv(glue("{path_why}lish-moa/train_targets_scored.csv"))
test_features_input <- read_csv(glue("{path_why}lish-moa/test_features.csv"))
sample_submission<-read_csv(glue("{path_why}lish-moa/sample_submission.csv"))

# Train-test split
set.seed(498)
test = sample(1:nrow(train_features), nrow(train_features)/10)
train = -test

train_y<-train_scores[train,] %>% dplyr::select(-sig_id)
predictors = names(train_y)
num_cols_to_use = length(predictors)

#Grab sif_ids of train and test data sets
test_x_sig_id<-train_features[test,] %>% dplyr::select(sig_id)
test_features_sig_id<-test_features_input %>% dplyr::select(sig_id)

# store train and test datasets in separate variables after dropping the sig_ids. 
train_x<-train_features[train,] %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time)) %>%dplyr::select(-sig_id)
test_x<-train_features[test,] %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time)) %>% dplyr::select(-sig_id)
test_features<-test_features_input %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time)) %>% dplyr::select(-sig_id)
test_y<-train_scores[test,]%>% dplyr::select(-sig_id)


#One-Hot encoding
train_x_onehot<-convert_onehot(train_x)
test_x_onehot<-convert_onehot(test_x)
test_features_onehot<-convert_onehot(test_features)

# index of all data points which are ctl_vechicles
train_not_ctl = train_x_onehot$type_ctl != 1
test_not_ctl = test_x_onehot$type_ctl != 1
test_features_not_ctl = test_features_onehot$type_ctl != 1

#Separate dataset into g and c features
train_x_g<-train_x%>%dplyr::select(starts_with('g-'))
train_x_c<-train_x%>%dplyr::select(starts_with('c-'))
test_x_g<-test_x%>%dplyr::select(starts_with('g-'))
test_x_c<-test_x%>%dplyr::select(starts_with('c-'))
test_feat_g<-test_features%>%dplyr::select(starts_with('g-'))
test_feat_c<-test_features%>%dplyr::select(starts_with('c-'))


#Perform PCA#
print(glue("Starting PCA..."))
pca_g = preProcess(train_x_g, method = 'pca', thresh = 0.80)
pca_c = preProcess(train_x_c, method = 'pca', thresh = 0.80)
train_x_g<-predict(pca_g, train_x_g)
train_x_c<-predict(pca_c, train_x_c)
test_x_g<-predict(pca_g, test_x_g)
test_x_c<-predict(pca_c, test_x_c)
test_feat_g<-predict(pca_g, test_feat_g)
test_feat_c<-predict(pca_c, test_feat_c)
print(glue("Completed PCA!"))

# Modifiy colnames of train and test dataset to avoid conlflicts
names(train_x_g)<-glue("PCg-{c(1:length(train_x_g))}")
names(test_x_g)<-glue("PCg-{c(1:length(test_x_g))}")
names(test_feat_g)<-glue("PCg-{c(1:length(test_feat_g))}")

# combine all features of train-test dataset after pre-processing into corresponding variable
train_x_all<-(cbind(train_x_onehot, train_x_g, train_x_c) %>% as_tibble())[train_not_ctl,-c(1,2)]
test_x_all<-(cbind(test_x_onehot, test_x_g, test_x_c) %>% as_tibble())[,-c(1,2)]
test_features_all<-(cbind(test_features_onehot, test_feat_g, test_feat_c) %>% as_tibble())[,-c(1,2)]


# Initialize clusters to perform parallel model training
cl<-makeCluster(8)          # Change this to number of cores/parallel models to train. CAREFUL!!
registerDoParallel(cl)
start_time<-Sys.time()
print(glue("Started training models..."))

#Start Model Training
models<-foreach(i=1:length(predictors)  ,.packages=c("glue","dplyr","xgboost")) %dopar% {
  train_y_predictor<-train_y[train_not_ctl,] %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  datamatrix<-xgb.DMatrix(data = as.matrix(train_x_all), label = train_y_predictor)
  xgboost(data = datamatrix, eta = 0.05, max_depth=2, colsample_bynode=  0.5, colsample_bylevel = 0.5, 
          colsample_bytree = 1, subsample = 0.7, nrounds = 200, num_parallel_tree = 10, objective = 'binary:logistic', tree_method = 'hist', nthread=2)
}
end_time<-Sys.time()
diff=difftime(end_time,start_time,units="secs")
print(glue("Training Complete!"))
print(glue("Time taken for training models: {diff} seconds."))
stopCluster(cl)

# Predict on test data
print(glue("Starting predictions on test data..."))
test_preds<-foreach(i=1:num_cols_to_use  ,.packages=c("glue","dplyr","xgboost")) %do% {
  pred<-predict(models[[i]],newdata = as.matrix(test_x_all))
}
print(glue("Prediction complete!\n"))

# Add 0's to all data points which were ctl_vehicles
for(i in 1:length(test_preds)){
  test_preds[[i]][!test_not_ctl] = 0
}

#Predict on train data
print(glue("Starting predictions on train data..."))
train_preds<-foreach(i=1:num_cols_to_use  ,.packages=c("glue","dplyr","xgboost")) %do% {
  pred<-predict(models[[i]],newdata = as.matrix(train_x_all ))
}
print(glue("Prediction complete!\n"))

# Add 0's to ctl_vechicles
for(i in 1:length(train_preds)){
  train_preds[[i]][!train_not_ctl] = 0
}

# Calculate the log-loss score
print(glue("Starting logloss calculation..."))
loglosses<-foreach(i=1:num_cols_to_use  ,.packages=c("glue","dplyr","xgboost")) %do% {
  test_y_predictor<-test_y %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  temp <- pmax(pmin(as.numeric(test_preds[[i]]), 1 - 1e-15), 1e-15)
  logloss(temp,test_y_predictor)
}

train_loglosses<-foreach(i=1:num_cols_to_use  ,.packages=c("glue","dplyr","xgboost")) %do% {
  train_y_predictor<-train_y %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  temp <- pmax(pmin(as.numeric(train_preds[[i]]), 1 - 1e-15), 1e-15)
  logloss(temp,train_y_predictor)
}

print(glue("Logloss on test data: {mean(loglosses%>%unlist())}\n"))
print(glue("Logloss on train data: {mean(train_loglosses%>%unlist())}\n"))

#Generate the final submission file for Kaggle
for(i in 1:length(predictors)){
  pred = predict(models[[i]] , newdata = as.matrix(test_features_all))
  pred[!test_features_not_ctl] = 0
  sample_submission[[predictors[i]]] = pred
}

write_csv(sample_submission, 'submission.csv')

print("End...")
