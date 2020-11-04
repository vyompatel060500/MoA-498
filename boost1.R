#!/bin/Rscript

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

fix_names <- function(df) {
  names(df) <- gsub('-', '_', names(df))
  df
}

path_why <- "./project498/MoA-498/"

train_features <- read_csv(glue("{path_why}lish-moa/train_features.csv")) %>% fix_names
train_scores <- read_csv(glue("{path_why}lish-moa/train_targets_scored.csv")) %>% fix_names
test_features <- read_csv(glue("{path_why}lish-moa/test_features.csv")) %>% fix_names

train = 1:10000
test = 20001:dim(train_features)[1]

train_x<-train_features[train,] %>% dplyr::select(-sig_id, -cp_type, -cp_time, -cp_dose)
test_x<-train_features[test,] %>% dplyr::select(-sig_id, -cp_type, -cp_time, -cp_dose)

sig_id<-test_features %>% dplyr::select(sig_id)
test_features<-test_features %>% dplyr::select(-sig_id, -cp_type, -cp_time, -cp_dose)


train_x_cg<-train_x%>%dplyr::select(starts_with('c') | starts_with('g'))
test_x_cg<-test_x%>%dplyr::select(starts_with('c') | starts_with('g'))

print(glue("Starting PCA..."))
pca_cg <- preProcess(train_x_cg, method = "pca", thresh = 0.8)
train_x_pca<-predict(pca_cg,train_x_cg)
test_x_pca<-predict(pca_cg, test_x_cg)
print(glue("Completed PCA!"))

train_y<-train_scores[train,]%>% dplyr::select(-sig_id)
test_y<-train_scores[test,]%>% dplyr::select(-sig_id)

train_y_predictor<-train_y %>% dplyr::select(nfkb_inhibitor) %>% unlist(use.names = FALSE)
models = list()
pvalues = list()

for(i in 1:length(train_x_pca)){
  models[[i]]<-glm(train_y_predictor~., data = train_x_pca[i])
  pvalues[[i]]<-coef(summary(models[[i]]))[2,4]
}

start_time<-Sys.time()
print(glue("Started training models..."))

cl<-makeCluster(10)
registerDoParallel(cl)
cv_10 <- trainControl(method = "cv", number = 10)
train_y_predictor<-train_y %>% dplyr::select(nfkb_inhibitor) %>% unlist(use.names = FALSE)
model<-train(as.factor(train_y_predictor)~., data = train_x_pca[,which(pvalues<0.001)], method = "xgbTree", trControl = cv_10 )
get_best_result(model)
stopCluster(cl)


end_time<-Sys.time()
diff=difftime(end_time,start_time,units="secs")
print(glue("Training Complete!"))
print(glue("Time taken for training models: {diff} seconds."))

print(glue("Starting predctions..."))
preds<-predict(model,newdata = test_x_pca)%>%unlist()
print(glue("Prediction complete"))

#preds <- pmax(pmin(as.numeric(preds), 1 - 1e-15), 1e-15)
logloss(preds,test_y$nfkb_inhibitor)

