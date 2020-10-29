library(glue)
library(tidyverse) 
library(MASS)
library(boot)
library(speedglm)
library(readr)
library(foreach)
library(doParallel)


logloss<-function(predicted, actual)
{   #function to compute the Log-Loss
  
  # :param : actual- Ground truth (correct) 0-1 labels vector
  # :param : predicted- predicted values from the model
  # return: result- log-loss value
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}

fix_names <- function(df) {
  names(df) <- gsub('-', '_', names(df))
  df
}


train_features <- read_csv("/home/patel/Downloads/lish-moa/train_features.csv") %>% fix_names
train_scores <- read_csv("/home/patel/Downloads/lish-moa/train_targets_scored.csv") %>% fix_names
test_features <- read_csv("/home/patel/Downloads/lish-moa/test_features.csv")%>% fix_names
sample <- read_csv("/home/patel/Downloads/lish-moa/sample_submission.csv") %>% fix_names

train = 1:20000

train_x<-train_features[train,] %>% dplyr::select(-sig_id, -cp_type, -cp_time, -cp_dose)
test_x<-train_features[-train,] %>% dplyr::select(-sig_id, -cp_type, -cp_time, -cp_dose)

train_y<-train_scores[train,]%>% dplyr::select(-sig_id)
test_y<-train_scores[-train,]%>% dplyr::select(-sig_id)


predictors = names(train_y)
models = list()
loglosses = list()

cl<-makeCluster(4)     # modify this to number of parallel processes required
registerDoParallel(cl)

start_time<-Sys.time()
print(glue("Started training models..."))
# The foreach loop trains models for each of the output variables and returns a list of
# models. To access i'th model, use models[[i]].
models<-foreach(i=1:length(predictors)  ,.packages=c("glue","dplyr","speedglm")) %dopar% {
  train_y_predictor<-train_y %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  speedglm(train_y_predictor~ . ,data = data.frame(train_x), family=binomial(), maxit = 250)
}
end_time<-Sys.time()
diff=difftime(end_time,start_time,units="secs")
print(glue("Time taken for training models: {diff} seconds."))
stopCluster(cl)

print(glue("Started prediction on trained models..."))
# This foreach loop predicts the probabilities using trained models foe each output
# variable and returns a list of predictions. To access 'ith model's prediction,
# use preds[[i]]
preds<-foreach(i=1:length(predictors)  ,.packages=c("glue","dplyr","speedglm")) %do% {
  predict(models[[i]],newdata = test_x,type="response")
}

print(glue("Computing loglosses of predicted outcomes..."))
# This foreach loop computes the logloss value for each output variable using
# the predictions generated from each model. To access i'th variable's logloss
# use loglosses[[i]]
loglosses<-foreach(i=1:length(predictors)  ,.packages=c("glue","dplyr","speedglm")) %do% {
  test_y_predictor<-test_y %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  logloss(preds[[i]],test_y_predictor)
}

print(glue("The mean(final) logloss of the model is: {mean(unlist(loglosses))}."))
 

submission=list()
for(i in 1:length(predictors)){
  submission[[i]] = predict(models[[i]] , newdata = test_features,type="response")
}
write_csv(data.frame(submission), 'submission1.csv')
 


 


