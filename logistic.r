library(glue)
library(tidyverse) 
library(MASS)
library(boot)
library(speedglm)
library(readr)

library(foreach)
library(doParallel)
cl<-makeCluster(20)     # modify this to number of parallel processes required
registerDoParallel(cl)


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
t<-test_y %>% dplyr::select(nfkb_inhibitor) 
 


predictors = names(train_y)
models = list()
loglosses = list()



foreach(i=1:length(predictors)  ,.packages=c("dplyr","speedglm")) %dopar% {
  
  train_y_predictor<-train_y %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  test_y_predictor<-test_y %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
  
  
  models[[predictors[i]]]<-speedglm(train_y_predictor~.,data = data.frame(train_x),family = binomial(),maxit = 200)
  pred = predict(models[[predictors[i]]],newdata = test_x,type="response")
  
  loss<-logloss(pred,test_y_predictor)
  loglosses[predictors[i]] = loss
  
  print(glue("The logloss for {predictor} on test data was {loss}"))
}

total = 0
for (loss in loglosses){total = total+loss}
print(glue("The mean(final) logloss of the model is: {total/length(loglosses)}."))
 

submission=list()
for(i in (predictors)){
  submission[[i]] = predict(models[[i]] , newdata = test_features,type="response")
}

write_csv(data.frame(submission), 'submission1.csv')
 


 


