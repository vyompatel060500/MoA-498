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

options(error=recover)

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


############
# setup

pred_csvs_dir <- "grid_search_pred_csvs"
dir.create(file.path(getwd(), pred_csvs_dir))
loglosses_dir <- "grid_search_logloss_rds_files"
dir.create(file.path(getwd(), loglosses_dir))

source('UTIL-db.R')
init_db()

# path_why <- "./project498/MoA-498/"
# path_why <- "/home/patel/project498/MoA-498/"
path_why <- "./"

train_features <- read_csv(glue("{path_why}lish-moa/train_features.csv")) 
train_scores <- read_csv(glue("{path_why}lish-moa/train_targets_scored.csv"))
test_features_input <- read_csv(glue("{path_why}lish-moa/test_features.csv"))
sample_submission<-read_csv(glue("{path_why}lish-moa/sample_submission.csv"))
# tSNE<-read_csv(glue("{path_why}lish-moa/tsne4dims.csv"))

drop_ctl <- FALSE
with_pca <- TRUE
with_important_only <- FALSE

#preprocess_dataset <- function(X){
#  all_x <- X %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time))
#  #One-Hot encoding
#  all_x_onehot<-convert_onehot(all_x)
#  all_not_ctl = all_x_onehot$type_ctl != 1
#  all_x_g<-all_x%>%dplyr::select(starts_with('g-'))
#  all_x_c<-all_x%>%dplyr::select(starts_with('c-'))
#  print(glue("Starting PCA..."))
#  all_pca_g = preProcess(all_x_g, method = 'pca', thresh = 0.80)
#  all_pca_c = preProcess(all_x_c, method = 'pca', thresh = 0.80)
#  print(glue("Completed PCA!"))
#  names(all_x_g)<-glue("PCg-{c(1:length(all_x_g))}")
#  if(drop_ctl) {
#    all_x_all<-(cbind(all_x_onehot, all_x_g, all_x_c) %>% as_tibble())[all_not_ctl,-c(2)]
#  } else {
#    all_x_all<-(cbind(all_x_onehot, all_x_g, all_x_c) %>% as_tibble())[ ,-c(2)]
#  }
#  all_x_all
#}

# end setup
#########


#set.seed(498)
test = sample(1:nrow(train_features), nrow(train_features)/10)
train = -test
train_y <- train_scores[train,]
test_y  <- train_scores[test, ]
predictors = names(train_y %>% dplyr::select(-sig_id))

all_x <-rbind(train_features, test_features) %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time))
train_x<-train_features[train,] %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time))
test_x<-train_features[test,] %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time))
test_features<-test_features_input %>% dplyr::mutate(cp_type = factor(cp_type), cp_dose = factor(cp_dose), cp_time = factor(cp_time))
#tSNE_train<-tSNE[train,]
#tSNE_test<-tSNE[test,]

all_x_sig_id <- tibble(sig_id=all_x$sig_id)
train_x_sig_id <- tibble(sig_id=train_x$sig_id)
test_x_sig_id <- tibble(sig_id=test_x$sig_id)
test_features_sig_id <- tibble(sig_id=test_features$sig_id)

#One-Hot encoding
all_x_onehot<-convert_onehot(all_x)
train_x_onehot<-convert_onehot(train_x)
test_x_onehot<-convert_onehot(test_x)
test_features_onehot<-convert_onehot(test_features)

all_not_ctl = all_x_onehot$type_ctl != 1
train_not_ctl = train_x_onehot$type_ctl != 1
test_not_ctl = test_x_onehot$type_ctl != 1
test_features_not_ctl = test_features_onehot$type_ctl != 1

all_x_g<-all_x%>%dplyr::select(starts_with('g-') )
all_x_c<-all_x%>%dplyr::select(starts_with('c-') )
train_x_g<-train_x%>%dplyr::select(starts_with('g-') )
train_x_c<-train_x%>%dplyr::select(starts_with('c-') )
test_x_g<-test_x%>%dplyr::select(starts_with('g-') )
test_x_c<-test_x%>%dplyr::select(starts_with('c-') )
test_feat_g<-test_features%>%dplyr::select(starts_with('g-') )
test_feat_c<-test_features%>%dplyr::select(starts_with('c-') )

if(with_pca) {
  print(glue("Starting PCA..."))
  pca_g = preProcess(all_x_g, method = 'pca', thresh = 0.80)
  pca_c = preProcess(all_x_c, method = 'pca', thresh = 0.90) # thresh 0f 0.80 lead to only 2 PCs
  all_x_g<-predict(pca_g, all_x_g)
  all_x_c<-predict(pca_c, all_x_c)
  train_x_g<-predict(pca_g, train_x_g)
  train_x_c<-predict(pca_c, train_x_c)
  test_x_g<-predict(pca_g, test_x_g)
  test_x_c<-predict(pca_c, test_x_c)
  test_feat_g<-predict(pca_g, test_feat_g)
  test_feat_c<-predict(pca_c, test_feat_c)

  names(all_x_g)<-glue("PCg-{c(1:length(all_x_g))}")
  names(train_x_g)<-glue("PCg-{c(1:length(train_x_g))}")
  names(test_x_g)<-glue("PCg-{c(1:length(test_x_g))}")
  names(test_feat_g)<-glue("PCg-{c(1:length(test_feat_g))}")

  print(glue("Completed PCA!"))
} else if(with_important_only) {
  if(with_pca) stop("ERROR: Can't have both with_pca and with_important_only")
  # TODO: Apply important feature only filtering.
}


if(drop_ctl){
  all_x_all<-(cbind(all_x_sig_id, all_x_onehot, all_x_g, all_x_c) %>% as_tibble())[all_not_ctl,-c(3)]
  train_x_all<-(cbind(train_x_sig_id, train_x_onehot, train_x_g, train_x_c) %>% as_tibble())[train_not_ctl,-c(3)]
  test_x_all<-(cbind(test_x_sig_id, test_x_onehot, test_x_g, test_x_c) %>% as_tibble())[test_not_ctl,-c(3)]
  test_features_all<-(cbind(test_features_sig_id, test_features_onehot, test_feat_g, test_feat_c) %>% as_tibble())[test_features_not_ctl,-c(3)]
} else {
  all_x_all<-(cbind(all_x_sig_id, all_x_onehot, all_x_g, all_x_c) %>% as_tibble())[ ,-c(3)]
  train_x_all<-(cbind(train_x_sig_id, train_x_onehot, train_x_g, train_x_c) %>% as_tibble())[ ,-c(3)]
  test_x_all<-(cbind(test_x_sig_id, test_x_onehot, test_x_g, test_x_c) %>% as_tibble())[,-c(3)]
  test_features_all<-(cbind(test_features_sig_id, test_features_onehot, test_feat_g, test_feat_c) %>% as_tibble())[,-c(3)]
}

train_models <- function(nrounds, ...) {
    params = list(...)

    #these_pars_name <- glue('loglosses_xgboostgridsearch_{paste0(names(params), params, "nrounds", nrounds, collapse="_")}')
    #these_pars_name <- glue('xgbgs_nrounds{nrounds}_{paste0(names(params), params, collapse="_")}')
    these_pars_name <- glue('xgbgs_nrounds_with_scale_pos_weight_{nrounds}_{paste0(names(params), params, collapse="_")}')

    these_pars_rds <- glue('{these_pars_name}.rds')
    these_pars_csv <- glue('{these_pars_name}.csv')

    if(file.exists(these_pars_rds)) return(NA) # Short circuit if we already tried these params

    cl<-makeCluster(1)
    registerDoParallel(cl)
    start_time<-Sys.time()
    print(glue("Started training models..."))

    num_cols_to_use <- length(predictors)
    #num_cols_to_use <- 2

    models<-foreach(i=1:num_cols_to_use, .packages=c("glue","dplyr","xgboost"), .export=ls(globalenv())) %dopar% {
      if(drop_ctl){
	train_y_predictor <- train_y[train_not_ctl,] %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
      } else {
	train_y_predictor <- train_y %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
      }
      datamatrix<-xgb.DMatrix(data = as.matrix(train_x_all %>% dplyr::select(-sig_id)), label = train_y_predictor)
      #p = list(colsample_bynode=0.8, learning_rate=1, max_depth=5, num_parallel_tree=100, objective='binary:logistic', subsample=0.8, tree_method='gpu_hist')
      ### pos_scaling was bad!
      ## tux5: tune scale_pos_wegiht on a per model basis
      #params$scale_pos_weight = sum(train_y_predictor==1)/length(train_y_predictor)
      #params
      xgboost(data = datamatrix, nrounds=nrounds, params = params, nthread=2)
    }
    end_time<-Sys.time()
    diff=difftime(end_time,start_time,units="secs")
    print(glue("Training Complete!"))
    print(glue("Time taken for training models: {diff} seconds."))
    stopCluster(cl)

    print(glue("Starting predictions..."))
    preds<-foreach(i=1:num_cols_to_use  ,.packages=c("glue","dplyr","xgboost")) %do% {
      pred<-predict(models[[i]],newdata = as.matrix(test_x_all))
    }
    print(glue("Prediction complete!\n"))


    for(i in 1:length(preds)){
      preds[[i]][!test_not_ctl] = 0
    }

    print(glue("Starting logloss calculation..."))
    loglosses<-foreach(i=1:num_cols_to_use  ,.packages=c("glue","dplyr","xgboost")) %do% {
      test_y_predictor<-test_y %>% dplyr::select(-sig_id) %>% dplyr::select(predictors[i]) %>% unlist(use.names = FALSE)
      
      temp <- pmax(pmin(as.numeric(preds[[i]]), 1 - 1e-15), 1e-15)
      logloss(temp,test_y_predictor)
    }

    # new_preds<-matrix(nrow = dim(test_x)[1], ncol = length(predictors))
    # dimnames(new_preds) = list(test_x_sig_id %>% unlist(), predictors)
    # new_preds<-data.frame(new_preds)
    # for(i in 1:length(predictors)){
    #   new_preds[i] = preds[[i]]
    # }

    new_preds<-matrix(nrow = dim(test_x)[1], ncol = num_cols_to_use)
    dimnames(new_preds) = list(test_x_sig_id %>% unlist(), predictors)
    new_preds<-data.frame(new_preds)
    for(i in 1:num_cols_to_use){
      new_preds[i] = preds[[i]]
    }

    write_csv(new_preds, file.path(pred_csvs_dir, these_pars_csv))
    saveRDS(loglosses, file.path(loglosses_dir, these_pars_rds))

    ll <- mean(unlist(loglosses))
    return_value=glue("Logloss on test data: {mean(unlist(loglosses))}; nrounds:{nrounds} params: {paste(names(params), params, collapse=',')}\n")
    print(return_value)
    #write(return_value, file="XGB_LOGLOSS_METADATA.txt", append=TRUE)
    insert_result(res=loglosses, logloss=ll, ..., nrounds=nrounds, with_pca=with_pca, with_important_only=with_important_only, drop_ctl=drop_ctl)
    return_value
}

nodename <- Sys.info()['nodename']
if(nodename=="tux5") {
  eta = c(0.05, 0.1, 0.15, 0.2)
  # NOTE: Also setting pos_weight_scaling
  # nrounds = 2000 # lead to ll of about 0.19...; Not good enough
  nrounds = 300
  num_parallel_tree = 10
} else if(nodename=="tux6") {
  eta = c(0.01, 0.03, 0.05)
  nrounds = 200
  num_parallel_tree = 10
} else if(nodename=="tux7") {
  eta = 0.05
  nrounds = 200
  num_parallel_tree = 100
} else if(nodename=="tux8") {
  # NOTE: HAD to change naming convention for this one.
  # NOTE: Also set grow_policy=lossguide for this one.
  # eta = 0.2; nrounds = 10; num_parallel = 1000 ==> garbage; 0.07....
  eta = 0.05
  nrounds = 200
  num_parallel_tree = 10
} else {
  # testing on local
  eta = 0.05
  nrounds = 2
  num_parallel_tree = 1
}


# Maximum delta step we allow each leaf output to be. If the value is set to 0, it means there is no constraint. If it is set to a positive value, it can help making the update step more conservative. Usually this parameter is not needed, but it might help in logistic regression when class is extremely imbalanced. Set it to value of 1-10 might help control the update.

param_grid <- expand.grid(
   list(
     eta=eta,
     max_delta_step=c(3),
     #colsample_bynode=c(0.7, 0.3),
     colsample_bynode=0.3, # 0.3 seems to do better
     max_depth=c(2,3),
     # max_depth=c(1,2), # depth 1 == stumps; Gives an additive model with no interactions modeled; STUMPS WEREN't good
     #max_depth=c(3,6), # default is 6; 6 was bad
     num_parallel_tree=num_parallel_tree,
     objective='binary:logistic',
     #subsample=c(1.0, 0.7),
     #subsample=0.7, # almost like cross val
     subsample=0.5, # almost like cross val
     #sampling_method='gradient_based', # Might be good for imbalanced dataset?? ONLY SUPPORTED for 'gpu_hist'; Set subsample as low as 0.1 here; (subsample >= 0.5 for uniform sampling)
     # scale_pos_weight on tux5 only
     #scale_pos_weight=0.3,
     # lossguide on tux8 only.
     grow_policy='depthwise',
     #grow_policy='lossguide', # lossguide: split at nodes with highest loss change; As opposed to: depthwise: split at nodes closest to the root.
     booster='gbtree',
     tree_method='hist' # faster!
     #tree_method='exact'
   ),
   stringsAsFactors=FALSE
)

#if(nodename=="tux7") {
#  # Short circuit and do logistic regression.
#  # nrounds = 20 lead to 0.19... almost there
#  # nrounds = 200 lead to 0.28.... Not good
#  # nrounds = 200 lead to 0.18 with lambda = 1; NOT BAD!
#  nrounds = 100
#  param_grid <- expand.grid(
#     list(
#       lambda=c(0.5, 1),
#       alpha=0,
#       colsample_bynode=c(0.3, 0.7),
#       booster='gblinear',   
#       objective='binary:logistic',
#       subsample=c(0.4, 0.7) # almost like cross val
#     ),
#     stringsAsFactors=FALSE
#  )
#}

results <- purrr::pmap(param_grid, function(...) {
    train_models(nrounds, ...)
})

#for(i in 1:length(predictors)){
#  pred = predict(models[[i]] , newdata = as.matrix(test_features_all))
#  pred[!test_features_not_ctl] = 0
#  sample_submission[[predictors[i]]] = pred
#}
#
#write_csv(sample_submission, 'submission.csv')

print("End...")
