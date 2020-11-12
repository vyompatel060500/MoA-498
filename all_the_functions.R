
logloss<-function(predicted, actual)
{   #function to compute the Log-Loss
  
  # :param : actual- Ground truth (correct) 0-1 labels vector
  # :param : predicted- predicted values from the model
  # return: result- log-loss value
  result <- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
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