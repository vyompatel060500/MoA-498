# install.packages("speedglm")
library(tidyverse) # metapackage of all tidyverse packages
library(MASS)
library(boot)
library(speedglm)
library(readr)

fix_names <- function(df) {
    names(df) <- gsub('-', '_', names(df))
    df
}

# train_features <- read_csv("lish-moa/train_features.csv") %>% fix_names
# train_scores <- read_csv("lish-moa/train_targets_scored.csv") %>% fix_names
# # test_features <- read_csv("lish-moa/test_features.csv") %>% fix_names
# sample_submission <- read_csv("lish-moa/sample_submission.csv") %>% fix_names

filter_features <- function(df) {
    df %>% dplyr::select(-sig_id, -cp_type, -cp_time, -cp_dose)
    idxs <- sample(ncol(df), 50)
    df[idxs]
}

train_x<-train_features %>% filter_features
show(train_x)
train_y <- train_scores %>% dplyr::select(-sig_id)
# test_x <- test_features %>% filter_features
dim(train_x)
dim(train_y)

idxs <- sort(sample(nrow(train_x), nrow(train_x) * 0.7))
real_train_x <- train_x[idxs,]
real_train_y <- train_y[idxs,]
real_test_x  <- train_x[-idxs,]
real_test_y  <- train_y[-idxs,]


print("here")
glm_fit<-speedglm(real_train_y$adenosine_receptor_agonist~ .,data = data.frame(real_train_x),family=binomial(), maxit=100)
summary(glm_fit)

pred[abs(pred) <= .Machine$double.eps * 2] <- 0

pred <- predict(glm_fit, newdata=real_test_x, type='response')
