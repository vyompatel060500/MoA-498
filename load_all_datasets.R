
library(glue)
library(readr)

path_why <- ""

train_features <- read_csv(glue("{path_why}lish-moa/train_features.csv"))     # %>% fix_names
train_scores <- read_csv(glue("{path_why}lish-moa/train_targets_scored.csv")) # %>% fix_names
test_features <- read_csv(glue("{path_why}lish-moa/test_features.csv"))       # %>% fix_names
train_drug    <- read_csv(glue("{path_why}lish-moa/train_drug.csv"))          # %>% fix_names