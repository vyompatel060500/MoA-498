

cat("\n\n## In setup_libs.R: Installing R pakcages ##########################################\n\n")


list.of.packages <- c( 
"ggplot2", 
"MASS",
"readr",
"dplyr", 
"tidyr",
"glue",
"purrr",
"tidyverse",
"boot",
"speedglm",
"foreach",
"doParallel",
"caret",
"e1071",
"xgboost",
"onehot",
"DBI",
"RSQLite",
"keras",
"tensorflow"
)

# create local user library path (not present by default)
dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
# install to local user library path
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, lib = Sys.getenv("R_LIBS_USER"), repos="http://cran.us.r-project.org")

lapply(list.of.packages, require, character.only = TRUE)


cat("\n\n## Exit setup_libs.R: Finish testing R pakcages ##########################################\n\n")



