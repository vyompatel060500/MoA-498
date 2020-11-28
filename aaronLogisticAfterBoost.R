
d <- readr::read_csv('MoA-498/preds_with_names.csv')
Y <- readr::read_csv('MoA-498/lish-moa/test_features.csv')

head(d)

enet_out <- cv.glmnet(d, Y$`5-alpha_reductase_inhibitor`, alpha=0.5, family='binomial')
bestlam <- enet_out$lambda.1se
