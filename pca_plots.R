# Code snipper used to generate PCA variance plots.
# train_x_c and train_x_g are data.frames containing
# originial cell and gene features

library(factoextra)
pca_c<-prcomp(train_x_c, scale. = TRUE, center = TRUE)
pca_g<-prcomp(train_x_g, scale. = TRUE, center = TRUE)

fviz_eig(pca_g, xlab = "No. of Principal Components", ylab = "Variance Explained", choice = "variance", main = "", barfill = "dodgerblue2", barcolor = "red")

fviz_eig(pca_c, xlab = "No. of Principal Components", ylab = "Variance Explained", choice = "variance", main = "", barfill = "dodgerblue2", barcolor = "red")
