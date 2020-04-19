#Set current working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#Include files
source("FLD.r")
source("SVD.r")
source("PCA.r")
source("PLS.r")
source("classifySVM.r")
#Include libraries
library(e1071)
library(pls)
library(MASS)
library(caTools)
library(kernlab)
x<-read.table("madelon_train.data",sep=" ")[,-501]
y<-read.table("madelon_train.labels",sep=" ")
dataset=cbind(y,x)
#Original
org<-classifySVM(dataset)

#Applying SVD
svd_dataset<-svd_reduce(x,y,20)
svd<-classifySVM(svd_dataset)

#Applying PCA
pca_dataset<-pca_reduce(x,y,40)
pca<-classifySVM(pca_dataset)


#Applying FLD
fld_dataset<-fld_reduce(pca_dataset[,-1],pca_dataset[,1],15)
fld<-classifySVM(fld_dataset)


#Applying PLS
pls_dataset<-pls_reduce(pca_dataset,15)
pls<-classifySVM(pls_dataset)

cat("Accuracy for original dataset: ", org$acc,"\n")
cat("Confusion matrix for original dataset\n")
print(org$cm)

cat("Accuracy for SVD reduced dataset: ", svd$acc,"\n")
cat("Confusion matrix for SVD reduced dataset\n")
print(svd$cm)

cat("Accuracy for PCA reduced dataset: ", pca$acc,"\n")
cat("Confusion matrix for PCA reduced dataset\n")
print(pca$cm)

cat("Accuracy for PCA+FLD reduced dataset: ", fld$acc,"\n")
cat("Confusion matrix for PCA+FLD reduced dataset\n")
print(fld$cm)

cat("Accuracy for PCA+PLS reduced dataset: ", pls$acc,"\n")
cat("Confusion matrix for PCA+PLS reduced dataset\n")
print(pls$cm)
