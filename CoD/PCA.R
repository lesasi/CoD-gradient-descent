pca_reduce<-function(x,y,new_dim){
  pca<-prcomp(x, retx=TRUE, center = TRUE, scale. = TRUE)
  reduced_x<-pca$x[,1:new_dim]
  reduced_dataset<-data.frame(cbind(y,reduced_x))
  return(reduced_dataset)
}
