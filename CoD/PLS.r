#install.packages("pls")
pls_reduce<-function(dataset,new_dim)
{
  train<-sample.split(dataset,SplitRatio = 0.7)
  pls.fit=plsr(V1~., data=dataset[train,], scale=TRUE,ncomp=new_dim,validation="CV")
  z<-predict(pls.fit,dataset,direction="forward")
  z<-as.data.frame(z)
  z<-cbind(y,z)
  return(z)
}
