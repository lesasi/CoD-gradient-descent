classifySVM<-function(dataset){
  colnames(dataset)[1]<-"V1"
  set.seed(152)
  split<-sample.split(dataset,SplitRatio = 0.7)
  training_set<-subset(dataset,split==TRUE)
  testing_set<-subset(dataset,split==FALSE)
  classifier<-svm(formula=V1~.,data=training_set, type='C-classification',kernel='linear')
  y_pred<-predict(classifier, newdata = testing_set[,-1])
  y_act<-testing_set[,1]
  conf.matrix<-table(y_act,y_pred)
  accuracy<-mean(y_act==y_pred)
  return(list("cm"=conf.matrix,"acc"=accuracy))
}
