fld_reduce<-function(x,y,new_dim){
    #Representing each sample as column vector
    x<-t(x)
    y<-t(y)
    num_of_classes<-length(unique(t(y)))
    org_dim<-nrow(x)
    global_mean<-rowMeans(x)
    Sw<-matrix(0.0,org_dim,org_dim)   #Scatter matrix for within class covariance
    Sb<-matrix(0.0,org_dim,org_dim)   #Scatter matrix for between class covariance
    for (i in unique(y)){
      #CALCULATION OF WITHIN CLASS COVARIANCE
      class_data<-x[,y==i]
      #Setting mean to 0
      class_mean<-rowMeans(class_data)
      std_data<-sweep(class_data,1,class_mean)
      #Variance within classes
      Sk<-as.matrix(std_data)%*%t(as.matrix(std_data))
      #Sum of within class variance
      Sw<-Sw+Sk
      #CALCULATION OF BETWEEN CLASS COVARIANCE
      central_mean<-(class_mean-global_mean)%o%(class_mean-global_mean) #Outer product
      central_mean<-ncol(class_data)*central_mean
      Sb<-Sb+central_mean
    }
    #FINDING TRANFORMATION MATRIX W
    comp<-solve(Sw)%*%Sb
    eigenInfo<-rARPACK::eigs(comp, new_dim, which = "LM")   #Only compute eigen vectors for top new_dim eigen values
    W <- Re(eigenInfo$vectors) # transforming matrix
    reduced_x<-t(t(W)%*%x)
    reduced_dataset<-data.frame(cbind(t(y),reduced_x))
    return(reduced_dataset)
}