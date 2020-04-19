sochasticAndMiniBatchGradientDescent <- function(y, X, eta, num_iterations,batch_size = 1,choice = 0,epsilon = 0.0001){
  
  X = data.frame(rep(1,length(y)),X)
  N = dim(X)[1]
  if(batch_size>1){
    num_groups = N/batch_size
    done = 0
    bins  <- rep(1:num_groups, nrow(X) / num_groups)
    gX = split(X, bins)
    gy = split(y,bins)
  }
  if(batch_size == 1){
    gX = as.matrix(X)
    gy = as.matrix(y)
  }
  # X = as.matrix(data.frame(rep(1,length(y)),X))
  # y = as.matrix(y)
  if(choice ==0)
    minloss = 0.30
  if(choice == 1){
    minloss = 0.58
  }
  iter_print = 0
  
  if(batch_size == 1){
    iter_print = 5
    iter_arr = nrow(gX)
    epsilon = epsilon*10
  }
  else{
    iter_print = 100
    iter_arr = length(gX)
  }

  theta = matrix(rep(0,dim(X)[2]),nrow = 1)
  l2loss = c()
  m = batch_size
  iter = 0
  prevloss = 0
  print("Starting algorithm...")
  for(i in 0:num_iterations){
    # random sample index
    iter = i
    if(i > 50 & batch_size == 1){
      break
    }
    if(batch_size == 0){
      gX = gX[sample.int(iter_arr),]
    }
    for(j in 1:iter_arr){
      
      if(batch_size>1){
        X_sample = as.matrix(gX[[j]])
        y_sample = as.matrix(gy[[j]])
      }else if(batch_size == 1){
        X_sample = gX[j,,drop = FALSE]
        y_sample = gy[j,,drop = FALSE]
        
        
      }
      # choose random sample from X and the corresponding y
      loss = sqrt(sum(((X_sample%*%t(theta)) - y_sample)^2)/(2*m))
      if(batch_size > 1){
        grad = (1/(m)) * (t(X_sample) %*% ((X_sample%*%t(theta)) - y_sample))
      }
      if(batch_size == 1){
        grad = (1/(m)) * (t(X_sample) %*% ((X_sample%*%t(theta)) - y_sample))
      }
      theta = theta - eta*t(grad)
      l2loss = c(l2loss,loss)
    }
    
    if(i%%iter_print == 0){
      print(paste("Iteration:",i," Loss:",loss))
      if((abs(prevloss - loss)<epsilon)){
        done = 1
        break
      }
      prevloss = loss
    }
  }
  
  print("Algorithm converged")
  values<-list("coef" = t(theta), "l2loss" = l2loss, "Num_iterations" = iter)
  return(values)

}


gradientDescent <- function(y, X, eta, num_iterations,epsilon = 0.05)
{
  X = as.matrix(cbind(rep(1,length(y)),X))
  y = as.matrix(y)
  N = length(y)
  # initial value of theta matrix
  theta = matrix(rep(0,dim(X)[2]),nrow = 1)
  # initial value of other components
  l2loss = c()
  loss = 0
  iter = 0
  # beginning iterations - using formula for calculating theta
  print("Starting algorithm.....")
  for(i in 0:num_iterations){
    prevloss = loss
    grad = (1/(N)) * (t(X) %*% ((X%*%t(theta)) - y))
    theta = theta - eta*t(grad)
    # loss calculation 
    loss = sqrt(sum(((X%*%t(theta)) - y)^2)/(2*N))
    if(i%%1000 == 0){
      print(paste("Iteration:",i," Loss:",loss))
    }
    l2loss = c(l2loss,loss)
    iter = i
    if(sqrt(sum(grad^2))<epsilon){
      break
    }
  }
  print("Algorithm converged")
  values<-list("coef" = t(theta), "l2loss" = l2loss, "Num_iterations:" = iter)
  return(values)
}

testTheta <- function(y,X,theta){
  
  X1 = as.matrix(data.frame(rep(1,length(y)),X))
  y1 = as.matrix(y)
  theta = t(as.matrix(theta))
  N = dim(X1)[1]
  e = t(y1) - theta%*%t(X1)
  rmsloss = sqrt(sum(e^2)/(2*N))
  mean_loss = 0
  for(i in e){
    mean_loss = mean_loss + abs(i)
  }
  return(list(rmsloss,mean_loss/N))
}



