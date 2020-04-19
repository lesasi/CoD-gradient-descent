source("gradient_descent.R")
source("preProcess.R")
options(scipen = 999)

life_expectancy <- "../datasets/life-expectancy/Life Expectancy Data.csv"
white_wine <- "../datasets/Wine/winequality-white.csv"
red_wine <- "../datasets/Wine/winequality-red.csv"

# Menu:
choice <- readline(prompt = "Enter dataset to train on:[0 for Life-Expectancy, 1 for Wine]")
if(choice != 0 & choice != 1){
  stop("User Input Error: Only values 0,1 allowed!! Exiting...")
}
print("Algorithms:")
print("1. Gradient Descent")
print("2. Stocastic Gradient Descent")
print("3. Stocastic Gradient Descent- Mini Batch")
al <- readline(prompt = "Enter algorithm to train[1,2,3]:")

if(al == 1){
  gradientAlgoUsed = 0
}else if(al == 2){
  gradientAlgoUsed = 1
  stochastic_percentage = 1
}else if(al == 3){
  gradientAlgoUsed = 1
  # stochastic_percentage <- as.numeric(readline(prompt = "Enter batch size[Decimal value from 0-1]"))
  stochastic_percentage <- 64
}else{
  stop("User Input Error: Only values 1,2,3 allowed!! Exiting....")
  
}
init_num_iterations <- 100000
init_learning_rate <- as.numeric(readline(prompt = "Enter learning rate- Recommended range[0.00001,0.0001]:"))


if(choice == 0){
  # life expectancy data: 
  data_set <- readInData(life_expectancy)
  data_set_processed <- preProcessData(data_set,c('Country'),c("Year","StatusDeveloped","StatusDeveloping"))
  # splitting into 80-20 split for train-test data
  index <- createDataPartition(data_set_processed$Life.expectancy,p=0.8, list=FALSE,times=1)
  train_data = data_set_processed[index,]
  test_data = data_set_processed[-index,]
  train_y = train_data["Life.expectancy"]
  train_X = train_data[,!(names(train_data) %in% c("Life.expectancy")),drop = FALSE]
  test_y = test_data["Life.expectancy"]
  test_X = test_data[,!(names(test_data) %in% c("Life.expectancy")),drop = FALSE]
}

if(choice == 1){
  data_set_1 <- readInData(white_wine,sep = ';')
  data_set_processed_1 <- preProcessData(X = data_set_1,choice = choice)
  data_set_2 <- readInData(red_wine,sep = ';')
  data_set_processed_2 <- preProcessData(X = data_set_2,choice = choice)
  data_set_processed <- rbind(data_set_processed_1,data_set_processed_2)
  # splitting into 80-20 split for train-test data
  index <- createDataPartition(data_set_processed$quality,p=0.8, list=FALSE,times=1)
  train_data = data_set_processed[index,]
  test_data = data_set_processed[-index,]
  train_y = train_data["quality"]
  train_X = train_data[,!(names(train_data) %in% c("quality")),drop = FALSE]
  test_y = test_data["quality"]
  test_X = test_data[,!(names(test_data) %in% c("quality")),drop = FALSE]
}

# the actual training

ptm <- proc.time()
if(gradientAlgoUsed == 0){
  grad = gradientDescent(train_y,train_X,init_learning_rate,init_num_iterations)
}
if(gradientAlgoUsed == 1){
  grad = sochasticAndMiniBatchGradientDescent(train_y,train_X,init_learning_rate,init_num_iterations,stochastic_percentage,choice)
}

time_taken = proc.time() - ptm
print(paste("Training took ",time_taken[1]," seconds"))
# results to be saved in file
l2 = data.frame(grad$l2loss)
theta = data.frame(grad$coef)
iterations_done = grad$Num_iterations

print("Checking error of test data........")
test_data = testTheta(test_y,test_X,theta)
test_rms_loss = test_data[1]
test_mean_loss = test_data[2]
# print(paste("RMS loss= ",test_rms_loss, " Mean loss = ",test_mean_loss))


# making list of variables to save to file:
result_list = list("theta" = theta, "l2loss_train" = l2, "iterations" = iterations_done, "l2loss_test" = test_rms_loss, "meanloss_test" = test_mean_loss, "training_time" = time_taken[1])
if(choice == 0){
  parent_folder = "../outputs/Life-Expectancy/"
  data_name = "Life-Expectancy"
}
if(choice == 1){
  parent_folder = "../outputs/Wine/"
  data_name = "Wine"
}
if(gradientAlgoUsed == 0){
  filename = paste("GD_",init_learning_rate,sep = "")
  textq = paste("Results for",data_name,"dataset:","\nGradient Descent- With learning rate:", init_learning_rate,"Done in ",iterations_done, " iterations:")
}
if(gradientAlgoUsed == 1){
  if(stochastic_percentage == 1){
    filename = paste("SGD_",init_learning_rate,sep = "")
    textq = paste("Results for",data_name,"dataset:","\nStochastic Gradient Descent- With learning rate:", init_learning_rate,"Done in",iterations_done, "iterations:")
  }
  else{
    filename = paste("SGD-MiniBatch_",init_learning_rate,sep = "")
    textq = paste("Results for",data_name,"dataset:","\nSGD-MiniBatch_",stochastic_percentage,"batch- With learning rate:", init_learning_rate,"Done in",iterations_done, "iterations:")
  }
}
textq = paste(textq,"\nAfter training and testing on selected test data:\n",
              "loss: ",test_rms_loss,
              "\n", "Mean loss: ",test_mean_loss,
              "\n", "Time taken for training: ",time_taken[1],"s")
full_file_name = paste(parent_folder,filename,".txt",sep = "")
print(full_file_name)
fileConn<-file(full_file_name)
writeLines(c(textq), fileConn)
close(fileConn)
# saveRDS(result_list, file = "../outputs/results.rds")
print(paste("Results for",data_name,"dataset:"))
cat(textq)
cat(paste("\nResults written to file:",full_file_name))