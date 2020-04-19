library(caret)

# function to return dataframes for X
readInData <- function(inputfilename,sep = ','){
  data_set <- read.csv(inputfilename,stringsAsFactors = FALSE,sep = sep)
  return(data_set)
}

fillInMissingElementsLifeExp <- function(X){
  
  X_dev = X[X[,"StatusDeveloping"] == 1,]
  X_com = X[X[,"StatusDeveloped"] == 1,]
  
  for(column in colnames(X)){
    data_set_processed_col <- X[column][!is.na(X[column])]
    X_dev_mean = mean(X_dev[column][!is.na(X_dev[column])])
    X_com_mean = mean(X_com[column][!is.na(X_com[column])])
    X[column][is.na(X[column] & X["StatusDeveloping"] == 1)] = X_dev_mean
    X[column][is.na(X[column] & X["StatusDeveloped"] == 1)] = X_com_mean
  }
  return(X)
}

fillInMissingElementsWine <- function(X){
  for(column in colnames(X)){
    data_set_processed_col <- X[column][!is.na(X[column])]
    X_mean = mean(data_set_processed_col)
    X[column][is.na(X[column])] = X_mean
  }
  return(X)
}
preProcessData <- function(X, drop_columns = c(), l_ignore = c(),choice = 0, categorical_columns = c(),X_trained = c()){
  
  X <- X[,!(names(X) %in% drop_columns),drop = FALSE]
  # one hot encoding 
  dmy <- dummyVars("~ .",data = X)
  X <- data.frame(predict(dmy, newdata = X))
  
  # analyseData(X,"../outputs/developing_or_developed.txt")
  # taking care of missing values: for now, setting them as the mean of the column
  if(choice == 0){
    X_test = fillInMissingElementsLifeExp(X)
  }
  if(choice == 1){
    X_test = fillInMissingElementsWine(X)
  }
  # removing outliers
  for(i in colnames(X)){
    # ignore these
    if(i %in% l_ignore){
      next
    }
    mean_X = mean(as.matrix(X[i]))
    std_X = sd(as.matrix(X[i]))
    X_test[i][X_test[i] > mean_X +3*std_X] = mean_X +3*std_X
    X_test[i][X_test[i] < mean_X -3*std_X] = mean_X -3*std_X
    # X_test = X_test[()&(X_test[,i] > mean_X - 3*std_X),]
    N = dim(X_test)[1]
  }
  X_scaled = X_test
  # scaled data- around a previous trained dataset if possible, otherwise default
  if(length(X_trained) ==0){
    X_scaled <- scale(X_scaled)
  }
  else{
    X_scaled <- scale(X_scaled, center=attr(X_trained, "scaled:center"),
                      scale=attr(X_trained, "scaled:scale"))
  }
  # returning processed and scaled data
  return(data.frame(X_scaled))
}

analyseData <- function(X,output_file){
  
  # sink(output_file)
  X_dev = X[X[,"StatusDeveloping"] == 1,]
  X_com = X[X[,"StatusDeveloped"] == 1,]
  for(column in colnames(X)){ 
    print(paste(column, ": Developing:", "Number:", length(X_dev[column][!is.na(X_dev[column])]), "Mean: ",mean(X_dev[column][!is.na(X_dev[column])])))
    print(paste(column, ": Developed :", "Number:", length(X_com[column][!is.na(X_com[column])]), "Mean: ",mean(X_com[column][!is.na(X_com[column])])))
    print(paste(column, ": Total     :", "Number:", length(X[column][!is.na(X[column])]), "Mean: ",mean(X[column][!is.na(X[column])])))
    print("------------------------------------------------------------------------------------------------------------")
  }
  # sink()
}