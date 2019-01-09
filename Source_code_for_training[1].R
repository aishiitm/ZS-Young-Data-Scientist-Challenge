require(caret)
require(e1071)
require(randomForest)
zs_train_data<-read.csv('train.csv',header=TRUE,na.strings = c(""))

#Removing the first ciolumn as it's only the content id which is insignificant in prediction
zs_train_data<-zs_train_data[,-1]

#Below is a function to obtain the mode of a given column in a dataset
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
#Imputing missing values with mode and mean for categorical and numeric variables respectively
for (var in 1:ncol(zs_train_data)){
  if (class(zs_train_data[,var]) %in%  c("numeric","integer")) {
    zs_train_data[is.na(zs_train_data[,var]),var] <- mean(zs_train_data[,var], na.rm = TRUE)
  } else if (class(zs_train_data[,var]) %in% c("character","factor")) {
    zs_train_data[is.na(zs_train_data[,var]),var] <- Mode(zs_train_data[,var], na.rm = TRUE)
  }
}

train <-zs_train_data

#Fitting random Forest to the data for class prediction.Setting importance to TRUE helps us compute variable Importance later
fit <- randomForest(as.factor(target_bin)~.,data=train, importance=TRUE,ntree=1500)
#Plotting 15 top variables in the order of their importance-Package required'randomForest'
varImpPlot(fit,sort=TRUE,n.var=15)

