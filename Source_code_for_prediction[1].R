#Reading the test data given
test<-read.csv("test.csv",header=TRUE,na.strings = c(""))

#Removing the first column of test data as it's just content ids
test<-test[,-1]

#Below is a function to obtain the mode of a given column in a dataset
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

#Repeating the procedure of replacing NAs in test data with column means or mode depending on the variable class

for (var in 1:ncol(test)){
  if (class(test[,var]) %in%  c("numeric","integer")) {
    test[is.na(test[,var]),var] <- mean(test[,var], na.rm = TRUE)
  } else if (class(test[,var]) %in% c("character","factor")) {
    test[is.na(test[,var]),var] <- Mode(test[,var], na.rm = TRUE)
  }
}
#Predict the model on the test data.Setting type=probab gives us the class probabilities
fitted.results <- predict(fit,newdata=test,type='prob')

#Reading the submission format data
submission_set4<-read.csv("submission_format.csv",header=TRUE)

#Using only the second column as the target_bin as we need the probability that content is successful
submission_set4$target_bin<-fitted.results[,2]

#Writing the column target_bin into the submission_format data and naming it as the file name given
write.csv(submission_set4,file="MM13B021.csv",row.names = FALSE)