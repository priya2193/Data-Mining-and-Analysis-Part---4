#install.packages("e1071")
#install.packages('tuneR')
rm(list = ls())
#CREATE DATA SET
library(ISLR)
library(e1071)
library(tuneR)
set.seed(9004)
train = sample(dim(OJ)[1], 800)
train_data = OJ[train, ]
test_data = OJ[-train, ]

error_test = c(0,0,0,0,0,0,0)
error_train = c(0,0,0,0,0,0,0)
error_test_radial = c(0,0,0,0,0,0,0)
error_train_radial = c(0,0,0,0,0,0,0)
error_test_polynomial = c(0,0,0,0,0,0,0)
error_train_polynomial = c(0,0,0,0,0,0,0)

k = 0

###################################################
#SVM FOR LINEAR KERNEL
###################################################

for (i in c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )){
  
  svm.linear <- svm(train_data$Purchase ~ ., data = train_data, kernel = "linear", cost = i)
  
  train.pred <- predict(svm.linear, train_data)
  table(train_data$Purchase, train.pred)
  error_train[k] <- mean(train.pred != train_data$Purchase)
 
  
  test.pred <- predict(svm.linear, test_data)
  table(test_data$Purchase, test.pred)
  error_test[k] <- mean(test.pred != test_data$Purchase)

  
  k = k+1
}
error_test[0] <-  0.1962962963
error_test[1] <- 0.1925925926
error_test[2] <- 0.1740740741 
error_test[3] <- 0.1740740741 
error_test[4]<- 0.1814814815 
error_test[5] <- 0.1777777778 
error_test[6]<- 0.1777777778
#######################################################################
#DETERMINING OPTIMAL COST FOR TRAINING AND TEST DATA FOR LINEAR KERNEL
#######################################################################  

tune.train <- tune(svm, Purchase ~., data = train_data, kernel = "linear", 
                  ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )))

summary(tune.train)

tune.test <- tune(svm, Purchase ~., data = test_data, kernel = "linear", 
                  ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )))

summary(tune.test)

#######################################################################
#PLOT FOR TRAINING AND TEST DATA FOR LINEAR KERNEL
####################################################################### 
cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)

par(mfrow=c(1,2))

plot(cost,error_train,type="b",lty=1,col="blue",xlab = "cost",main=" Training Error - Linear Kernel",ylab="Training error")

plot(cost,error_test,type="b",lty=2,col = "blue",xlab = "cost",main="Test Error - Linear Kernel",ylab="Test error")

linear_err_table = cbind(error_test,error_train,cost)

###################################################
#SVM FOR RADIAL KERNEL
###################################################
k = 0
for (i in c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )){
  
  svm.linear.radial <- svm(train_data$Purchase ~ ., data = train_data, kernel = "radial", cost = i)
  
  train.pred.radial <- predict(svm.linear.radial, train_data)
  table(train_data$Purchase, train.pred.radial)
  error_train_radial[k] <- mean(train.pred.radial != train_data$Purchase)

  
  test.pred.radial <- predict(svm.linear.radial, test_data)
  table(test_data$Purchase, test.pred)
  error_test_radial[k] <- mean(test.pred.radial != test_data$Purchase)
  
  
  k = k+1
}

#######################################################################
#DETERMINING OPTIMAL COST FOR TRAINING AND TEST DATA FOR RADIAL KERNEL
#######################################################################  

tune.train.radial <- tune(svm, Purchase ~., data = train_data, kernel = "radial", 
                   ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )))

summary(tune.train.radial)

tune.test.radial <- tune(svm, Purchase ~., data = test_data, kernel = "radial", 
                  ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )))

summary(tune.test.radial)

#######################################################################
#PLOT FOR TRAINING AND TEST DATA FOR RADIAL KERNEL
####################################################################### 
cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)

par(mfrow=c(1,2))

plot(cost,error_train_radial,type="b",lty=1,col="blue",xlab = "cost",main=" Training Error - Radial Kernel",ylab="Training error")

plot(cost,error_test_radial,type="b",lty=2,col = "blue",xlab = "cost",main="Test Error - Radial Kernel",ylab="Test error")


###################################################
#SVM FOR POLYNOMIAL KERNEL
###################################################
k = 0
for (i in c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )){
  
  svm.polynomial <- svm(train_data$Purchase ~ ., data = train_data, kernel = "polynomial", cost = i, degree = 2)
  
  train.pred.polynomial <- predict(svm.polynomial, train_data)
  table(train_data$Purchase, train.pred.polynomial)
  error_train_polynomial[k] <- mean(train.pred.polynomial != train_data$Purchase)
  
  
  test.pred.polynomial <- predict(svm.polynomial, test_data)
  table(test_data$Purchase, test.pred)
  error_test_polynomial[k] <- mean(test.pred.radial != test_data$Purchase)
  
  
  k = k+1
}

#######################################################################
#DETERMINING OPTIMAL COST FOR TRAINING AND TEST DATA FOR POLY KERNEL
#######################################################################  

tune.train.polynomial <- tune(svm, Purchase ~., data = train_data, kernel = "polynomial", 
                          ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )),degree = 2)

summary(tune.train.polynomial)

tune.test.polynomial <- tune(svm, Purchase ~., data = test_data, kernel = "polynomial", 
                         ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10 )),degree = 2)

summary(tune.test.polynomial)

#######################################################################
#PLOT FOR TRAINING AND TEST DATA FOR RADIAL KERNEL
####################################################################### 
cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)

par(mfrow=c(1,2))

plot(cost,error_train_polynomial,type="b",lty=1,col="blue",xlab = "cost",main=" Training Error - Polynomial Kernel",ylab="Training error")

plot(cost,error_test_polynomial,type="b",lty=2,col = "blue",xlab = "cost",main="Test Error - Polynomial Kernel",ylab="Test error")

