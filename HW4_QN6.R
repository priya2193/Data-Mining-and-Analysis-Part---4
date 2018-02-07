rm(list = ls()) # Clearing the background data

#install.packages("ElemStatLearn")
#install.packages("neuralnet")
#install.packages("gam")
train_data_set <- read.delim("/Users/priyamurthy/Downloads/train_data.txt", sep = ",", header = TRUE)
test_data_set <- read.delim("/Users/priyamurthy/Downloads/test_data.txt", sep = ",", header = TRUE)
data_heart <- read.delim("/Users/priyamurthy/Downloads/data_6.txt", sep = ",", header = TRUE)

library(gam)
library(neuralnet)
library(ElemStatLearn)

##########################################################
#Creating dataset
##########################################################


test.error <- NULL
train.error <- NULL


##########################################################
#Scaling for Neural Network
##########################################################
maxs <- apply(data_heart, 2, max) 
mins <- apply(data_heart, 2, min)
scaled <- as.data.frame(scale(data_heart, center = mins, scale = maxs - mins))

name <- names(scaled)
formula <- as.formula(paste("X1 ~", paste(name[!name %in% "X1"], collapse = " + ")))

set.seed(1234)

##########################################################
#Cross validation and test
##########################################################
for(i in 1:15)
{
  
  nn.model <- neuralnet(formula,data=scaled,hidden=c(i),threshold=2,linear.output=T)
  train.error[i] <- sum(((as.data.frame(nn.model$net.result)*(40-5)+5) - (scaled$X1*(40-5)+5))^2)/nrow(scaled)
  
  test.error[i] <- cv_data(data_heart,i)
  
}

cv_data <- function(data,n)
{
  hidden_l=c(n)
  
  maxs <- apply(data, 2, max) 
  mins <- apply(data, 2, min)
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  
  cv.error <- NULL
  
  k <- 1
  
  ##################
  #Cross validation
  ################## 
  
  for(i in 1:k)
  { 
    # Train-test split
    index <- sample(1:nrow(data),round(0.90*nrow(data)))
    cv_train <- scaled[index,]
    cv_test <- scaled[-index,]
    #Neural net
    neural_net <- neuralnet(formula,data=cv_train,hidden=hidden_l,threshold=2,linear.output=T)
    
    #Prediction and Scaling
    predict.nn <- compute(neural_net,cv_test[,2:45])
    predict.nn <- predict.nn$net.result*(max(data$X1)-min(data$X1))+min(data$X1)
    
    
    test.cv.r <- (cv_test$X1)*(max(data$X1)-min(data$X1))+min(data$X1)
    
    # Calculating MSE test error
    cv.error[i] <- sum((test.cv.r - predict.nn)^2)/nrow(cv_test)
  }
  
  # Return average MSE
  return(mean(cv.error))
}

# Plot train error
plot(train.error,main='Mean Squared Error vs Hidden neurons',xlab="Hidden neurons",ylab='Train error MSE',type='b',col='blue',lwd=2)
# Plot test error
plot(test.error,main='Mean Square Error vs Hidden neurons',xlab="Hidden neurons",ylab='Test error MSE',type='b',col='green',lwd=2)

# Number of neurons (index) that minimizes test/train error
min_test<-which(min(test.error) == test.error)
min_train<-which(min(train.error) == train.error)

min(test.error)
min(train.error)
library(NeuralNetTools)
garson(nn.model)
nn.model<-neuralnet(formula,data=scaled,hidden=min_test,threshold=2,linear.output=T)

