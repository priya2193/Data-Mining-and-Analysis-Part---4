rm(list = ls())
iris<- read.csv("/Users/priyamurthy/Downloads/iris (1).txt",header=FALSE)

#removing the first variable
train_data<-iris[1:75, ]
test_data<-iris[76:150,]
train_data_target<-train_data[,5]
test_data_target<-test_data[,5]
dim(train_data)

# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)
install.packages("caretEnsemble")
install.packages("caret")
#install.packages("ipred")
library(ipred)
install.packages("C50")
library(C50)

####################################################
#BOOSTING ON IRIS DATASET
####################################################

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
fit.c50 <- train(iris$V5~., data=iris, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(iris$V5~., data=iris, method="gbm",  trControl=control, verbose=FALSE)
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)



####################################################
#BAGGING ON IRIS DATASET
####################################################

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
fit.treebag <- train(V5~., data=iris, method="treebag", metric=metric, trControl=control)
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)

#Accuracy Bagged CART - 91.67 and random forest - 94

####################################################
#RANDOM FOREST ON IRIS DATASET
####################################################
library(randomForest)
set.seed(seed)
fit.rf <- train(V5~., data=iris, method="rf", metric=metric, trControl=control)
output.forest <- randomForest(V5 ~., data = iris)
print(output.forest)


rm(list = ls())
data(iris)
iris_data_set <- read.delim("/Users/priyamurthy/Downloads/iris.txt", sep = "\t", header = TRUE)

train <- sample(1:(nrow(iris_data_set)/2))
iris_train <- iris_data_set[train,]
iris_test <- iris_data_set[-train,]

y_true_train <- iris_train$Species
y_true_test <- iris_test$Species
#########################################
# Logistic Regression
#########################################
glm.fit <- glm( iris_train$Species ~. , data = iris_train, family = binomial, maxit = 100)
summary(glm.fit)



# Predict
glm.probs.train <- predict(glm.fit, newdata = iris_train, type = "response")
y_hat_train <- round(glm.probs.train)
table(y_hat_train,y_true_train)
glm.probs.test <- predict(glm.fit, newdata = iris_test, type = "response")
y_hat_test <- round(glm.probs.test)
table(y_hat_test,y_true_test)




#########################################
#  Calculate the error rates
########################################
train_err <- mean(abs(y_hat_train- y_true_train))/length(y_true_train)
test_err <- mean(abs(y_hat_test- y_true_test))/length(y_true_test)

train_err
test_err
accuracy_train <- (1 - train_err)*100
accuracy_test <- (1 - test_err)*100


#########################################
# KNN
#########################################
library("ggplot2")
library("ElemStatLearn")
#install.packages("class")
library("class")
#errr_test<- c(0,0,0,0,0,0,0,0)
#errr_train <- c(0,0,0,0,0,0,0,0)
errr_train <- 0
errr_test <- 0
for (i in c(1,3,5,7,9,11, 13,15)){
  #quartz()
  test_predict <- knn(iris_train,iris_test,y_true_test,i)
  train_predict <- knn(iris_train,iris_train,y_true_train, i)
  
  #converting to data frame
  test_predict_df <- as.data.frame(test_predict)
  train_predict_df <- as.data.frame(train_predict)
  errr_test <- c(errr_test,0)
  errr_train <- c(errr_train,0)
  errr_test[i] <- mean(y_true_test!=test_predict_df)/nrow(iris_test)
  errr_train[i] <- mean(y_true_train!=train_predict_df)/nrow(iris_train)
  
}
errr_test
errr_train

accuracy_knn_train <- c(0,0,0,0,0,0,0,0)
accuracy_knn_test <- c(0,0,0,0,0,0,0,0)
for(i in c(1,3,5,7,9,11,13,15,100))
{
  accuracy_knn_train[i] <- (1 - errr_train[i])*100
  accuracy_knn_test[i] <- (1 - errr_test[i])*100 
}





