#installed.packages("kernlab")
#install.packages("randomForest")
rm(list = ls())
library(kernlab)
library(randomForest)
set.seed(12345)
data(spam)
spam_train=spam[1:2300,]
spam_test=spam[2301:4600,]
prediction <- c(0,0,0,0,0,0,0,0,0,0)
output.forest <- c(0,0,0,0,0,0,0,0,0,0)
#oob_error <-c(0,0,0,0,0,0,0,0,0,0)
#test_error <-c(0,0,0,0,0,0,0,0,0,0)
k <- 1
for(i in c(1,2,5,9,10,50,100))
{
  output.forest <- randomForest(type ~., data = spam_train, mtry=k, ntree=400, importance=TRUE, do.trace=100)
  
  plot(output.forest)
  prediction<-predict(output.forest,data = spam_test) 
  
  plot(output.forest)
  #test.err[i]= with(spam_test, mean( (type - prediction[i])^2)) 
  test.err= mean( (prediction!= spam_test$type)^2)
  print(test.err)

  k = k + 1
}

