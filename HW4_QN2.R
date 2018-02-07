# This is an optional lab concerning "tree"
rm(list= ls())
#install.packages("neuralnet")
library(neuralnet)
#install.packages("tree")
library("tree")
library("caret")
library(rpart)
library(MASS)

wine_data_set <- read.delim("/Users/priyamurthy/Downloads/wine.data.txt", sep = ",", header = TRUE)

set.seed(12345)
#test_indis <- sample(1:nrow(wine_data_set), 4/5*nrow(wine_data_set))
aux <- c(1:178)
train_indis <- sample(aux, 142, replace = FALSE)
test_indis <- setdiff(aux, train_indis)

test <- wine_data_set[test_indis,]
training <- wine_data_set[-test_indis,]


###################################################################
#Classifiction tree for whole dataset
###################################################################

model.control <- rpart.control(minsplit = 10, xval = 5, cp = 0)
fit.wine.data <- rpart(wine_data_set$Class~., data = wine_data_set, method = "class", control = model.control)

plot(fit.wine.data,branch = 0.5, uniform = T, compress = T,  main = "Full Tree: without pruning")
text(fit.wine.data, use.n = T, all = T, cex = .6)

#plot(fit.wine.data, uniform = T, compress = T)
#text(fit.wine.data, cex = 1)


#plot(fit.wine.data, uniform = T, compress = T)
#text(fit.wine.data, use.n = T, all = T, cex = 1)


#plot(fit.wine.data, branch = .4, uniform = T, compress = T)
#text(fit.wine.data, use.n = T, all = T, cex = 1)

###################################################################
#Classifiction tree for train dataset
###################################################################

model.control <- rpart.control(minsplit = 10, xval = 5, cp = 0)
fit.wine.data.train <- rpart(training$Class~., data = training, method = "class", control = model.control)

plot(fit.wine.data.train,branch = 0.5, uniform = T, compress = T,  main = "Full Tree of test dataset: without pruning")
text(fit.wine.data.train, use.n = T, all = T, cex = .6)

plot(fit.wine.data.train$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(fit.wine.data.train$cptable[,4])
pruned_fit_wine <- prune(fit.wine.data.train, cp = fit.wine.data.train$cptable[min_cp,1])


## plot the full tree and the pruned tree

plot(pruned_fit_wine, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_wine, cex = .5)


plot(fit.wine.data, branch = .3, compress=T, main = "Full Tree")
text(fit.wine.data, cex = .5)


#############################################################
#Using prediction to see how the tree categorizes a dataset
#############################################################

table(predict(fit.wine.data.train, training, type = "class"),training[,"Class"])
fit.wine.data$where
fit.wine.data$frame
trainingnodes <- rownames(fit.wine.data.train$frame)[fit.wine.data.train$where]
table(rownames(fit.wine.data.train$frame)[fit.wine.data.train$where])

#############################################################
#Prediction for test data
#############################################################
testresults <- predict(fit.wine.data.train, test, type = "matrix")
testresults <- data.frame(testresults)
names(testresults) <- c("ClassGuess","NofClass1onNode", "NofClass2onNode",
                        "NofClass3onNode", "PClass1", "PClass2", "PClass2")


nodes_wine <- fit.wine.data.train 

nodes_wine$frame$yval = as.numeric(rownames(nodes_wine$frame))
testnodes <- predict(nodes_wine, test, type="vector")
table(predict(nodes_wine, test, type="vector"))


