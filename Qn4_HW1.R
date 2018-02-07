#############
#Qn4. Analysis of boston housing data in the MASS library
#############

#Installing Packages
#install.packages("MASS")
library(MASS)
#install.packages("ggplot2")
library(ggplot2)
?Boston
#Check the no. of dimensions and names of the Boston dataset
dim(Boston)
names(Boston)
##Pairwise scatter plots of the predictors 
pairs(Boston)


#ScatterPLot between other predictors and per capita income
pairs(Boston$crim~.,data = Boston,
      main = "Scatterplot Matrix b/w other predictors and crim")

#Finding relation between all the predictors of Boston data set
cor(Boston, Boston)

#Correlation
cor(Boston$crim, Boston)

##Making Plots for Tax rate, crime rate and Pupil-teacher ratio
X2 = Boston$crim
X1 = Boston$chas


##Making histogram
par(mfrow = c(1,4))
hist(Boston$crim ,main = "Crime rate Histogram", xlab = "Crime rate", ylab = "Frequency")

#Plot for tax rate
X1 = Boston$tax
##Making histogram
par(mfrow = c(1,4))
hist(Boston$tax ,main = "Tax rate Histogram", xlab = "Tax rate", ylab = "Frequency")

#Plot for ptratio
X1 = Boston$ptratio
##Making histogram
par(mfrow = c(1,4))
hist(Boston$ptratio ,main = "Pupil-Teacher ratio Histogram", xlab = "Pupil-Teacher ratrio", ylab = "Frequency")


#Calculating range of each predictor 
#Calculating range of Crime rate using Histogram
crim1 <- subset(Boston, Boston$crim >=0 & Boston$crim < 10 )
percentage = nrow(crim1)/nrow(Boston$chas)
print(percentage)

crim2 <- subset(Boston, Boston$crim >=10  & Boston$crim < 20 )
percentage = nrow(crim2)/nrow(Boston)
print(percentage)

crim3 <- subset(Boston, Boston$crim >=20  & Boston$crim < 30 )
percentage = nrow(crim3)/nrow(Boston)
print(percentage)

crim4 <- subset(Boston, Boston$crim >10 )
percentage = nrow(crim4)/nrow(Boston)
print(percentage)

crim5 <- subset(Boston, Boston$crim >20 )
percentage = nrow(crim5)/nrow(Boston)
print(percentage)

##Calculating how many suburbs are away from the river and near the river
Near_River <- length(Boston$chas[Boston$crim>10 & Boston$chas == 1]) 
Away_River <- length(Boston$chas[Boston$crim>10 & Boston$chas == 0])
print(Near_River)
print(Away_River)


#Calculating range of Tax rate using Histogram
tax1 <- subset(Boston, Boston$tax<500)
percentage = nrow(tax1)/nrow(Boston)
print(percentage)

tax2 <- subset(Boston, Boston$tax >=500 )
percentage = nrow(tax2)/nrow(Boston)
print(percentage)

##Calculating how many suburbs are away from the river and near the river
Near_River <- length(Boston$chas[Boston$tax<500 & Boston$chas == 1]) 
Away_River <- length(Boston$chas[Boston$tax<500 & Boston$chas == 0])
print(Near_River)
print(Away_River)

#Calculating range of Pupil-Teacher ratio using Histogram
ptratio1 <- subset(Boston, Boston$ptratio>=19)
percentage = nrow(ptratio1)/nrow(Boston)
print(percentage)

ptratio2 <- subset(Boston, Boston$ptratio <19)
percentage = nrow(ptratio2)/nrow(Boston)
print(percentage)

##Calculating how many suburbs are away from the river and near the river
Near_River <- length(Boston$chas[Boston$ptratio>=19 & Boston$chas == 1]) 
Away_River <- length(Boston$chas[Boston$ptratio<19 & Boston$chas == 0])
print(Near_River)
print(Away_River)

###Subsets with more than 7 rooms per dwelling
rooms_mt_7 <- subset(Boston , Boston$rm > 7)
print(nrow(rooms_mt_7))


rooms_mt_8 <- subset(Boston , Boston$rm > 8)
print(nrow(rooms_mt_8))

print(summary(rooms_mt_8))

##Making histogram
par(mfrow = c(1,4))
hist(Boston$rm ,main = "Rooms more than 8", xlab = "Rooms more than 8", ylab = "Frequency")


?Boston
