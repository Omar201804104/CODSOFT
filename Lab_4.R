library(randomForest)
install.packages("gbm")
library(MASS)
library(tidyverse)
library(gbm)

glimpse(Boston)
#splitting the data into 80% training 20% testing

set.seed(1,sample.kind = "Rejection")
ind = sample(1:nrow(Boston), 0.8*nrow(Boston), replace = F)
btrain = Boston[ind,]
btest = Boston[-ind,]

lrm <- lm(medv ~ ., data = btrain)
lrm_pred <- predict(lrm, newdata = btest)

summary(lrm_pred)

MSE_lrm <- mean ((btest$medv - lrm_pred)^2)

?randomForest #for documentation

#using bagging 
bag_boston = randomForest(formula = medv~., data = btrain, mtry = (ncol(btrain)-1),importance = T, ntree=1000)
pred_bag = predict(bag_boston, newdata = btest)
MSE_bag = mean((btest$medv-pred_bag)^2)
#########################
#Using random forest 
rf_boston <- randomForest(formula = medv~., data = btrain, mtry = sqrt(ncol(btrain)-1),importance = T, ntree=1000)
rf_pred = predict(bag_boston, newdata = btest)
MSE_bag = mean((btest$medv-rf_pred)^2)

##Importance plot
round(importance(rf_boston),2)
varImpPlot(rf_boston)


#SVM using simulated data

install.packages("e1071")
library(e1071)
library(pROC)
#Generating simulated data
set.seed(1)
x = matrix(rnorm(200*2),ncol=2)
x[1:100,] = x[1:100,]+2
x[101:150,] = x[101:150,]-2
y = c(rep(1,150),rep(2,50))
simdata = data.frame(x=x, y=as.factor(y))
view(simdata)

#Viewing the data
ggplot(data = simdata)+
  geom_point(mapping = aes(x = x.1, y = x.2,color = y))

#######Viewing the data we noticed that a good seperation would be radial (kernel is radial)

table(simdata$y)
simdata$y = factor(simdata$y, labels = c(-1,1))
table(simdata$y)

#split the simulated data

set.seed(4,sample.kind = "Rejection")
index = sample(1:nrow(simdata), 0.7*nrow(simdata), replace = F)
simdata_tr = simdata[index,]
simdata_te = simdata[-index,]

?svm

SVM = svm(y~. , data = simdata_tr, kernel = "radial",cost = 1,gamma = 1)
SVM
#plotting the seperation between classe
#######################################  the "x" signs are the support vectors there is 44 of them/
plot(SVM, simdata_tr, grid = 200)
#predicting on test data
SVM_pred = predict(SVM, newdata = simdata_te)
#confusion matrix
table(SVM_pred, simdata_te$y)
#Accuracy
mean(SVM_pred == simdata_te$y)

#Changing Gamma
SVM2 = svm(y~. , data = simdata_tr, kernel = "radial",cost = 0.01,gamma = 0.2)
SVM2_pred = predict(SVM2, newdata = simdata_te)
table(SVM2_pred, simdata_te$y)
mean(SVM2_pred == simdata_te$y)
