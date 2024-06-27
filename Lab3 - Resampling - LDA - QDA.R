install.packages("reshape2")
install.packages("MASS")
install.packages("pROC")
library(reshape2)
library(MASS)
library(pROC)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(dplyr)
setwd("C:/Users/Omar/Desktop/temp/Data Science")

diabetes <- read.csv('diabetes.csv')

names(diabetes)

head(diabetes$Outcome)

typeof(diabetes$Outcome)

#convert outcome 0 1 outcome to factor with categories as N and Y
diabetes$Outcome <- factor(diabetes$Outcome, labels = c("No","Yes"))
table(diabetes$Outcome) # summary of the outcome from the dataset

ggplot(data = diabetes)+
  geom_boxplot(mapping = aes(x = BMI, y = Outcome))

ggplot(data = diabetes)+
  geom_boxplot(mapping = aes(x = Pregnancies, y = Outcome))

diabetes %>% 
  ggplot()+
  geom_density(aes(Pregnancies,fill = Outcome, alpha = 10)) 

#START HERE


#mutate id as row number - so that i can sample based on id
diabetes <- diabetes%>%
  mutate(id = row_number())

#sample 70/30 using slice_sample function

training_data_set_1 = diabetes %>% slice_sample(prop = 0.7) #for each of these training sets 


#use anti_join to get test data(diabaetes - tr data points) the remaining 30% data

testing_data_set_1 = anti_join(diabetes,training_data_set_1, by = 'id') # you need to get the testing set

#need to remove id attribute because i only used it to select random samples
#SIMPLY REMOVES ID COLUMN 
training_data_set_1 = tr %>% dplyr ::select(-id)
testing_data_set_1 = te %>% dplyr ::select(-id)



#run LDA with Outcome as response and all variables


lda.mod <- lda(Outcome ~ Pregnancies,data = tr) #LDA containing all variables



summary(lda.mod)

lda.mod # gives me the coefficients

levels(te$Outcome)#shows me the order of my output, Level 1 or Level 2

as.integer(te$Outcome)


plot(lda.mod)


lda.mod.pred <- predict(lda.mod, newdata = te)

head(lda.mod.pred$x)
lda.mod.pred$x

names(lda.mod.pred)

conf <- table(lda.mod.pred$class,te$Outcome) #confusion matrix/comparing predicted vs actual classification of our test data

conf

#write funtion to compute performance measures from confusion matrix
measure = function(confmatrix){
  sensitivity = confmatrix[2,2] / (confmatrix[1,2] + confmatrix[2,2])
  specificity = confmatrix[1,1] / (confmatrix[1,1] + confmatrix[2,1])
  accuracy = sum(diag(confmatrix)) / sum(confmatrix)
  return (c(se = sensitivity, sp = specificity, acc = accuracy))
}

measure(conf)







# Assuming your data is in a data frame called 'your_data' with 'Outcome' as the response variable
install.packages("MASS")
library(leaps)
library(MASS)  # for the lda function

# Create a formula for the LDA model
lda_formula <- Outcome ~ .

# Generate all possible models with up to p predictors
all_models <- regsubsets(lda_formula, data = te, nvmax = ncol(te) - 1)

# Extract summary information about the models
summary_all_models <- summary(all_models)

# Find the best model based on some criterion (e.g., AIC, BIC)
best_model <- which.min(summary_all_models$cp)

# Get the details of the best model
best_model_details <- coef(all_models, id = best_model)

# Fit the best model using lda
best_lda_model <- lda(lda_formula, data = your_data[, best_model_details$which])

# Print the details of the best model
print(best_model_details)
# Print the summary of the best LDA model
summary(best_lda_model)



















#Running it again to check accuracy after another split of data / accuracy is sensitive to the split
#sample 70/30 using slice_sample function

tr = diabetes %>% slice_sample(prop = 0.7)

view (tr)

#use anti_join to get test data(diabaetes - tr data points) the remaining 30% data

te = anti_join(diabetes,tr, by = 'id')
view(te)

#need to remove id attribute because i only used it to select random samples

tr = tr %>% dplyr ::select(-id)
te = te %>% dplyr ::select(-id)
conf <- table(lda.mod.pred$class,te$Outcome)

lda.mod.pred <- predict(lda.mod, newdata = te)

head(lda.mod.pred$x)

names(lda.mod.pred)
measure(conf)

#QDA

qda.mod <- qda(Outcome ~ .,data = tr)
summary(qda.mod)
qda.mod
qda.mod.pred <- predict(qda.mod, newdata = te)

head(qda.mod.pred$x)

names(qda.mod.pred)

conf2 <- table(qda.mod.pred$class,te$Outcome)
conf2
measure(conf2)


library("pROC")
roc_lda = roc(response = te$Outcome,
              predictor = lda.mod.pred$posterior[,2])

roc_qda = roc(response = te$Outcome,
         predictor = qda.mod.pred$posterior[,2])

ggroc(list(lda = roc_lda,
      qda = roc_qda))

auc(roc_lda)
auc(roc_qda)
