setwd("C:/Users/student/Desktop")
library(tidyverse)
library(ggplot2)
df <- read.csv("Wage.csv", header = T)
View(df)

#replace
colnames(df)[1] <- "ID"

#VISUALIZATION using ggplot2
#general form
#ggplot(datae = <DATA>) + 

ggplot(data = df) +
  geom_point(mapping = aes(x = age, y = wage))

ggplot(data = df) +
  geom_point(mapping = aes(x = age, y = wage, color = education))

ggplot(data = df) +
  geom_point(mapping = aes(x = age, y = wage, size = education, 
                           color = education))

ggplot(data = df) +
  geom_point(mapping = aes(x = age, y = wage, alpha = education))

ggplot(data = df) +
  geom_point(mapping = aes(x = age, y = wage, shape = education))

ggplot(data = df) +
  geom_point(mapping = aes(x = age, y = wage))+
  facet_wrap(~ education, nrow = 3)

ggplot(data = df) +
  geom_point(mapping = aes(x = age, y = wage))+
  geom_smooth(mapping = aes(x = age, y = wage))

ggplot(data = df) +
  geom_bar(mapping = aes(x = education,  fill = education))

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = education, y = wage))

#changed data set
df <- read.csv("HousePrices.csv", header = T)
view(df)
glimpse(df) #getting a gimpse of the data rather than the whole set 
typeof(df$grade)

#grade vs price
df$grade <- as.factor(df$grade)
typeof(df$grade)

ggplot(data = df) +
  geom_boxplot(mapping = aes(x = grade, y = price, fill = grade))

#correlation test of sqft vs price
cor.test(df$sqft_living,df$price)

#pairs
pairs(~sqft_living + sqft_lot + sqft_basement,df)

slr_model <- lm(price ~ sqft_living, data = df)
summary(slr_model)

plot(df$sqft_living, df$price, xlab = "sqft_living", ylab = "price", col = "blue", pch = 20)
abline(slr_model, col = "pink", lwd = 3, lty = 1)

mlr_model <- lm(price ~ sqft_living + sqft_lot + waterfront, data = df)
summary(mlr_model)