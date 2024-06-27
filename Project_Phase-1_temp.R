setwd("C:/Users/Omar/Desktop")
library(ggplot2)
library(tidyverse)
df <- read.csv("2023_PL_standings.csv", header = T)

View(df)

ggplot(data = df) +
  geom_point(mapping = aes(x = home_goals_for, y = rank,color = team.name))+
  geom_smooth(mapping = aes(x = home_goals_for, y = rank))

df$rank <- as.factor(df$rank)
typeof(df$rank)

cor.test(df$home_goals_for,df$away_goals_for)

slr_model <- lm(rank ~ home_goals_for,date = df)
summary(slr_model)
