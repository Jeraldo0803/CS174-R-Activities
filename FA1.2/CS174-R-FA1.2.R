##### DATA LOADING #####
library(readr)
library(ggplot)

#Set working directory
setwd("FA1.2\\")

#Retrieve data
bank_data<- read.csv("bank-full.csv", sep=";") 
head(bank_data)
str(bank_data)

bank_data$y <- ifelse(bank_data$y == "no", 0, 1)

model <- lm(bank_data$y ~ bank_data$balance + bank_data$duration, data=bank_data)

summary(model)

plot(model)

##### DATA PREPROCESSING #####

##### EXPLORATORY DATA ANALYSIS #####

##### MODELLING #####

##### TESTING #####

##### EVALUATION #####