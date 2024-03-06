##### DATA LOADING #####
library(readr)

#Set working directory
getwd()

#Retrieve data
bank_data<- read.csv("bank-full.csv", sep=";") 
head(bank_data)
str(bank_data)

#bank_data$y <- ifelse(bank_data$y == "no", 0, 1)

#model <- lm(bank_data$y ~ bank_data$balance + bank_data$duration, data=bank_data)

#summary(model)

#plot(model)

##### DATA PREPROCESSING #####
#Check for null values

#Convert categorical to numerical

#Remove Outliers

##### EXPLORATORY DATA ANALYSIS #####
#Generate Correlation Matrix of Original Features

#Calculate Feature Importance

#Remove Unimportant Attributes

#Check Correlation of Important Features

##### MODELLING #####

##### TESTING #####

##### EVALUATION #####