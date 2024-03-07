##### DATA LOADING #####
#import necessary libraries
library(readr)
library(dplyr)

#get working directory
getwd()

#Retrieve data
bank_data <- read.csv("bank-full.csv", sep=";")
head(bank_data)
str(bank_data)
shape(bank_data)

##### DATA PREPROCESSING #####
#Check for null values per column
null_values <- colSums(is.na(bank_data))
print(null_values)

#Convert categorical to numerical
one_hot_vars <- dummy_cols(bank_data, columns= "job", "marital", "contact", "poutcome")

#Remove Outliers

##### EXPLORATORY DATA ANALYSIS #####

#Generate Correlation Matrix of Original Features

#Calculate Feature Importance

#Remove Unimportant Attributes

#Check Correlation of Important Features

#Count instances of Relevant Features
#Ex:job_count <- table(bank_data$job)
#Ex:print(job_count)

##### MODELLING #####

##### TESTING #####

##### EVALUATION #####