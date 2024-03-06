##### DATA LOADING #####
library(readr)
library(ggplot)

setwd("FA1.2\\")
bank_data<- read_delim("bank-full.csv", delim = ";")

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