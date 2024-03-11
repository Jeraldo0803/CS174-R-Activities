library(data.table)
library(caret)
library(rpart)
library(rpart.plot)

setwd("M1-SA/")
getwd()

parishousing.df <- read.csv("ParisHousingClass.csv")

# Check if data is null
null_count <- sapply(parishousing.df, function(x) sum(is.na(x)))
print(null_count)


