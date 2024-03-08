##### DATA LOADING #####
#import necessary libraries
library(readr)
library(caTools)
library(corrplot)
#get working directory
getwd()

#Retrieve data
bank_data <- read.csv("bank-full.csv", sep=";")
head(bank_data)
str(bank_data)
dim(bank_data)

##### DATA PREPROCESSING #####
#Check for null values per column
null_values <- colSums(is.na(bank_data))
print(null_values)

#Convert categorical to numerical
#One-hot encoding
cols_to_convert <- c("job", "marital", "contact", "poutcome")
one_hot_data <- model.matrix(~ . + 0, data= bank_data[cols_to_convert])
print(one_hot_data)
bank_data2 <- bank_data[, !(names(bank_data) %in% c("job", "marital", "contact", "poutcome"))]
encoded_bank_data <- cbind(bank_data2, one_hot_data)

#Label Encoding
encoded_bank_data$education_num <- as.numeric(factor(encoded_bank_data$education))
encoded_bank_data$default_num <- as.numeric(factor(encoded_bank_data$default))
encoded_bank_data$housing_num <- as.numeric(factor(encoded_bank_data$housing))
encoded_bank_data$loan_num <- as.numeric(factor(encoded_bank_data$loan))
encoded_bank_data$month_num <- as.numeric(factor(encoded_bank_data$month))
encoded_bank_data$y_num <- as.numeric(factor(encoded_bank_data$y))
converted_bank_data <- encoded_bank_data[, !(names(encoded_bank_data) %in% c("education", "default", "housing", "loan", "month", "y"))]

#Final Converted Data
str(converted_bank_data)
head(converted_bank_data)

##### EXPLORATORY DATA ANALYSIS #####
#Generate Correlation Matrix
corr_age <- cor(converted_bank_data[, c('age','y_num')])
corr_balance <- cor(converted_bank_data[, c('balance','y_num')])
corr_day <- cor(converted_bank_data[, c('day','y_num')])
corr_duration <- cor(converted_bank_data[, c('duration','y_num')])
corr_camp <- cor(converted_bank_data[, c('campaign','y_num')])
corr_pdays <- cor(converted_bank_data[, c('pdays','y_num')])
corr_previous <- cor(converted_bank_data[, c('previous','y_num')])
corr_edu <- cor(converted_bank_data[, c('education_num','y_num')])
corr_def <- cor(converted_bank_data[, c('default_num','y_num')])
corr_house <- cor(converted_bank_data[, c('housing_num','y_num')])
corr_loan <- cor(converted_bank_data[, c('loan_num','y_num')])
corr_month <- cor(converted_bank_data[, c('month_num','y_num')])

#Create list of correlation matrix and titles
corr_list <- list(corr_age, corr_balance, corr_day, corr_duration, corr_camp, corr_pdays
                  , corr_previous, corr_edu, corr_def, corr_house, corr_loan, corr_month)
title_list <- list("Age V.S. Y", "Balance V.S. Y", "Day V.S. Y", "Duration V.S. Y", "Campaign V.S. Y",
                   "Pdays V.S. Y", "Previous V.S. Y", "Education V.S. Y", "Default V.S. Y", 
                   "Housing V.S. Y", "Loan V.S. Y", "Month V.S Y")

#Set the display dimension
par(mfrow = c(3, 4))

j = 1

#Visualize the correlation matrix as a heatmap
for (i in corr_list) {
  corrplot(
    i, 
    method = "color", 
    col = colorRampPalette(c("blue", "white", "red"))(20),
    addCoef.col = "black", 
    number.cex = 1, 
    tl.cex = 1, title = title_list[j]
  )
  j = j + 1
}

#Data Distribution
str(converted_bank_data)
cols_to_plot <- c("age", "balance", "day", "duration", "campaign", "pdays",
                  "previous", "education_num", "default_num", "housing_num", 
                  "loan_num", "month_num")

# Set the display dimension
par(mfrow = c(3, 4))

for (variable in cols_to_plot) {
  hist(converted_bank_data[[variable]], main = paste(variable, "Distribution"),
       xlab = variable, ylab = "Frequency")
}

#Display Summarized Statistics of each Feature
summary(converted_bank_data)

##### MODELLING #####

#Train/Test Split

set.seed(1024)
train <- sample.split(Y = converted_bank_data$y, SplitRatio = 0.7)
trainset <- subset(converted_bank_data, train == T)
testset <- subset(converted_bank_data, train == F)

prop.table(table(trainset$y))

prop.table(table(testset$y))

#model <- lm(output_variable ~ independent_variable1 + independent_variable2, data=bank_data)
model <- lm(trainset$y ~ trainset$duration + trainset$pdays, data=trainset)

summary(model)
# Extract independent variables from the test set
test_input <- testset[, c("duration", "pdays")]

# Make predictions using the model
predictions <- predict(model, newdata = test_input)

##### TESTING #####
#predictions <- predict(model, newdata=testset)

##### EVALUATION #####
# Example: Calculate evaluation metrics for a regression model
# Assuming 'predictions' contains the predicted numerical values and 'actual_values' contains the true numerical values

cat("Length of predictions:", length(predictions), "\n")
cat("Length of testset$y:", length(testset$y), "\n")


# Mean Squared Error (MSE)
mse <- mean((predictions - testset$y)^2)
cat("Mean Squared Error (MSE):", mse, "\n")

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Mean Absolute Error (MAE)
mae <- mean(abs(predictions - testset$y))
cat("Mean Absolute Error (MAE):", mae, "\n")

# R-squared
rsquared <- 1 - sum((testset$y - predictions)^2) / sum((testset$y - mean(testset$y))^2)
cat("R-squared:", rsquared, "\n")


