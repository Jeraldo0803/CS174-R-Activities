##### DATA LOADING #####
#import necessary libraries
library(readr)
library(caTools)
library(corrplot)
library(ggplot2)
library(broom)
library(gridExtra)

#get working directory
getwd()
setwd("FA1.2/")

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
encoded_bank_data$month_num <- as.numeric(factor(encoded_bank_data$month))
encoded_bank_data$y_num <- as.numeric(factor(encoded_bank_data$y))
converted_bank_data <- encoded_bank_data[, !(names(encoded_bank_data) %in% c("education", "default", "housing", "loan", "month", "y"))]
converted_bank_data$y_num <- ifelse(encoded_bank_data$y == "no", 0, 1)
converted_bank_data$housing_num <- ifelse(encoded_bank_data$housing == "no", 0, 1)
converted_bank_data$loan_num <- ifelse(encoded_bank_data$loan == "no", 0, 1)

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

#####EXPERIMENTING#####

##### MODELLING #####

#Train/Test Split

set.seed(1024)

n_rows <- nrow(converted_bank_data)

# Number of rows for the training set (e.g., 70%)
train_size <- floor(0.7 * n_rows)

# Randomly select indices for the training set
train_indices <- sample(1:n_rows, train_size, replace = FALSE)

# Create the training set
X_train <- converted_bank_data[train_indices, -which(names(converted_bank_data) == "y_num")]
Y_train <- converted_bank_data[train_indices, "y_num"]

# Create the test set (using the remaining indices)
test_indices <- setdiff(1:n_rows, train_indices)
X_test <- converted_bank_data[test_indices, -which(names(converted_bank_data) == "y_num")]
Y_test <- converted_bank_data[test_indices, "y_num"]



#####Model Fitting#####

#####################
# Fit logistic regression model including only duration
logistic_model_duration <- glm(Y_train ~ duration, data = X_train, family = "binomial")

# Summarize the model
summary(logistic_model_duration)

# Set plot parameter to display 2 plots side-by-side
par(mfrow = c(1,2))

# Predict probabilities using the logistic regression model on training data
train_predictions_duration <- predict(logistic_model_duration, type = "response", newdata = X_train)

# Create a scatter plot of duration vs. predicted probabilities using training data
plot(X_train$duration, train_predictions_duration, 
     xlab = "Duration", ylab = "Predicted Probability of y_num", 
     main = "Logistic Regression: Duration vs. Predicted Probability (Training Data)")

# Add a line representing the logistic regression model
lines(X_train$duration, train_predictions_duration, col = "blue")

# Create a sequence of duration values for prediction
duration_seq <- seq(min(X_train$duration), max(X_train$duration), length.out = 100)

# Predict probabilities using the logistic regression model on the duration sequence
predicted_probabilities_duration <- predict(logistic_model_duration, newdata = data.frame(duration = duration_seq), type = "response")

# Plot the logistic function for duration
plot(X_train$duration, Y_train, pch = 19, xlab = "Duration", ylab = "Probability of y_num",
     main = "Logistic Regression: Duration vs. Probability of y_num", ylim = c(0, 1))
lines(duration_seq, predicted_probabilities_duration, col = "blue")






############
# Fit logistic regression model including only balance
logistic_model_balance <- glm(Y_train ~ balance, data = X_train, family = "binomial")

# Summarize the model
summary(logistic_model_balance)

# Predict probabilities using the logistic regression model on training data
train_predictions_balance <- predict(logistic_model_balance, type = "response", newdata = X_train)

# Create a scatter plot of balance vs. predicted probabilities using training data
plot(X_train$balance, train_predictions_balance, 
     xlab = "Balance", ylab = "Predicted Probability of y_num", 
     main = "Logistic Regression: Balance vs. Predicted Probability (Training Data)")

# Add a line representing the logistic regression model
lines(X_train$balance, train_predictions_balance, col = "blue")

# Create a sequence of balance values for prediction
balance_seq <- seq(min(X_train$balance), max(X_train$balance), length.out = 100)

# Predict probabilities using the logistic regression model on the balance sequence
predicted_probabilities_balance <- predict(logistic_model_balance, newdata = data.frame(balance = balance_seq), type = "response")

# Plot the logistic function for balance
plot(X_train$balance, Y_train, pch = 19, xlab = "Balance", ylab = "Probability of y_num",
     main = "Logistic Regression: Balance vs. Probability of y_num", ylim = c(0, 1))
lines(balance_seq, predicted_probabilities_balance, col = "blue")


##### BOTH BALANCE AND DURATION #####

# Fit logistic regression model including both duration and balance
logistic_model_both <- glm(Y_train ~ duration + balance, data = X_train, family = "binomial")

# Summarize the model
summary(logistic_model_both)

# Predict probabilities using the logistic regression model on training data
train_predictions_both <- predict(logistic_model_both, type = "response", newdata = X_train)

par(mfrow = c(1,2))

# Plot the logistic function for duration
plot(X_train$duration, Y_train, pch = 19, xlab = "Duration", ylab = "Probability of y_num",
     main = "Duration vs. Probability of y_num", ylim = c(0, 1))
lines(duration_seq, predicted_probabilities_duration, col = "blue")

# Plot the logistic function for balance
plot(X_train$balance, Y_train, pch = 19, xlab = "Balance", ylab = "Probability of y_num",
     main = "Balance vs. Probability of y_num", ylim = c(0, 1))
lines(balance_seq, predicted_probabilities_balance, col = "blue")

# Create a new data frame for plotting
interaction_data <- expand.grid(duration = seq(min(X_train$duration), max(X_train$duration), length.out = 100), 
                                balance = c(min(X_train$balance), max(X_train$balance)))

# Predict probabilities for each combination of duration and balance
interaction_data$pred_low <- predict(logistic_model_both, newdata = interaction_data, type = "response")

# Repeat prediction with a different intercept (assuming balance affects intercept)
interaction_data$balance <- interaction_data$balance + 1  # Adjust for intercept shift (modify as needed)
interaction_data$pred_high <- predict(logistic_model_both, newdata = interaction_data, type = "response")

# Reset balance to original values
interaction_data$balance <- interaction_data$balance - 1

# Use the predicted probabilities directly in ggplot
ggplot(interaction_data, aes(x = duration, y = pred_low, linetype = "Low Balance")) +
  geom_line() +
  geom_line(aes(x = duration, y = pred_high, linetype = "High Balance")) +
  labs(x = "Duration", y = "Predicted Probability", linetype = "Balance") +
  ggtitle("Interaction Plot for Logistic Regression Model") +
  theme_bw()  # Optional for a cleaner look

##### EVALUATION #####


