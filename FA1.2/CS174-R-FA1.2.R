##### DATA LOADING #####
#import necessary libraries
library(readr)
#get working directory
getwd()
setwd('CS174-R-Activities\\FA1.2')

#Comment

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
#Generate Correlation Matrix of All features
corr_matrix <- cor(converted_bank_data)

#Visualize the correlation matrix as a heatmap
heatmap(corr_matrix,
        symm = TRUE,         # Ensure the heatmap is symmetric
        margins = c(10,10),  # Adjust margins for labels
        main = "Correlation Matrix",  # Title of the plot
        col = colorRampPalette(c("blue", "white", "red"))(100))  # Color scheme

#Check for Outliers
#Age
boxplot(converted_bank_data$age, main = "Boxplot of Age Data", ylab = "Age", col = "yellow")

#Balance
boxplot(converted_bank_data$balance, main = "Boxplot of Balance Data", ylab = "Balance", col = "yellow")

#Duration
boxplot(converted_bank_data$duration, main = "Boxplot of Duration Data", ylab = "Duration", col = "yellow")

#Data Distribution
str(converted_bank_data)
cols_to_plot <- c("age","education_num", "balance", "duration", "campaign","housing_num", "loan_num", "month_num")
par(mfrow = c(2, 4))


for (variable in cols_to_plot) {
  hist(converted_bank_data[[variable]], main = paste(variable, "Distribution"),
       xlab = variable, ylab = "Frequency")
}

par(mfrow = c(1, 1))

#Display Summarized Statistics of each Feature
summary(converted_bank_data)

##### MODELLING #####
#model <- lm(output_variable ~ independent_variable1 + independent_variable2, data=bank_data)
#model <- lm(bank_data$y ~ )

##### TESTING #####


##### EVALUATION #####


