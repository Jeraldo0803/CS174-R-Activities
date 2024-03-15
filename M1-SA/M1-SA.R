library(data.table)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)
library(ggcorrplot)

setwd("M1-SA/")
getwd()

parishousing.df <- read.csv("ParisHousingClass.csv")

# Check if data is null
null_count <- sapply(parishousing.df, function(x) sum(is.na(x)))
print(null_count)

#Change Category "Basic" to 0, "Luxury" to 1
parishousing.df$category <- ifelse(parishousing.df$category == "Basic", 0, 1)

cor(parishousing.df)
ggcorrplot(cor(parishousing.df))

# Revert Dependent Variable to String for Display
parishousing.df$category <- ifelse(parishousing.df$category == 0, "Basic", "Luxury")

##### MODELLING #####
#Train/Test Split
set.seed(1024)

n_rows <- nrow(parishousing.df)

# Number of rows for the training set (e.g., 70%)
train_size <- floor(0.7 * n_rows)

# Randomly select indices for the training set
train_indices <- sample(1:n_rows, train_size, replace = FALSE)

# Create the training set
X_train <- parishousing.df[train_indices, -which(names(parishousing.df) == "category")]
Y_train <- parishousing.df[train_indices, "category"]

# Create the test set (using the remaining indices)
test_indices <- setdiff(1:n_rows, train_indices)
X_test <- parishousing.df[test_indices, -which(names(parishousing.df) == "category")]
Y_test <- parishousing.df[test_indices, "category"]

# Model Fitting

model_fit <- rpart(Y_train ~., 
                           data= X_train,
                   control=rpart.control(cp=0.001)
                           )
par(xpd = NA) # otherwise on some devices the text is clipped

rpart.plot(model_fit,
           type = 4,
           extra = 1, 
           )

#text(model_fit, digits = 2)

printcp(model_fit)

plotcp(model_fit)

model_summary <- summary(model_fit)

#use fitted model to predict if Basic or Luxury
pred <- predict(model_fit, newdata=X_test, type="class")

# Create a mapping to define numeric values for factor levels
mapping <- c("Basic" = 0, "Luxury" = 1)

# Convert factor levels to numeric using the replace function
pred_numeric <- factor(pred, labels = mapping)
Y_test_numeric <- factor(Y_test, labels = mapping)

class(pred_numeric)
class(Y_test_numeric)

confusionMatrix(pred_numeric, Y_test_numeric)

