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

# Correlation Matrix

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
# rpart(formula, data=, method=, control=)

model_fit <- rpart(Y_train ~., 
                           data= X_train,
                   control=rpart.control(cp=0.001)
                           )
par(xpd = NA) # otherwise on some devices the text is clipped

rpart.plot(model_fit,
           type = 4, # 
           extra = 1, # this argument shows all observations
           cp = 0.1
           )

text(model_fit, digits = 2)

printcp(model_fit)

plotcp(model_fit)

summary(model_fit)

best <- model_fit$cptable[which.min(model_fit$cptable[,"xerror"]),"CP"]

pruned_tree <- prune(model_fit, cp=best)

prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

rpart.plot(pruned_tree, type=4, extra=1)

#use pruned tree to predict salary of this player
predict(pruned_tree, newdata=X_test)


