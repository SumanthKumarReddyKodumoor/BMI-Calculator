#1.1
library(dplyr)
#setting up the directory first
getwd()
setwd("C:/Users/kodmo/Downloads")

data <- read.csv2("DATASET FOR NEW BODY FAT MEASUREMENT.csv", header = TRUE)

head(data)
print(data)

#1.2
# Select the required variables
Required_variables <- data[c("BMI","Age", "Height.cm", "W.Kg", "FatMass.Kg", "FM", "FFM.Kg", "BMR.Kcal")]
Required_variables

#1.3
#selecting the random samples of 45 participants as test data
set.seed(53)
test_data <- Required_variables[sample(nrow(Required_variables), 45), ]

library(dplyr)
library(glmnet)
set.seed(53)
# Select the remaining data apart from test_data
train_data <- anti_join(Required_variables, test_data)
train_data

dim(train_data)
dim(test_data)

#2.1
#LASSO Regression

library(glmnet)

# Set up the predictor variables and response variable
x_train <- as.matrix(train_data[, c("Age", "Height.cm", "W.Kg", "FatMass.Kg", "FM", "FFM.Kg", "BMR.Kcal")])
y_train <- train_data$BMI

# Set up the grid of tuning parameters to search over
alpha_vals <- 1  # LASSO penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
cv_fit <- cv.glmnet(x_train, y_train, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10, standardize = FALSE)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
lasso_fit <- glmnet(x_train, y_train, alpha = alpha_vals, lambda = best_lambda, standardize = FALSE)
best_model_coef <- coef(lasso_fit, s = best_lambda)




#Ridge regression

#2.2


# Set up the grid of tuning parameters to search over
alpha_vals <- 0  # Ridge penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
cv_fit <- cv.glmnet(x_train, y_train, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10, standardize = FALSE)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
ridge_fit <- glmnet(x_train, y_train, alpha = alpha_vals, lambda = best_lambda, standardize = FALSE)
best_model_coef <- coef(ridge_fit, s = best_lambda)




#2.3

x <- Required_variables[, c("Age", "Height.cm", "W.Kg", "FatMass.Kg", "FM", "FFM.Kg", "BMR.Kcal")]
y <- Required_variables$BMI

# Create interaction terms
x_interact <- model.matrix(~ Age + Height.cm + W.Kg + FatMass.Kg + FM + FFM.Kg + BMR.Kcal + Age:Height.cm + Age:W.Kg + Age:FatMass.Kg + Age:FM + Age:FFM.Kg + Age:BMR.Kcal + Height.cm:W.Kg + Height.cm:FatMass.Kg + Height.cm:FM + Height.cm:FFM.Kg + Height.cm:BMR.Kcal + W.Kg:FatMass.Kg + W.Kg:FM + W.Kg:FFM.Kg + W.Kg:BMR.Kcal + FatMass.Kg:FM + FatMass.Kg:FFM.Kg + FatMass.Kg:BMR.Kcal + FM:FFM.Kg + FM:BMR.Kcal + FFM.Kg:BMR.Kcal, data = x)

# Set up the grid of tuning parameters to search over
alpha_vals <- 1  # LASSO penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
set.seed(53)
cv_fit <- cv.glmnet(x_interact, y, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10, standardize = FALSE)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
lasso_fit <- glmnet(x_interact, y, alpha = alpha_vals, lambda = best_lambda, standardize = FALSE)
best_model_coef <- coef(lasso_fit, s = best_lambda)


#2.4



# Set up the grid of tuning parameters to search over
alpha_vals <- 0  # LASSO penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
set.seed(53)
cv_fit <- cv.glmnet(x_interact, y, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10, standardize = FALSE)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
ridge_fit <- glmnet(x_interact, y, alpha = alpha_vals, lambda = best_lambda, standardize = FALSE)
best_model_coef <- coef(ridge_fit, s = best_lambda)


#2.4

# Set up the grid of tuning parameters to search over
alpha_vals <- 0  # Ridge penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
set.seed(53)
cv_fit <- cv.glmnet(x_interact, y, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10, standardize = FALSE)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
ridge_fit <- glmnet(x_interact, y, alpha = alpha_vals, lambda = best_lambda, standardize = FALSE)
best_model_coef <- coef(ridge_fit, s = best_lambda)

# Evaluate the performance of the model on the test data using root mean squared error (RMSE)
x_test_interact <- model.matrix(~ Age*Height.cm + Age*W.Kg + Age*FatMass.Kg + Age*FM + Age*FFM.Kg + Age*BMR.Kcal +
                                  Height.cm*W.Kg + Height.cm*FatMass.Kg + Height.cm*FM + Height.cm*FFM.Kg + Height.cm*BMR.Kcal +
                                  W.Kg*FatMass.Kg + W.Kg*FM + W.Kg*FFM.Kg + W.Kg*BMR.Kcal +
                                  FatMass.Kg*FM + FatMass.Kg*FFM.Kg + FatMass.Kg*BMR.Kcal +
                                  FM*FFM.Kg + FM*BMR.Kcal + FFM.Kg*BMR.Kcal, data = test_data)

y_pred <- predict(ridge_fit, newx = x_test_interact, s = best_lambda)
RMSE <- sqrt(mean((y_test - y_pred)^2))
RMSE





#3.1

library(dplyr)
library(glmnet)

# Select the required variables
Required_variables <- data[c("BMI","Age", "Height.cm", "W.Kg", "FatMass.Kg", "FM", "FFM.Kg", "BMR.Kcal")]

# Normalize the variables (excluding Age)
Normalized_variables <- as.data.frame(lapply(Required_variables[, -2], scale))

# Add Age back to the normalized variables
Normalized_variables <- cbind(Age = Required_variables$Age, Normalized_variables)

# Split the data into training and testing sets
set.seed(53)
test_data <- Normalized_variables[sample(nrow(Normalized_variables), 45), ]
train_data <- anti_join(Normalized_variables, test_data)

# Set up the predictor variables and response variable
x_train <- as.matrix(train_data[, c("Age", "Height.cm", "W.Kg", "FatMass.Kg", "FM", "FFM.Kg", "BMR.Kcal")])
y_train <- train_data$BMI

# Set up the grid of tuning parameters to search over
alpha_vals <- 1  # LASSO penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
set.seed(53)
cv_fit <- cv.glmnet(x_train, y_train, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
lasso_fit <- glmnet(x_train, y_train, alpha = alpha_vals, lambda = best_lambda)
best_model_coef <- coef(lasso_fit, s = best_lambda)

#3.2

# Set up the grid of tuning parameters to search over
alpha_vals <- 0  # LASSO penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
set.seed(100)
cv_fit <- cv.glmnet(x_train, y_train, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
ridge_fit <- glmnet(x_train, y_train, alpha = alpha_vals, lambda = best_lambda)
best_model_coef <- coef(ridge_fit, s = best_lambda)

#3.3
  
library(dplyr)
library(glmnet)



# Select the required variables
Required_variables <- data[c("BMI","Age", "Height.cm", "W.Kg", "FatMass.Kg", "FM", "FFM.Kg", "BMR.Kcal")]

# Normalize the variables (excluding Age)
Required_variables_norm <- Required_variables %>% 
  mutate(across(c("Height.cm", "W.Kg", "FatMass.Kg", "FM", "FFM.Kg", "BMR.Kcal"), scale))

# Create interaction terms
Required_variables_interact <- Required_variables_norm %>% 
  mutate(
    H_W <- Height.cm * W.Kg,
    H_F <- Height.cm * FatMass.Kg,
    H_B <- Height.cm * BMR.Kcal,
    W_F <- W.Kg * FatMass.Kg,
    W_B <- W.Kg * BMR.Kcal,
    F_B <- FatMass.Kg * BMR.Kcal
  )

# Set up the predictor variables and response variable
x_interact_norm <- as.matrix(Required_variables_interact[, c("Age", "Height.cm", "W.Kg", "FatMass.Kg", "FM", "FFM.Kg", "BMR.Kcal")])
y <- Required_variables_interact$BMI

# Set up the grid of tuning parameters to search over
alpha_vals <- 1  # LASSO penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
set.seed(53)
cv_fit <- cv.glmnet(x_interact_norm, y, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
lasso_fit <- glmnet(x_interact_norm, y, alpha = alpha_vals, lambda = best_lambda)
best_model_coef <- coef(lasso_fit, s = best_lambda)


#2.4

# Set up the grid of tuning parameters to search over
alpha_vals <- 0  # LASSO penalty
lambda_vals <- 10^seq(4, -2, length.out = 100)

# Perform 10-fold cross-validation using the cv.glmnet() function
set.seed(53)
cv_fit <- cv.glmnet(x_interact_norm, y, alpha = alpha_vals, lambda = lambda_vals, nfolds = 10)

# Extract the optimal value of lambda based on minimum mean cross-validated error
best_lambda <- cv_fit$lambda.min

# Obtain the coefficients of the best model using the glmnet() function
ridge_fit <- glmnet(x_interact_norm, y, alpha = alpha_vals, lambda = best_lambda)
best_model_coef <- coef(ridge_fit, s = best_lambda)



