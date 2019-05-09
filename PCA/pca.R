# Load the data
crime_data <- read.table("uscrime.txt", header=TRUE, sep="\t")
#attach(crime_data)
colnames(crime_data)
dim(crime_data)

# Get the full regression model
full_regression_model <- lm(crime_data$Crime~.,data=crime_data)
summary(full_regression_model)

# Perform PCA using function ”prcomp” and select first 4 principle components
pca_model <- prcomp(crime_data[,-16], scale. = TRUE)
V <- pca_model$rotation
V_reduced <- V[,1:4]
T_R_computed <- pca_model$x

# Reconstruct regression model using (X times 4 principle components)
reconstructed_regression_model <- lm(crime_data$Crime~T_R_computed[,1:4])
summary(reconstructed_regression_model)
Reconstructed_regression_coefficients <-
  as.matrix(reconstructed_regression_model$coefficients[-1])

# Reconstruct coefficients for scaled original dataset X
A <- V_reduced %*% Reconstructed_regression_coefficients
A - as.matrix(full_regression_model$coefficients[-1])

# Compare observed vs predicted plots for full and pca regression models
par(mfrow = c(1,2))
plot(crime_data$Crime, predict(full_regression_model), xlab = "Observed Crime",
     ylab = "Predicted Crime", main = "Full Regression Model", abline(a = 0, b = 1, col = "red"))
plot(crime_data$Crime, predict(reconstructed_regression_model),
     xlab = "Observed Crime", ylab = "Predicted Crime", main = "PCA Re-Constructed Model",
     abline(a = 0, b = 1, col = "red"))

# Check whether R-square improved if we retained more columns in V
# The result shows by increasing number of columns retained, we got improved R-square
# It might indicates more variance captured by the reconstructed model

# set number of columns in V/$rotation you would like to retain
i=10
V_reduced_test <- V[,1:i]
T_R_computed <- pca_model$x
reconstructed_regression_model_test <- lm(crime_data$Crime~T_R_computed[,1:i])
summary(reconstructed_regression_model_test)

Reconstructed_regression_coefficients_test <-
  as.matrix(reconstructed_regression_model_test$coefficients[-1])

A_test <- V_reduced_test %*% Reconstructed_regression_coefficients_test
A_test - as.matrix(full_regression_model$coefficients[-1])

par(mfrow = c(1,2))
plot(crime_data$Crime, predict(full_regression_model), xlab = "Observed Crime",
     ylab = "Predicted Crime", main = "Full Regression Model", abline(a = 0, b = 1, col = "red"))
plot(crime_data$Crime, predict(reconstructed_regression_model_test),
     xlab = "Observed Crime", ylab = "Predicted Crime", main = "PCR Re-Constructed Model",
     abline(a = 0, b = 1, col = "red"))
