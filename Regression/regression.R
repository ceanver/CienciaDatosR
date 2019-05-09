# Load the data
crime_data <- read.table("uscrime.txt", header=TRUE, sep="\t")
colnames(crime_data)
dim(crime_data)

model = lm(crime_data$Crime~.,data=crime_data)
summary(model)


# Check Assumptions
plot(crime_data[,1:16])

# Check Residuals
par(mfrow = c(3,5))
text(0.5,0.5,"Residuals vs Predictors",cex=2,font=2)
plot(crime_data[,1], model$residuals, xlab="M", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,2], model$residuals, xlab="So", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,3], model$residuals, xlab="Ed", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,4], model$residuals, xlab="Po1", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,5], model$residuals, xlab="Po2", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,6], model$residuals, xlab="LF", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,7], model$residuals, xlab="M.F", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,8], model$residuals, xlab="Pop", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,9], model$residuals, xlab="NW", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,10], model$residuals, xlab="U1", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,11], model$residuals, xlab="U2", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,12], model$residuals, xlab="Wealth", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,13], model$residuals, xlab="Ineq", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,14], model$residuals, xlab="Prob", ylab='Residuals')
abline(0,0,col="red")
plot(crime_data[,15], model$residuals, xlab="Time", ylab='Residuals')
abline(0,0,col="red")
mtext("Residuals vs Predictors", side = 3, line =-3, outer = TRUE)


# Fitted vs Residuals, qqplot, histogram and cook's distance
library(car)
par(mfrow =c(2,2))
plot(model$fitted, model$residuals, xlab="Fitted Values", ylab="Residuals")
abline(0,0,col="red")
qqnorm(model$resid,ylab="Residuals", main="")
qqline(model$resid, col="blue")
hist(model$residuals, xlab="Residuals", main="", nclass=15, col="blue")
plot(cooks.distance(model), type="h", lwd=3, col="red", ylab="Cook's Distance")

# Predict new value without transformation
predicting_input <- data.frame(M=14.0, So=0, Ed=10, Po1 = 12, Po2 = 15.5,
                               LF = 0.64, M.F = 94, Pop = 150, NW = 1.1,
                               U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1,
                               Prob = 0.04, Time = 39.0)

predicting_result <- predict(model, predicting_input, interval="prediction")
predicting_result <- predict(model, predicting_input, interval="confidence")

predicting_result

# Transformation: Box-Cox
#boxCox(model, lambda=seq(-1,1,0.1))
trans <- boxCox(model)
trans_df <- as.data.frame(trans)
optimal_lambda <- trans_df[which.max(trans$y),1]
optimal_lambda

# Transform column Crime to the power of optimal_lambda and recompute the model
crime_data_new <- cbind(crime_data, crime_data$Crime^optimal_lambda)
head(crime_data_new,5)
crime_data_new <- crime_data_new[ , -which(names(crime_data_new) %in% c("Crime"))]
names(crime_data_new)[16] = "Crime_transf"
head(crime_data_new,5)
model_new = lm(crime_data_new$Crime_transf~.,data=crime_data_new)
summary(model_new)
plot(model_new)

# Predict new value with transformation using optimal_lambda
predicting_input_new <- data.frame(M=14.0, So=0, Ed=10.0, Po1 = 12.0, Po2 = 15.5,
                               LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1,
                               U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1,
                               Prob = 0.04, Time = 39.0)
predicting_result_new <- predict(model_new, predicting_input_new, interval="prediction")
predicting_result_new <- predicting_result_new^(1/optimal_lambda)
predicting_result_new

