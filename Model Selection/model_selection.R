# Load the data
crime_data <- read.table("uscrime.txt", header=TRUE, sep="\t")
attach(crime_data)
colnames(crime_data)
dim(crime_data)

# Stepwise regression using AIC
library(MASS)

# Build a full model
full_model <- lm(Crime~., data=crime_data)
step <- stepAIC(full_model,direction = "both")
step$anova

# Stepwise regression using F-test p-value
library(rms)
full_model <- ols(Crime~M+So+Ed+Po1+Po2+LF+M.F+Pop+NW+U1+U2+Wealth+
                    Ineq+Prob+Time,data=crime_data)
fastbw(full_model, rule="p",sls=0.05)

# Build an null model
null<-lm(Crime~1, data=crime_data)
# AIC-based forward selection
model_aic_forward <- step(null, direction="forward", trace=1, scope=~
                            M+So+Ed+Po1+Po2+LF+M.F+Pop+NW+U1+U2+Wealth+
                            Ineq+Prob+Time)
summary(model_aic_forward)

# AIC-based backward selection
model_aic_backward <- step(full_model, direction="backward", trace=1)
summary(model_aic_backward)

# AIC-based forward-backgward selection
null<-lm(Crime~1, data=crime_data)
model_aic_both <- step(null, direction="both", trace=1, scope=~
                         M+So+Ed+Po1+Po2+LF+M.F+Pop+NW+U1+U2+Wealth+
                         Ineq+Prob+Time)
summary(model_aic_both)


# Apply Lasso
library(glmnet)
x <- as.matrix(crime_data[,1:15])
x <- scale(as.matrix(crime_data[,1:15]))
y <- as.matrix(Crime)

lasso_model <- glmnet(x, y, family = "gaussian", standardize = TRUE,
                      alpha = 1)
coef(lasso_model)
plot(lasso_model, label = TRUE)
plot(lasso_model, xvar="lambda", label = TRUE)

coef(lasso_model, s=0.1)

# Software picks the model
set.seed(10)
lasso_cv_fit <- cv.glmnet(x, y, family = "gaussian", standardize = TRUE,
                    alpha = 1)
coef(lasso_cv_fit)
plot(lasso_cv_fit)

# Apply Ridge
ridge_model <- glmnet(x, y, family = "gaussian", standardize = TRUE,
                      alpha = 0)
plot(ridge_model)

# Apply Elastic Net
elasticnet_model <- glmnet(x, y, family = "gaussian", standardize = TRUE,
                      alpha = 0.5)
plot(elasticnet_model)
print(elasticnet_model)

for (i in 0:10) {
  assign(paste("fit", i, sep=""),
         cv.glmnet(x, y, type.measure="mse", alpha=i/10,family="gaussian"))
}
plot(fit5, main="Elastic Net")
