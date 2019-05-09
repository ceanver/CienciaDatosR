# Classification using Kernlab ksvm
# import library kernlab
library(kernlab)

# import raw data
rawdata <- read.table("credit_card_data-headers.txt", header=TRUE, sep="\t")
summary(rawdata)

# copy rawdata to data frame data
data<-rawdata

# Call ksvm (ksvm can be used for classification , for regression, or for 
# novelty detection.)
# Depending on whether y is a factor or not, the default setting for type is 
# C-svc or eps-svr
# vanilladot is for Linear kernel
# C:Cost of constraints violation (default:1) this is the 'C' constant of 
# the regularaization term in the Lagrange formulation
# scaling is required
model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]), type="C-svc", 
              kernel="vanilladot", C=(100), scaled=TRUE)

# Calculate parameter a1 to am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])

# calculate a0
a0 <- -model@b

# compute what the model predicts
pred <- predict(model,data[,1:10])

# Tabulate Prediction vs Actual
table(data[,11],pred)

# compute accuracy
sum(pred==data[,11])/nrow(data)


# Classification using Kernlab ksvm
# import library kknn
library(kknn)

# Use train.kknn to find the model with optimal k value via leave-one-out for all 654 observations
fit<-train.kknn(R1 ~.,data,kmax=100, kernel="rectangular",scale=TRUE)
plot(fit)
title("Cross Validation (Leave-one-out)")

# Make prediction using model "fit"
pred<-predict(fit,data)
pred_bin<-round(pred)

#Finding the prediction accuracy
table(pred_bin,data$R1)
sum(pred_bin==data[,11])/nrow(data)
summary(fit)

# Setting seed to produce reproducible results
set.seed(1)
m<-nrow(data)

# Randomly selecting 1/3 rd indices among 654 indices
val<-sample(1:m,size=round(m/3), replace = FALSE)
d.learn <- data[-val,]
d.valid <- data[val,]

# Use train.kknn to find the model with optimal k value
fit<-train.kknn(R1 ~.,d.learn,kmax=100, kernal="rectangular", scale=TRUE)
plot(fit)
title("Cross Validation")

# Make prediction using model "fit"
pred<-predict(fit,d.valid)
pred_bin<-round(pred)

#Finding the prediction accuracy
table(pred_bin,d.valid$R1)
sum(pred_bin==d.valid[,11])/nrow(d.valid)
