# import raw data
rawdata <- read.table("credit_card_data-headers.txt", header=TRUE, sep="\t")

# copy rawdata to data frame data
data<-rawdata

# import library kknn
library(kknn)

m<-nrow(data)

# Setting seed to produce reproducible results
set.seed(10)
val<-sample(1:m,size=round(m*0.8), replace = FALSE)
d.cross <- data[val,]
d.test <- data[-val,]

#Randomly shuffle the crossvalidation data (80% of total dataset)
d.cross <- d.cross[sample(nrow(d.cross)),]
folds <-cut(seq(1,nrow(d.cross)), breaks=10, labels=FALSE)

# For k value in kknn, we iterate between 1 to 100
# For k-fold cross validation, we set k=10
avg_model_acc<-numeric()
ideal_k<-0

for(i in 1:100){
  avg_model_acc[i]<-0
  #print(i)
  for(j in 1:10){
  pred_result<-integer()
  test_indices <- which(folds==j, arr.ind=TRUE)
  valid_data <- d.cross[test_indices,]
  train_data <- d.cross[-test_indices,]
  kknnmodel <- kknn(R1~.,train_data, valid_data, k=i, scale=TRUE, kernel="rectangular")
  prediction <- predict(kknnmodel)
  #print(j)
  #print(prediction)
  #pred_result <- append(pred_result, round(prediction))
  pred_result <- round(prediction)
  #print(pred_result)
  avg_model_acc[i] = avg_model_acc[i] + sum(pred_result==valid_data[,11])/nrow(valid_data) 
  }
  avg_model_acc[i]<-(avg_model_acc[i]/10)
}  

ideal_k<-which(avg_model_acc==max(avg_model_acc))
cat("ideal value of k is: ", ideal_k)


# Train again using the whole cross validation set.
kknnmodel <- kknn(R1~.,d.cross, d.test, k=ideal_k, scale=TRUE, kernel="rectangular")
prediction <- predict(kknnmodel)
pred_result <- round(prediction)

# Calculate the accuracy of model we chose using test dataset
final_pred_acc <- sum(pred_result==d.test[,11])/nrow(d.test)
print(final_pred_acc)

