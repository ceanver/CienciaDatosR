# Read raw data
raw_data <- read.table("breast-cancer-wisconsin.data.txt", header=FALSE, sep=",")
colnames(raw_data) <- c("Sample_code_number", "Clump_Thickness", "Uniformity_of_Cell_Size",
                        "Uniformity_of_Cell_Shape", "Marginal_Adhesion",
                        "Single_Epithelial_Cell_Size", "Bare_Nuclei",
                        "Bland_Chromatin", "Normal_Nucleoli", "Mitoses",
                        "Class")
raw_data[raw_data=="?"] <- NA

# Remove column Sample Code Number as this column may have no predicting power
raw_data <- raw_data[,-1]
dim(raw_data)
sapply(raw_data, class)
raw_data$Bare_Nuclei <- as.integer(raw_data$Bare_Nuclei)
raw_data$Class <- as.factor(raw_data$Class)
summary(raw_data)
sapply(raw_data, class)

# dataset by removing na
data_remove_na <- raw_data[!is.na(raw_data$Bare_Nuclei),]
dim(data_remove_na)

# dataset by creating new binary variable to indicate missing value
data_binary_indicate_missing <- transform(raw_data, missing = ifelse(is.na(raw_data$Bare_Nuclei),1,0))
data_binary_indicate_missing$Bare_Nuclei[is.na(data_binary_indicate_missing$Bare_Nuclei)] <- 0
data_binary_indicate_missing <- data_binary_indicate_missing[,c(1,2,3,4,5,6,7,8,9,11,10)]
dim(data_binary_indicate_missing)
sapply(data_binary_indicate_missing, class)

# 1. imputation using mean from complete entries
data_mean_imputation <- raw_data
data_mean_imputation$Bare_Nuclei[is.na(data_mean_imputation$Bare_Nuclei)] <-
  mean(data_mean_imputation$Bare_Nuclei, na.rm = TRUE)

# imputation using median from complete entries
data_median_imputation <- raw_data
data_median_imputation$Bare_Nuclei[is.na(data_median_imputation$Bare_Nuclei)] <-
  median(data_median_imputation$Bare_Nuclei, na.rm = TRUE)
sapply(data_median_imputation, class)


# 2. imputation using regression
# using mice library for regression imputation
library(mice)
missing_data_regression_imputation_mice <- mice(raw_data, method = "norm.predict",m=1)
data_regression_imputation_mice <- complete(missing_data_regression_imputation_mice)


# 3. using mice library for regression imputation with perturbation
# https://www.rdocumentation.org/packages/mice/versions/2.46.0/topics/mice.impute.norm.nob
missing_data_regression_perturb_imputation_mice <- mice(raw_data, method = "norm.nob",m=1)
data_regression_perturb_imputation_mice <- complete(missing_data_regression_perturb_imputation_mice)

# 4. apply svm on imputated dataset, refer to # HW1 code
# https://www.rdocumentation.org/packages/kernlab/versions/0.9-25/topics/ksvm
library(kernlab)
accuracy_mean_imputation <- run_ksvm(data_mean_imputation)
accuracy_median_imputation <- run_ksvm(data_median_imputation)
accuracy_regression_imputation_mice <- run_ksvm(data_regression_imputation_mice)
accuracy_regression_perturb_imputation_mice <- run_ksvm(data_regression_perturb_imputation_mice)
accuracy_remove_na <- run_ksvm(data_remove_na)
accuracy_add_missing_column <- run_ksvm_additional_column(data_binary_indicate_missing)

run_ksvm <- function(df) {
  set.seed(10)
  m <- nrow(df)
  val<- sample(1:m, size = round(m*0.75), replace=FALSE)
  print(val)
  df_train <- df[val,]
  df_test <- df[-val,]

  model <- ksvm(as.matrix(df_train[,1:9]),df_train[,10], type="C-svc",
                kernel="vanilladot", C=(0.1), scaled=TRUE)
  print(model@b)
  pred <- predict(model, df_test[,1:9])
  #print(pred)
  accuracy <- sum(pred==df_test[,10])/nrow(df_test)
  return (accuracy)
}

run_ksvm_additional_column <- function(df) {
  set.seed(10)
  m <- nrow(df)
  val<- sample(1:m, size = round(m*0.75), replace=FALSE)
  print(val)
  df_train <- df[val,]
  df_test <- df[-val,]

  model <- ksvm(as.matrix(df_train[,1:10]),df_train[,11], type="C-svc",
                kernel="vanilladot", C=(0.1), scaled=TRUE)
  print(model@b)
  pred <- predict(model, df_test[,1:10])
  accuracy <- sum(pred==df_test[,11])/nrow(df_test)
  return (accuracy)
}

methods <- c("mean_imputation","median_imputation", "regression_imputation",
  "regression_perturbation_imputation", "remove_na", "add_missing_column")

accuracy <- c(accuracy_mean_imputation, accuracy_median_imputation,
              accuracy_regression_imputation_mice, accuracy_regression_perturb_imputation_mice,
              accuracy_remove_na, accuracy_add_missing_column)


as.table(setNames(accuracy,methods))
