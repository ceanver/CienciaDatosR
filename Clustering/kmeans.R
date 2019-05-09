library(datasets)
library(ggplot2)
data <- iris
summary(data)
ggplot(data, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
ggplot(data, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

# Make a list of combinations of predictors
attr_comb <- list(1, 2, 3, 4, c(1,2) , c(1,3) , c(1,4) , c(2,3) , c(2,4) , c(3,4) , c(1,2,3) , c (1,2,4) , c(1,3,4) , c(2,3,4) , c(1,2,3,4))
attr_list <- colnames(data)

# Iterate through all the combinations, calculate the r-squared values for each combination
ss_explained <- numeric()
for (comb in attr_comb) {comb_ss <- numeric()
  for (k in 1:10){
    model <- kmeans(data[,comb], k, nstart = 20)
    comb_ss <- c(comb_ss , model$betweenss/model$totss)
  }
ss_explained <- rbind (ss_explained , comb_ss)
}

# Plot the k iterations for each attribute combination to find the best k
for (i in 1:nrow(ss_explained)){ 
  comb_index = attr_comb[i] 
  comb_name = ''
  for (j in comb_index){
  comb_name = paste(comb_name, attr_list[j], sep = '+')
  }
  plot(1:10, ss_explained[i,], type='b', main=comb_name, xlab='k',ylab='BSS/TSS') 
}

# Use best k of 3 and re-run through different combination of predictors
for (comb in attr_comb){
  best_model <- kmeans(data[,comb] , center=3, nstart = 20) 
  print (comb)
  print(table(data$Species , best_model$cluster))
}
