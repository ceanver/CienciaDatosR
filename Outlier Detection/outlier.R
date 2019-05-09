# Outlier Detection
library(outliers)

# import raw data
rawdata <- read.table("uscrime.txt", header=TRUE, sep="\t")

# import raw dta without headers
summary(rawdata)

# copy rawdata to data frame data
data<-rawdata
dim(data)
plot(data[,16])
attach(data)
boxplot(Crime)

# Use grubbs
# highest crime city
# type=10 for one outlier
grubbs.test(Crime, type = 10, opposite = FALSE, two.sided = FALSE)

# lowest crime city
# opposite set as true
grubbs.test(Crime, type = 10, opposite = TRUE, two.sided = FALSE)
