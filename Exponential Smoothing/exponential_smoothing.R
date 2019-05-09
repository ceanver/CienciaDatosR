# Load the data
temp_data <- read.table("temps.txt", header=TRUE, sep="\t")
#attach(temp_data)
colnames(temp_data)
dim(temp_data)
temp_data <- t(temp_data)

temp_data_series <- temp_data[2:21,]
rownames(temp_data_series) <- c()
temp_data_series <- as.vector(t(temp_data_series))
temp_data_series <- as.numeric(temp_data_series)
temp_data_series <- ts(temp_data_series, frequency=123, start=c(1996,1), end=c(2015,123))
plot(temp_data_series)

# Decompose function using moving average
# https://www.rdocumentation.org/packages/stats/versions/3.4.1/topics/HoltWinters
temp_data_series_components <- decompose(temp_data_series, type="multiplicative")
plot(temp_data_series_components)

# stl function to break down data by trend, seasonal and remainder
temp_data_series_stl <- stl(temp_data_series, s.window = "period")
plot(temp_data_series_stl)

# HoltWinters/triple exponential smoothing method
temp_data_holtwinters <- HoltWinters(temp_data_series)
temp_data_holtwinters_matrix <- matrix(temp_data_holtwinters$fitted[,4],ncol=123)
plot(temp_data_holtwinters)
plot(fitted(temp_data_holtwinters))

##---------------------------------------------------------------
# Optional Trail on forecast function
library(forecast)
temp_data_hw <- HoltWinters(temp_data_series, seasonal = "mult")
plot(temp_data_hw)
#temp_data_hw_forecast <- predict(temp_data_hw, n.ahead = 50, prediction.interval = T)
temp_data_hw_forecast <- forecast.HoltWinters(temp_data_hw, h=20)
plot(forecast(temp_data_hw))
