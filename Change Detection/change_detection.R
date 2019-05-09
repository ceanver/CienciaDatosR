# Load the data
temp_data <- read.table("temps.txt", header=TRUE, sep="\t")
#attach(temp_data)

# Empty vector to store the fall starts dates &
# temperature on that day for different years
fall_starts <- c()
fall_starts_temps <- c()
summer_avg_temps <- c()

# Outer loop to iterate through column 2(1996) to column 21 (2015)
for (i in 2:21) {
  # Get temperature values and date info
  Temp_Year <- temp_data[,i]
  Temp_Date <- temp_data[,1]

  # Compute the mean temp in summer (July 1st to 31st Aug)
  Temp_Summer_Mean <- mean(as.matrix(temp_data[1:62,i]))

  # Create 1x2 chart canvas
  par(mfrow=c(1,2))
  # Plot line chart on data
  plot(temp_data[,i]~as.Date(temp_data[,1], "%d-%b"),type="l", xlab = "Date", ylab="Temperature")
  # Add blue line to show the avg temp of summer in the year selected
  abline(Temp_Summer_Mean, 0, lty=3, col="blue")

  # Set threshold for cusum and critical value (tolerence/buffer)
  Critical_Value <- 5
  Threshold <- 80

  # Initiate C, Cplus and Cmins. These three the the vectors to
  # capture cumulative sum for each iteration
  # Initiate stoC, stoCplus, and stoCmins, these are cumulativee sum variable
  # gets updated in each iterature. stoCplus to detect increase
  # stoCmins to detect decrease
  C <- NULL
  Cplus <- NULL
  Cmins <- NULL
  sample_size <-length(Temp_Year)
  stoC <- 0
  stoCplus <- 0
  stoCmins <- 0
  summer_ends <- c()

  # Inner loop to iterate through July 1st to Oct 31st
  for (j in 1:sample_size) {

    # Get value updated for cumulative sum
    stoC <- Temp_Year[j] - Temp_Summer_Mean
    stoCplus <- max(0, stoCplus + stoC - Critical_Value)
    stoCmins <- min(0, stoCmins + stoC + Critical_Value)
    C <- c(C, stoC)
    Cplus <- c(Cplus, stoCplus)
    Cmins <- c(Cmins, stoCmins)

    # Since we are interested in catching the date transition to autumn
    # get the index of j if cusum(minus) is even smaller than the critical/tolerence
    # value (C in Joel's video)
    if(stoCmins < -Threshold) {summer_ends <- c(summer_ends,j)}

    # Tabulate the results of j, date, temp and cumulative sums
    lin <- sprintf("j %4.0f , Date[j] %6s  Temp[j] %4.0f  C, %7.2f  C+ %7.2f  C- %7.2f \n",
                 j, Temp_Date[j],Temp_Year[j], stoC, stoCplus, stoCmins)
    # Choose whether to print out the results
    #cat(lin)
  }

  # The critical value, Hplus and Hmins for plotting. Two directions
  # since we are observing the change in two directions
  Hplus <- Threshold
  Hmins <- -Threshold

  # plot the cusum chart
  ylim <- c(min(c(C, Hmins)), max(c(C, Hplus)))
  plot(as.Date(temp_data[,1], "%d-%b"), C, main=" CUSUM of data", ylim=ylim, xlab = "Date",
     ylab="CUSUM", type = "l")
  lines(as.Date(temp_data[,1], "%d-%b"), Cplus, lty=2)
  lines(as.Date(temp_data[,1], "%d-%b"), Cmins, lty=2)
  abline(0,0)
  abline(Hplus, 0, lty=3)
  abline(Hmins, 0, lty=3)

  # pirnt out the official summer ends date generated from the model
  print(temp_data[summer_ends[1],1])
  print(temp_data[summer_ends[1],i])

  # Update the fall_starts and fall_starts_temps vectors
  fall_starts <- c(fall_starts, summer_ends[1])
  fall_starts_temps <- c(fall_starts_temps,temp_data[(summer_ends[1]),i])
  summer_avg_temps <- c(summer_avg_temps, mean(temp_data[1:fall_starts[i-1],i]))
}

#temp_data[fall_starts,1]
#fall_starts_temps

# Generate the final results
result <- cbind(c(1996:2015),format(as.Date(temp_data[fall_starts,1], "%d-%b"),
                    format="%d-%b"), fall_starts_temps, summer_avg_temps)
result <-data.frame(result)
colnames(result) <- c("Year","Autumn Start Date", "Temp on the Autumn Start Day", "Avg Temp in the Summer")
result

#-------------------------------------------------------------

# Test for specific year specified by column numbers
# Set i here to determine which year to run
i=4
Temp_Year <- temp_data[,i]
Temp_Date <- temp_data[,1]
Temp_Summer_Mean <- mean(as.matrix(temp_data[1:62,i]))
par(mfrow=c(1,2))
plot(temp_data[,i]~as.Date(temp_data[,1], "%d-%b"),type="l", xlab = "Date", ylab="Temperature")
abline(Temp_Summer_Mean, 0, lty=3, col="blue")

Critical_Value <- 5
Threshold <- 80

C <- NULL
Cplus <- NULL
Cmins <- NULL
sample_size <-length(Temp_Year)
stoC <- 0
stoCplus <- 0
stoCmins <- 0
summer_ends <- c()

for (j in 1:sample_size) {
  stoC <- Temp_Year[j] - Temp_Summer_Mean
  stoCplus <- max(0, stoCplus + stoC - Critical_Value)
  stoCmins <- min(0, stoCmins + stoC + Critical_Value)
  C <- c(C, stoC)
  Cplus <- c(Cplus, stoCplus)
  Cmins <- c(Cmins, stoCmins)
  if(stoCmins < -Threshold) {summer_ends <- c(summer_ends,j)}
  lin <- sprintf("j %4.0f , Date[j] %6s  Temp[j] %4.0f  C, %7.2f  C+ %7.2f  C- %7.2f \n",
                 j, Temp_Date[j],Temp_Year[j], stoC, stoCplus, stoCmins)
  cat(lin)
}

Hplus <- Threshold
Hmins <- -Threshold
ylim <- c(min(c(C, Hmins)), max(c(C, Hplus)))
plot(as.Date(temp_data[,1], "%d-%b"), C, main=" CUSUM of data", ylim=ylim, xlab = "Date",
     ylab="CUSUM", type = "l")
lines(as.Date(temp_data[,1], "%d-%b"), Cplus, lty=2)
lines(as.Date(temp_data[,1], "%d-%b"), Cmins, lty=2)
abline(0,0)
abline(Hplus, 0, lty=3)
abline(Hmins, 0, lty=3)

print(temp_data[summer_ends[1],1])
print(temp_data[(summer_ends[1]),i])

#--------------------------------------------------------------
# Q3_2 Compare Summer Avg across years to identify a warming year
Temp_Year <- summer_avg_temps
Temp_Date <- c(1996:2015)
Temp_Summer_Mean <- mean(summer_avg_temps)

# Plot the Summer Avg (Summer Period from July 1st to Autumn Start Date Determined by Q3_1)
par(mfrow=c(1,2))
plot(summer_avg_temps~Temp_Date,type="l", xlab = "Year", ylab="Avg Summer Temperature")
abline(Temp_Summer_Mean, 0, lty=3, col="blue")

Critical_Value <- 0
Threshold <- 5

C <- NULL
Cplus <- NULL
Cmins <- NULL
sample_size <-length(Temp_Year)
stoC <- 0
stoCplus <- 0
stoCmins <- 0
temp_warming <- c()

# For loop to iterate through the summer average temp across year 1996-2015
for (j in 1:sample_size) {
  stoC <- Temp_Year[j] - Temp_Summer_Mean
  stoCplus <- max(0, stoCplus + stoC - Critical_Value)
  stoCmins <- min(0, stoCmins + stoC + Critical_Value)
  C <- c(C, stoC)
  Cplus <- c(Cplus, stoCplus)
  Cmins <- c(Cmins, stoCmins)
  if(stoCplus > Threshold) {temp_warming <- c(temp_warming,j)}
  lin <- sprintf("j %4.0f , Date[j] %6s  Temp[j] %4.0f  C, %7.2f  C+ %7.2f  C- %7.2f \n",
                 j, Temp_Date[j],Temp_Year[j], stoC, stoCplus, stoCmins)
  cat(lin)
}

Hplus <- Threshold
Hmins <- -Threshold
ylim <- c(min(c(C, Hmins)), max(c(C, Hplus)))
plot(Temp_Date, C, main=" CUSUM of data", ylim=ylim, xlab = "Year",
     ylab="CUSUM", type = "l")
lines(Temp_Date, Cplus, lty=2)
lines(Temp_Date, Cmins, lty=2)
abline(0,0)
abline(Hplus, 0, lty=3, col="red")
abline(Hmins, 0, lty=3, col="red")

# Print the result of warming year and its  average summer temperature
print(Temp_Date[temp_warming[1]])
print(Temp_Year[temp_warming[1]])
