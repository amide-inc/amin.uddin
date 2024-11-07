library('quantmod')
library('zoo')
library('tidyr')
library('dplyr')


# Load necessary libraries
# install.packages("moments") # Install moments if not already installed
library(moments)
# Load your dataset (assuming it's in CSV format)
# Replace 'your_data.csv' with the actual file path or file name
data <- read.csv("./zomato.csv")

getwd()
setwd("/Users/amin/Desktop/pune")

# Display the first few rows of the data to confirm it's loaded correctly
head(data)

sum(is.na(data))

# Display summary statistics of the 'Close' column
summary(data$Close)

#dimension and size of dataset
dim(data)
nrow(data)
ncol(data)

# Calculate the standard deviation of the 'Close' column
sd_value <- sd(data$Close)
print(paste("Standard Deviation:", sd_value))

# Calculate the skewness of the 'Close' column
skewness_value <- skewness(data$Close)
print(paste("Skewness:", skewness_value))

# Calculate the kurtosis of the 'Close' column
kurtosis_value <- kurtosis(data$Close)
print(paste("Kurtosis:", kurtosis_value))

sum(is.na(data$Date))
sum(is.na(data$Close))
head(data)

#Time series plot

data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
head(data)
plot(data$Date, data$Close, type = "l", main = "Stock Price Over Time", xlab = "Date", ylab = "Close Price")

#adding movin average
data$SMA20 <- SMA(data$Close, n = 20)
head(data,30)
plot(data$Date, data$SMA20, type = "l", col = "red", main = "20-Day SMA")



# Plot the first series (SMA20)
plot(data$Date, data$SMA20, type = "l", col = "red", main = "Stock Price and 20-Day SMA", 
     xlab = "Date", ylab = "Price")

# Add the second series (Close) to the same plot
lines(data$Date, data$Close, col = "blue")

# Optionally, add a legend to distinguish between the lines
legend("topright", legend = c("SMA20", "Close Price"), col = c("red", "blue"), lty = 1)

class(data$log_return)

#add log return
data$log_return = log(data$Close)
head(data)


hist(data$log_return, breaks = 50, main = "Histogram of Log Returns", xlab = "Log Returns")

plot(density(data$log_return), main = "Density Plot of Log Returns", xlab = "Log Returns")


boxplot(data$log_return, main = "Boxplot of Daily Returns", ylab = "Log Returns")
boxplot(data$Close, main = "Boxplot of Daily Returns", ylab = "Log Returns")
data$RSI14 <- RSI(data$Close, n = 14)
head(data)

correlation_matrix <- cor(data[, c("Close", "Volume", "RSI14", "SMA20")])
correlation_matrix <- cor(data[, c("Close", "Volume", "RSI14", "SMA20")], use = "complete.obs")

heatmap(correlation_matrix, main = "Correlation Heatmap")