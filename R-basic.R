# Loading Required Libraries
# We will use libraries like quantmod for financial data, dplyr for data manipulation, 
# and zoo for handling time series data.

if(!require(quantmod)) install.packages('quantmod')
if(!require(dplyr)) install.packages('dplyr')
if(!require(zoo)) install.packages('zoo')
if(!require(tidyr)) install.packages('tidyr')

library('quantmod')
library('dplyr')
library('zoo')
library('tidyr')

# 1. Data Collection
# We'll use quantmod to fetch financial data, e.g., historical stock prices of Apple (AAPL).
# quantmod provides convenient methods for obtaining data from Yahoo Finance, Google Finance, and others.
getSymbols("AAPL", from = "2020-01-01", to = "2023-01-01")


# Viewing the data
head(AAPL, 10)  # Displays the first few rows of the AAPL stock prices


# 2. Data Cleaning
# We'll handle missing data (if any) and remove unnecessary columns.
# Let's check for any missing data and impute if necessary.
sum(is.na(AAPL))  # Check for missing values


# If there were missing values, we could use forward fill or backward fill for time series
AAPL_clean <- na.locf(AAPL)  # Forward fill NA values
head(AAPL_clean)

nrow(AAPL_clean)

# Removing duplicates (if applicable) – though financial data rarely has duplicates
# AAPL_clean <- AAPL_clean %>% distinct()

# Convert xts object to a data.frame before using distinct()
AAPL_clean_df <- data.frame(Date = index(AAPL_clean), coredata(AAPL_clean))

# Now apply distinct to remove duplicates
AAPL_clean_df <- AAPL_clean_df %>% distinct()

# Convert back to xts if needed
AAPL_clean <- xts(AAPL_clean_df[,-1], order.by = as.Date(AAPL_clean_df$Date))

nrow(AAPL_clean)

# 3. Data Transformation
# Log returns are commonly used for financial time series as they provide a stable variance.
# Let's calculate daily log returns for AAPL stock prices.
AAPL_returns <- diff(log(Cl(AAPL_clean)))  # Cl extracts closing prices
head(AAPL_returns)

# Visualizing Log Returns
plot(AAPL_returns, main="Daily Log Returns of AAPL", col="blue")

# 4. Data Structuring
# Financial data may need to be resampled or indexed by date. 
# We'll resample AAPL data to monthly frequency by taking the monthly mean of closing prices.
AAPL_monthly <- to.monthly(AAPL_clean, indexAt = "lastof", OHLC = FALSE)
head(AAPL_monthly)

nrow()

# 5. Feature Engineering
# Now, let's create some technical indicators like moving averages and the Relative Strength Index (RSI).
# 5.1. Moving Average (e.g., 20-day moving average)
AAPL_clean$SMA_20 <- rollmean(Cl(AAPL_clean), 20, fill = NA, align = "right")
tail(AAPL_clean, 30)

# 5.2. Relative Strength Index (RSI) using quantmod's built-in function
AAPL_clean$RSI <- RSI(Cl(AAPL_clean), n = 14)
tail(AAPL_clean)


# Plot Moving Averages and RSI 
par(mfrow=c(2,1))  # Two plots in one frame
plot(AAPL_clean$AAPL.Close, type='l', main="AAPL Close Price with 20-Day SMA", col="black")
lines(AAPL_clean$SMA_20, col="red")
plot(AAPL_clean$RSI, type='l', main="14-Day RSI for AAPL", col="blue")


#################################################
## HIgh Frequency Data ##########################
#################################################
# 7. Dealing with High-Frequency Data
# Assuming we have high-frequency intraday data (we'll simulate for this example)
# We'll create a simulated high-frequency dataset and apply smoothing using a moving average
set.seed(123)
high_freq_data <- cumsum(rnorm(1000))  # Simulated high-frequency data
high_freq_ma <- rollmean(high_freq_data, 50, fill = NA)


# Plotting high-frequency data and smoothed data
plot(high_freq_data, type = 'l', main = "Simulated High-Frequency Data", col = "purple")
lines(high_freq_ma, col = "orange")


# Rolling window calculations: 30-day rolling mean and standard deviation of log returns
AAPL_rolling_mean <- rollmean(AAPL_returns, 30, fill = NA)
AAPL_rolling_sd <- rollapply(AAPL_returns, 30, sd, fill = NA)

# Plotting rolling statistics
par(mfrow=c(2,1))
plot(AAPL_rolling_mean, type="l", main="30-Day Rolling Mean of AAPL Returns", col="darkblue")
plot(AAPL_rolling_sd, type="l", main="30-Day Rolling Std Dev of AAPL Returns", col="darkred")



