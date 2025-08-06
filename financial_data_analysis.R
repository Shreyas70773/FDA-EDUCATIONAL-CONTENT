library(quantmod)
library(xts) # xts will also be loaded with quantmod, but good to be explicit
library(tseries)
library(moments)
library(forecast)

# --- Define the ticker and date range ---
# We'll get daily data for AAPL starting from January 1, 2010, up to today (or end of last trading day)
ticker <- "AAPL"
start_date <- "2010-01-01"


# --- Download the data ---
# getSymbols downloads data and stores it as an xts object named after the ticker (e.g., AAPL)
# auto.assign = TRUE means it creates an object named 'AAPL' in your global environment.
getSymbols("AAPL", src = "yahoo", from = start_date, auto.assign = TRUE)

# The downloaded data (AAPL) will be an xts object containing Open, High, Low, Close, Volume, and Adjusted Close prices.
# For financial analysis, especially for calculating returns, we almost always use' price.
# This price is adjusted for stock splits, dividends, and other corporate actions, giving a truer representation
# of the actual return an investor would have received.

# Extract the Adjusted Close price seriesse the 'Adjusted Clo
# The column for Adjusted Close is typically named 'AAPL.Adjusted' (or 'Ticker.Adjusted')
aapl_prices <- Ad(get(ticker)) # get(ticker) retrieves the AAPL object. Ad() is a quantmod helper to get Adjusted Close.

head(aapl_prices)
tail(aapl_prices)
str(aapl_prices)

# --- Compute Daily Log Returns for AAPL ---
# R_t = log(P_t / P_{t-1}) = log(P_t) - log(P_{t-1})
aapl_log_returns <- diff(log(aapl_prices))

# Remove the first NA value (the first return can't be computed as there's no prior day)
aapl_log_returns <- na.omit(aapl_log_returns)

# Display the first few log returns
head(aapl_log_returns)

# How many observations (rows) do we have in aapl_log_returns?
num_observations <- NROW(aapl_log_returns)
print(paste("Number of observations:", num_observations))

# What is the range (min and max values) of the log returns?
returns_range <- range(aapl_log_returns)
print(paste("Minimum log return:", returns_range[1]))
print(paste("Maximum log return:", returns_range[2]))

# You can also get mean, standard deviation etc. (we'll cover these more systematically soon)
# mean(aapl_log_returns)
# sd(aapl_log_returns)

# --- Visualization of AAPL Prices and Log Returns ---

# Plotting the Adjusted Close Prices
plot(aapl_prices, main = "AAPL Adjusted Close Price",
     xlab = "Date", ylab = "Price (USD)",
     col = "steelblue", grid.col = "lightgray")

# Plotting the Daily Log Returns
plot(aapl_log_returns, main = "AAPL Daily Log Returns",
     xlab = "Date", ylab = "Log Return",
     col = "darkred", grid.col = "lightgray")

# To emphasize volatility clustering, let's plot the squared log returns
# Squaring makes both positive and negative large returns equally "large" and positive.
plot(aapl_log_returns^2, main = "AAPL Squared Daily Log Returns (Volatility Proxy)",
     xlab = "Date", ylab = "Squared Log Return",
     col = "darkgreen", grid.col = "lightgray")


mean_return <- mean(aapl_log_returns)
sd_return <- sd(aapl_log_returns)
skewness_return <- skewness(aapl_log_returns)
kurtosis_return <- kurtosis(aapl_log_returns) # Note: 'moments' package gives raw kurtosis, not excess kurtosis

print(paste("Mean Log Return:", round(mean_return, 6)))
print(paste("Standard Deviation (Volatility):", round(sd_return, 6)))
print(paste("Skewness:", round(skewness_return, 4)))
print(paste("Kurtosis:", round(kurtosis_return, 4)))
print(paste("Excess Kurtosis (Kurtosis - 3):", round(kurtosis_return - 3, 4)))

# --- 2. Test for Normality (Jarque-Bera Test) ---
jb_test <- jarque.bera.test(aapl_log_returns)
print(jb_test)

# --- 3. Visualize Distribution with Histogram and Normal Curve ---
# Plotting a histogram of the returns
hist(aapl_log_returns, breaks = 50, freq = FALSE,
     main = "Histogram of AAPL Daily Log Returns",
     xlab = "Log Return", ylab = "Density",
     col = "lightblue", border = "darkblue")

# Overlaying a normal distribution curve for comparison
# First, calculate parameters for the theoretical normal curve
x_fit <- seq(min(aapl_log_returns), max(aapl_log_returns), length.out = 100)
y_fit <- dnorm(x_fit, mean = mean_return, sd = sd_return)
lines(x_fit, y_fit, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Empirical Returns", "Normal Distribution"),
       col = c("darkblue", "red"), lwd = c(NA, 2), pch = c(22, NA),
       pt.bg = c("lightblue", NA), bty = "n")

# A Q-Q plot is also excellent for checking normality visually
qqnorm(aapl_log_returns, main = "Normal Q-Q Plot of AAPL Daily Log Returns")
qqline(aapl_log_returns, col = "red", lwd = 2) # Adds a reference line for normality

# --- Plotting ACF and PACF ---

# Set plotting layout to have two plots side-by-side
par(mfrow = c(1, 2))

# 1. ACF of the NON-STATIONARY price series (AAPL.Adjusted)
# We expect to see a very slow decay, a hallmark of a unit root process.
acf(aapl_prices, main = "ACF of AAPL Adjusted Price", lag.max = 50)
pacf(aapl_prices, main = "PACF of AAPL Adjusted Price", lag.max = 50)

# The plot of the price series will show an ACF that stays very high for many lags.
# The PACF will have a huge spike at lag 1 and be near zero elsewhere. This is classic random walk behavior.

# Reset plotting layout to single plot if desired later
# par(mfrow = c(1, 1))

# Now, let's analyze our STATIONARY log returns
par(mfrow = c(1, 2)) # back to two plots

# 2. ACF and PACF of the STATIONARY log return series
# We are looking for the "signatures" of AR, MA, or ARMA processes.
acf(aapl_log_returns, main = "ACF of AAPL Log Returns", na.action = na.pass, lag.max = 50)
pacf(aapl_log_returns, main = "PACF of AAPL Log Returns", na.action = na.pass, lag.max = 50)

# The blue dashed lines represent the 95% confidence interval.
# If a spike extends beyond these lines, it is considered statistically significant.

# --- Fitting Simple ARMA Models ---
# We will use the 'stats' package, which comes with base R.
# The function is arima().

# Our hypothesis: An ARMA(0,0) model (white noise) is a good fit.
# This is an ARIMA(0, 0, 0) since our data is already stationary (d=0).
fit_wn <- arima(aapl_log_returns, order = c(0, 0, 0))
print(fit_wn)

# Let's also try fitting a simple AR(1) and MA(1) model.
# AR(1) model -> ARIMA(1, 0, 0)
fit_ar1 <- arima(aapl_log_returns, order = c(1, 0, 0))
print(fit_ar1)

# MA(1) model -> ARIMA(0, 0, 1)
fit_ma1 <- arima(aapl_log_returns, order = c(0, 0, 1))
print(fit_ma1)

# --- Checking Residuals ---
# A good model should leave residuals that are white noise (no autocorrelation).
# Let's check the ACF of the residuals for each model.

# Set smaller margins and figure size
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0))

# ACF of residuals from the White Noise ARMA(0,0) model
acf(residuals(fit_wn), main = "ACF of ARMA(0,0) Residuals")

# ACF of residuals from the AR(1) model
acf(residuals(fit_ar1), main = "ACF of AR(1) Residuals")

# ACF of residuals from the MA(1) model
acf(residuals(fit_ma1), main = "ACF of MA(1) Residuals")

par(mfrow = c(1, 1)) # Reset plotting

# To formally test for autocorrelation in residuals, we use the Ljung-Box test.
# Null Hypothesis (H0): The residuals are independently distributed (no autocorrelation).
# We want a HIGH p-value, which means we FAIL to reject the null.
# A high p-value suggests the model has captured the autocorrelation structure well.
Box.test(residuals(fit_wn), type = "Ljung-Box")
Box.test(residuals(fit_ar1), type = "Ljung-Box")
Box.test(residuals(fit_ma1), type = "Ljung-Box")

par(mfrow = c(1, 1)) # Reset to default

# Load the dataset (it's built-in)
data("AirPassengers")
AP <- AirPassengers

# Inspect the data
class(AP) # It's a 'ts' object, another common time series class in R
start(AP); end(AP); frequency(AP)
print(AP)

# Plot the data
plot(AP, main = "Monthly Airline Passengers 1949-1960",
     ylab = "Number of Passengers (in thousands)",
     xlab = "Year")

# --- 1. Fit a Simple Exponential Smoothing (SES) model ---
# We force it to be SES by turning off trend (beta) and seasonality (gamma)
ses_fit <- HoltWinters(AP, beta = FALSE, gamma = FALSE)

# --- 2. Fit Holt's Exponential Smoothing model ---
# We turn off seasonality (gamma) but allow the model to estimate trend (beta)
holt_fit <- HoltWinters(AP, gamma = FALSE)

# --- 3. Fit the full Holt-Winters model ---
# We let the function estimate all three components.
# It will automatically detect the frequency (12 for monthly)
# We can specify whether seasonality is 'additive' or 'multiplicative'.
# Given our visual analysis, 'multiplicative' is more appropriate.
hw_fit <- HoltWinters(AP, seasonal = "multiplicative")

# --- 4. Generate and Plot Forecasts ---
# Let's forecast 5 years (60 months) into the future for each model
ses_forecast <- forecast(ses_fit, h = 60)
holt_forecast <- forecast(holt_fit, h = 60)
hw_forecast <- forecast(hw_fit, h = 60)

# Plot the results to compare
# Set a larger plotting window
# par(mar=c(5,4,4,2)+0.1) # you might need to adjust margins

# Plot the Holt-Winters forecast first as it's the most comprehensive
plot(hw_forecast, main = "Forecasts from Exponential Smoothing Models",
     ylab = "Number of Passengers", xlab = "Year",
     fcol = "blue", flwd = 2) # fcol for forecast line color

# Now, overlay the other forecasts for comparison
# Holt's forecast line
lines(holt_forecast$mean, col = "red", lwd = 2)
# SES forecast line
lines(ses_forecast$mean, col = "darkgreen", lwd = 2)

# Add a legend to make sense of the lines
legend("topleft", legend = c("Holt-Winters", "Holt's (Trend)", "SES (Level only)", "Actual Data"),
       col = c("blue", "red", "darkgreen", "black"), lwd = 2, bty = "n")
