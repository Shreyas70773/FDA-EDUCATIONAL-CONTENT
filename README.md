🚧 Still in Progress
This is an ongoing project – more models, datasets, tests, and methods will be added as the course progresses. I’m treating this as a living notebook and reference.



------------------------------------------------------------------------>

📊 Financial Data Analytics in R
This repository is a personal learning log for my Financial Data Analytics class.
All my code, insights, and experiments are compiled into a single R script with detailed comments to help me understand and retain key concepts as I progress.

🧠 What's Included So Far
The current version includes hands-on code and explanations for:

📈 Downloading and analyzing stock data (AAPL) using quantmod

🧮 Computing and interpreting log returns

📊 Visualizing prices, returns, and volatility clustering

📐 Descriptive statistics: mean, standard deviation, skewness, kurtosis

🧪 Normality testing: Jarque-Bera test, histogram with normal overlay, Q-Q plots

🔁 ACF and PACF analysis for both price and return series

🔧 Fitting ARIMA models:

AR(1), MA(1), and White Noise models on returns

Residual diagnostics with ACF and Ljung-Box test

🔮 Forecasting time series (AirPassengers):

Simple Exponential Smoothing (SES)

Holt’s Linear Trend method

Full Holt-Winters (Multiplicative) method

Forecast plots with clear legends

🤖 SARIMA Modeling:

Automatic model selection using auto.arima()

Model summary, residual checks with checkresiduals()

Forecasting and visualization

⚡ Volatility Analysis:

ACF of squared log returns for detecting volatility clustering

📂 File Structure
financial_data_analysis.R – The main and only file (so far). All code is annotated clearly to explain every step and theory behind it.


📚 Libraries Used
quantmod

xts

tseries

moments

forecast

stats

TSA (indirectly used via forecast and tseries functionality)
