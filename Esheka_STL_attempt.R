# Core time series packages
library(forecast)   # For STL and ARIMA
library(tidyverse)  # For data wrangling and plotting

# Example: Load your data
secchi_data <- secchi_with_site_info

# Preview the structure
head(secchi_with_site_info)


# Convert to time series — here we assume weekly data
secchi_ts <- ts(secchi_data$observation, start = c(2013, 1), frequency = 52)


# Apply STL
stl_fit <- stl(secchi_ts, s.window = "periodic")

# Plot the decomposition
plot(stl_fit)
trend <- stl_fit$time.series[, "trend"]
seasonal <- stl_fit$time.series[, "seasonal"]
remainder <- stl_fit$time.series[, "remainder"]

# Plot individually
autoplot(trend) + ggtitle("Trend Component")

# Get residuals (deseasonalized data)
residuals <- remainder

# Fit ARIMA model on remainder
arima_fit <- auto.arima(residuals)

# Forecast next 35 days/weeks
forecast_resid <- forecast(arima_fit, h = 35)

# Reconstruct final forecast by adding trend + seasonality back
seasonal_forecast <- rep(tail(seasonal, 1), 35)  # repeat last known season
trend_forecast <- rep(tail(trend, 1), 35)        # same for trend

final_forecast <- forecast_resid$mean + trend_forecast + seasonal_forecast

# Plot forecast
autoplot(final_forecast) + ggtitle("35-Day Secchi Depth Forecast (STL + ARIMA)")

