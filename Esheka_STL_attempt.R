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
