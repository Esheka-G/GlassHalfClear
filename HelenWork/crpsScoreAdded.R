# Load packages
library(forecast)
library(tidyverse)
library(Metrics)
library(scoringRules)  # for CRPS

# Use smoothed fcre data
secchi_data <- fcre_smoothed

# Optional: remove early partial year
secchi_data <- secchi_data |>
  mutate(year = format(datetime, "%Y")) |>
  filter(year != "2013")

# Train/test split
n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

train <- secchi_data[1:trainN, ]
test <- secchi_data[testN:n, ]

# STL requires a univariate time series object
ts.train <- ts(train$Secchi_m, frequency = 365)

# Fit STL model with ARIMA
stl.fit <- stlm(ts.train, s.window = "periodic", method = "arima")

# Forecast next 30 days
stl.forecasts <- forecast(stl.fit, h = 30)

# Turn forecast into data frame
stl.df <- as.data.frame(stl.forecasts)

# Compare observed vs forecast
compare <- data.frame(
  time = 1:30,
  observed = test$Secchi_m,
  forecast = stl.df$`Point Forecast`
)

# Plot
ggplot(compare, aes(x = time)) +
  geom_line(aes(y = observed), color = "blue") +
  geom_point(aes(y = observed), color = "blue") +
  geom_line(aes(y = forecast), color = "red") +
  geom_point(aes(y = forecast), color = "red") +
  labs(title = "Observed vs STL Forecast", y = "Secchi (m)", x = "Time (Days)")

# Error metrics
stl.rmse <- Metrics::rmse(compare$observed, compare$forecast)
stl.mae <- Metrics::mae(compare$observed, compare$forecast)

# Estimate forecast SD from 80% interval (z ≈ 1.28)
z_80 <- qnorm(0.9)
sd_est <- (stl.df$`Hi 80` - stl.df$`Lo 80`) / (2 * z_80)

# Compute CRPS
crps_vals <- crps_norm(y = compare$observed, location = compare$forecast, scale = sd_est)
crps_score <- mean(crps_vals, na.rm = TRUE)

# Combine metrics
comparisonDF <- data.frame(
  model = "stl",
  RMSE = stl.rmse,
  MAE = stl.mae,
  CRPS = crps_score
)

print(comparisonDF)
