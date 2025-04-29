# Core time series packages
library(forecast)
library(tidyverse)
library(Metrics)
library(dplyr)
library(scoringRules)  # For CRPS

# Filter and join
fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013") %>%
  count(year)

secchi_data <- fcre_smoothed %>%
  left_join(fcreData_WaterTemp %>% select(datetime, Temp_C_mean),
            by = "datetime")

# Create transformed variables
secchi_data <- secchi_data %>%
  mutate(
    Temp_C_mean_sq = secchi_data$Temp_C_mean.x^2,
    Temp_C_mean_log = log1p(secchi_data$Temp_C_mean.x)  # log(1 + x) is safer for 0s
  )

# Remove any rows with missing values in predictors or target
secchi_data <- secchi_data %>%
  drop_na(Secchi_m, Temp_C_mean.x, Temp_C_mean_sq, Temp_C_mean_log)

n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

# Training and testing splits
train <- secchi_data[1:trainN,]
test <- secchi_data[testN:n,]
ts.train <- ts(train$Secchi_m, frequency = 365)

# Helper function to train and forecast using one regressor
run_model <- function(regressor_all, regressor_name) {
  xreg_train <- matrix(regressor_all[1:trainN], ncol = 1)
  xreg_test <- matrix(regressor_all[testN:n], ncol = 1)

  stl.fit <- stlm(ts.train, s.window = "periodic",
                  method = "arima", xreg = xreg_train)

  stl.forecasts <- forecast(stl.fit, h = 30, newxreg = xreg_test)

  # Estimate forecast standard deviation using 80% interval (z ≈ 1.28)
  z_80 <- qnorm(0.9)
  sd_est <- (stl.forecasts$upper[,2] - stl.forecasts$lower[,2]) / (2 * z_80)

  # Compute CRPS
  crps_vals <- crps_norm(y = test$Secchi_m, location = stl.forecasts$mean, scale = sd_est)
  crps_score <- mean(crps_vals, na.rm = TRUE)

  forecast_df <- data.frame(
    time = 1:30,
    observed = test$Secchi_m,
    forecast = stl.forecasts$mean,
    model = regressor_name
  )

  list(
    df = forecast_df,
    rmse = rmse(test$Secchi_m, stl.forecasts$mean),
    mae = mae(test$Secchi_m, stl.forecasts$mean),
    crps = crps_score
  )
}

# Run each model
results_original <- run_model(secchi_data$Temp_C_mean.x, "Original")
results_squared <- run_model(secchi_data$Temp_C_mean_sq, "Squared")
results_log     <- run_model(secchi_data$Temp_C_mean_log, "Log")

# Combine results for plotting and comparison
forecast_comparison <- bind_rows(
  results_original$df,
  results_squared$df,
  results_log$df
)

# Plot comparison
ggplot(forecast_comparison, aes(x = time)) +
  geom_line(aes(y = observed), color = "black", linewidth = 1) +
  geom_line(aes(y = forecast, color = model), linewidth = 1) +
  geom_point(aes(y = forecast, color = model)) +
  labs(title = "Forecast Comparison by Regressor Transformation on Water Temperature",
       y = "Secchi Depth (m)", x = "Time (Days)",
       color = "Model") +
  theme_minimal()

# Numeric comparison table
comparison_metrics <- data.frame(
  Model = c("Original", "Squared", "Log"),
  RMSE = c(results_original$rmse,
           results_squared$rmse,
           results_log$rmse),
  MAE = c(results_original$mae,
          results_squared$mae,
          results_log$mae),
  CRPS = c(results_original$crps,
           results_squared$crps,
           results_log$crps)
)

print(comparison_metrics)
