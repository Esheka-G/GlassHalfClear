# Core time series packages
library(forecast)
library(tidyverse)
library(Metrics)
library(dplyr)
library(scoringRules)  # for CRPS

# Filter and join
fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013") %>%
  count(year)

# Join all relevant variables into one dataset
secchi_data_quinn <- fcre_smoothed %>%
  left_join(fcreData_WaterTemp %>% select(datetime, Temp_C_mean), by = "datetime") %>%
  left_join(fcre_secchi_data %>% select(datetime, Chla_ugL_mean, Bloom_binary_mean, Rain_mm_sum), by = "datetime")

# Remove any rows with missing values in predictors or target
secchi_data_quinn <- secchi_data_quinn %>%
  drop_na(Secchi_m, Temp_C_mean.x, Chla_ugL_mean.x, Bloom_binary_mean.x, Rain_mm_sum.x)

# Split into training and test sets
n <- nrow(secchi_data_quinn)
trainN <- n - 30
testN <- trainN + 1
train <- secchi_data_quinn[1:trainN, ]
test <- secchi_data_quinn[testN:n, ]
ts.train <- ts(train$Secchi_m, frequency = 365)

# Function to run model using one regressor
run_model <- function(regressor_all, regressor_name) {
  xreg_train <- matrix(regressor_all[1:trainN], ncol = 1)
  xreg_test <- matrix(regressor_all[testN:n], ncol = 1)

  stl.fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
  stl.forecasts <- forecast(stl.fit, h = 30, newxreg = xreg_test)

  # Estimate forecast SD from 80% interval (z ≈ 1.28)
  z_80 <- qnorm(0.9)
  sd_est <- (stl.forecasts$upper[, 1] - stl.forecasts$lower[, 1]) / (2 * z_80)

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

# Run models for each regressor
results_original <- run_model(secchi_data_quinn$Temp_C_mean.x, "Water Temperature")
results_chla     <- run_model(secchi_data_quinn$Chla_ugL_mean.x, "Chlorophyll-a Concentration")
results_bloom    <- run_model(secchi_data_quinn$Bloom_binary_mean.x, "Mean Binary Presence of Algal Blooms")
results_rain     <- run_model(secchi_data_quinn$Rain_mm_sum.x, "Rainfall")

# Combine forecasts into one dataframe for plotting
forecast_comparison <- bind_rows(
  results_original$df,
  results_chla$df,
  results_bloom$df,
  results_rain$df
)

# Plot all model forecasts vs observed
ggplot(forecast_comparison, aes(x = time)) +
  geom_line(aes(y = observed), color = "black", linewidth = 1) +
  geom_line(aes(y = forecast, color = model), linewidth = 1) +
  geom_point(aes(y = forecast, color = model)) +
  labs(
    title = "Forecast Comparison by Regressor",
    y = "Secchi Depth (m)",
    x = "Time (Days)",
    color = "Regressor"
  ) +
  theme_minimal()

# Table of RMSE, MAE, CRPS for each model
comparison_metrics <- data.frame(
  Model = c("Water Temperature", "Chlorophyll-a Concentration", "Mean Binary Presence of Algal Blooms", "Rainfall"),
  RMSE = c(results_original$rmse, results_chla$rmse, results_bloom$rmse, results_rain$rmse),
  MAE = c(results_original$mae, results_chla$mae, results_bloom$mae, results_rain$mae),
  CRPS = c(results_original$crps, results_chla$crps, results_bloom$crps, results_rain$crps)
)

print(comparison_metrics)
