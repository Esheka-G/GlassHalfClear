# Core time series packages
library(forecast)
library(tidyverse)
library(Metrics)
library(dplyr)

# Filter and join
fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013") %>%
  count(year)

# Join all relevant variables into one dataset
secchi_data <- fcre_smoothed %>%
  left_join(fcreData_WaterTemp %>% select(datetime, Temp_C_mean), by = "datetime") %>%
  left_join(fcre_secchi_data %>% select(datetime, Chla_ugL_mean, Bloom_binary_mean, Rain_mm_sum), by = "datetime")

# Create transformed variables
secchi_data <- secchi_data %>%
  mutate(
    Temp_C_mean_sq = Temp_C_mean.x^2,
    Temp_C_mean_log = log1p(Temp_C_mean.x)  # Safe log transform
  )

# Remove any rows with missing values in predictors or target
secchi_data <- secchi_data %>%
  drop_na(Secchi_m, Temp_C_mean.x, Temp_C_mean_sq, Temp_C_mean_log,
          Chla_ugL_mean.x, Bloom_binary_mean.x, Rain_mm_sum)

# Split into training and test sets
n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1
train <- secchi_data[1:trainN, ]
test <- secchi_data[testN:n, ]
ts.train <- ts(train$Secchi_m, frequency = 365)

# Function to run model using one regressor
run_model <- function(regressor_all, regressor_name) {
  xreg_train <- matrix(regressor_all[1:trainN], ncol = 1)
  xreg_test <- matrix(regressor_all[testN:n], ncol = 1)

  stl.fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
  stl.forecasts <- forecast(stl.fit, h = 30, newxreg = xreg_test)

  forecast_df <- data.frame(
    time = 1:30,
    observed = test$Secchi_m,
    forecast = stl.forecasts$mean,
    model = regressor_name
  )

  list(
    df = forecast_df,
    rmse = rmse(test$Secchi_m, stl.forecasts$mean),
    mae = mae(test$Secchi_m, stl.forecasts$mean)
  )
}

# Run models for each regressor
results_original <- run_model(secchi_data$Temp_C_mean.x, "Temp_C_mean")
results_squared  <- run_model(secchi_data$Temp_C_mean_sq, "Temp_C_mean_sq")
results_log      <- run_model(secchi_data$Temp_C_mean_log, "Temp_C_mean_log")
results_chla     <- run_model(secchi_data$Chla_ugL_mean.x, "Chla_ugL_mean")
results_bloom    <- run_model(secchi_data$Bloom_binary_mean.x, "Bloom_binary_mean")
results_rain     <- run_model(secchi_data$Rain_mm_sum, "Rain_mm_sum")

# Combine forecasts into one dataframe for plotting
forecast_comparison <- bind_rows(
  results_original$df,
  results_squared$df,
  results_log$df,
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

# Table of RMSE and MAE for each model
comparison_metrics <- data.frame(
  Model = c("Temp_C_mean", "Temp_C_mean_sq", "Temp_C_mean_log",
            "Chla_ugL_mean", "Bloom_binary_mean", "Rain_mm_sum"),
  RMSE = c(results_original$rmse, results_squared$rmse, results_log$rmse,
           results_chla$rmse, results_bloom$rmse, results_rain$rmse),
  MAE = c(results_original$mae, results_squared$mae, results_log$mae,
          results_chla$mae, results_bloom$mae, results_rain$mae)
)

print(comparison_metrics)
