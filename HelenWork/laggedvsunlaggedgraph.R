library(forecast)
library(scoringRules)
library(Metrics)
library(ggplot2)

# Filter usable data with Secchi observations
secchi_data <- fcre_smoothed %>%
  filter(!is.na(Secchi_m), !is.na(AirTemp_C_mean.x), !is.na(Rain_mm_sum.x)) %>%
  mutate(
    AirTemp_lag15 = lag(AirTemp_C_mean.x, 15),
    Rain_lag6 = lag(Rain_mm_sum.x, 6)
  ) %>%
  filter(!is.na(AirTemp_lag15), !is.na(Rain_lag6))

# Split into training (all but last 30) and test (last 30)
n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

train <- secchi_data[1:trainN, ]
test <- secchi_data[testN:n, ]
ts_train <- ts(train$Secchi_m, frequency = 365)

# Helper function for fitting model and generating forecast
run_model <- function(xreg_all, test_y, regressor_names, model_label) {
  xreg_train <- as.matrix(xreg_all[1:trainN, ])
  xreg_test <- as.matrix(xreg_all[testN:n, ])

  fit <- stlm(ts_train, s.window = "periodic", method = "arima", xreg = xreg_train)
  fc <- forecast(fit, h = 30, newxreg = xreg_test)

  # Estimate standard deviation from 80% interval
  z_80 <- qnorm(0.9)
  sd_est <- (fc$upper[,1] - fc$lower[,1]) / (2 * z_80)

  # Metrics
  rmse_val <- rmse(test_y, fc$mean)
  mae_val <- mae(test_y, fc$mean)
  crps_val <- mean(crps_norm(y = test_y, location = fc$mean, scale = sd_est), na.rm = TRUE)

  forecast_df <- tibble(
    datetime = test$datetime,
    observed = test_y,
    forecast = as.numeric(fc$mean),
    model = model_label
  )

  return(list(df = forecast_df, rmse = rmse_val, mae = mae_val, crps = crps_val))
}

# Run unlagged model
xreg_unlagged <- secchi_data %>%
  select(AirTemp_C_mean.x, Rain_mm_sum.x)
results_unlagged <- run_model(xreg_unlagged, test$Secchi_m, c("AirTemp_C_mean.x", "Rain_mm_sum"), "Unlagged")

# Run lagged model
xreg_lagged <- secchi_data %>%
  select(AirTemp_lag15, Rain_lag6)
results_lagged <- run_model(xreg_lagged, test$Secchi_m, c("AirTemp_lag15", "Rain_lag6"), "Lagged")

# Combine forecasts
forecast_comparison <- bind_rows(results_unlagged$df, results_lagged$df)

ggplot(forecast_comparison, aes(x = datetime)) +
  geom_line(aes(y = observed, color = "Observed"), size = 1.2) +  # Add to legend
  geom_line(aes(y = forecast, color = model), size = 1) +
  geom_point(aes(y = forecast, color = model), size = 2) +
  scale_color_manual(values = c(
    "Observed" = "black",
    "Lagged" = "blue",
    "Unlagged" = "red"
  )) +
  labs(
    title = "30-Day Forecast of Secchi Depth",
    subtitle = "Lagged vs Unlagged Regressors",
    y = "Secchi Depth (m)", x = "Date",
    color = "Legend"
  ) +
  theme_minimal(base_size = 14)

# Print performance
metrics <- tibble(
  Model = c("Unlagged", "Lagged"),
  RMSE = c(results_unlagged$rmse, results_lagged$rmse),
  MAE = c(results_unlagged$mae, results_lagged$mae),
  CRPS = c(results_unlagged$crps, results_lagged$crps)
)
print(metrics)
