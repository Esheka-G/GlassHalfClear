library(tidyverse)
library(forecast)
library(Metrics)
library(scoringRules)
library(zoo)

# === Helper: Interpolate smoothed data ===
interpolate_daily <- function(df, site_label) {
  df <- df %>%
    mutate(datetime = as.Date(datetime)) %>%
    arrange(datetime)

  numeric_vars <- df %>%
    select(where(is.numeric)) %>%
    select(-any_of("depth_m")) %>%
    names()

  daily_grid <- tibble(datetime = seq.Date(min(df$datetime), max(df$datetime), by = "day"))

  daily_grid <- daily_grid %>%
    left_join(df %>% select(datetime, all_of(numeric_vars)), by = "datetime") %>%
    arrange(datetime)

  daily_grid <- daily_grid %>%
    mutate(across(all_of(numeric_vars), ~ na.approx(., na.rm = FALSE, rule = 2)))

  daily_grid$site_id <- site_label
  return(daily_grid)
}

# === Assume fcre_Combined is already created and includes: Secchi_m, Rain_mm_sum, AirTemp_C_mean ===
# Create smoothed version
fcre_smoothed <- interpolate_daily(fcre_Combined, "fcre")

# === Prepare forecast data ===
prep_forecast_data <- function(df) {
  df %>%
    select(datetime, Secchi_m, Rain_mm_sum, AirTemp_C_mean) %>%
    mutate(Rain_lag23 = lag(Rain_mm_sum, 23)) %>%
    filter(!is.na(Secchi_m), !is.na(Rain_lag23), !is.na(AirTemp_C_mean))
}

raw_data <- prep_forecast_data(fcre_Combined)
smoothed_data <- prep_forecast_data(fcre_smoothed)

# === Forecast function ===
forecast_model <- function(df, model_label) {
  n <- nrow(df)
  trainN <- n - 30
  testN <- trainN + 1

  train <- df[1:trainN, ]
  test  <- df[testN:n, ]

  ts.train <- ts(train$Secchi_m, frequency = 7)
  xreg_train <- as.matrix(train %>% select(Rain_lag23, AirTemp_C_mean))
  xreg_test  <- as.matrix(test %>% select(Rain_lag23, AirTemp_C_mean))

  fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
  fc <- forecast(fit, h = 30, newxreg = xreg_test)

  z_80 <- qnorm(0.9)
  sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

  forecast_df <- tibble(
    date = test$datetime,
    observed = test$Secchi_m,
    forecast = as.numeric(fc$mean),
    lower = as.numeric(fc$lower[, 1]),
    upper = as.numeric(fc$upper[, 1]),
    Model = model_label
  )

  metrics <- tibble(
    Model = model_label,
    RMSE = rmse(test$Secchi_m, fc$mean),
    MAE = mae(test$Secchi_m, fc$mean),
    CRPS = mean(crps_norm(test$Secchi_m, fc$mean, sd_est), na.rm = TRUE)
  )

  list(df = forecast_df, metrics = metrics)
}

# === Run models ===
raw_result <- forecast_model(raw_data, "Raw")
smoothed_result <- forecast_model(smoothed_data, "Smoothed")

raw_forecast_df <- raw_result$df
smoothed_forecast_df <- smoothed_result$df

combined_forecast <- bind_rows(raw_forecast_df, smoothed_forecast_df)

# === Filter to matching 30-day forecast window ===
forecast_window <- range(smoothed_forecast_df$date)

combined_forecast_filtered <- combined_forecast %>%
  filter(date >= forecast_window[1] & date <= forecast_window[2])

# === Plot ===
ggplot(combined_forecast_filtered, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Model), alpha = 0.3) +
  geom_line(aes(y = observed), color = "black", linewidth = 1.2) +
  geom_line(aes(y = forecast, color = Model), linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(y = forecast, color = Model), size = 2) +
  labs(
    title = "30-Day Secchi Depth Forecast",
    subtitle = "Comparison of Raw vs. Smoothed (Rain_lag23 + AirTemp)",
    x = "Date", y = "Secchi Depth (m)"
  ) +
  scale_color_manual(values = c("Raw" = "#1f78b4", "Smoothed" = "#33a02c")) +
  scale_fill_manual(values = c("Raw" = "#1f78b4", "Smoothed" = "#33a02c")) +
  theme_minimal(base_size = 14)

# === Print metrics ===
combined_metrics <- bind_rows(raw_result$metrics, smoothed_result$metrics)
print(combined_metrics)
