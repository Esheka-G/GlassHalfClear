# Load required libraries
library(tidyverse)
library(forecast)
library(Metrics)
library(scoringRules)
library(zoo)

# === 1. Interpolation function ===
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
    arrange(datetime) %>%
    mutate(across(all_of(numeric_vars), ~ na.approx(., na.rm = FALSE, rule = 2)))

  daily_grid$site_id <- site_label
  return(daily_grid)
}

# === 2. Prepare smoothed data and lags ===
fcre_smoothed <- interpolate_daily(fcre_Combined, "fcre")

smoothed_data <- fcre_smoothed %>%
  select(datetime, Secchi_m, Rain_mm_sum, AirTemp_C_mean) %>%
  mutate(
    Rain_lag23 = lag(Rain_mm_sum, 23),
    AirTemp_lag0 = AirTemp_C_mean
  ) %>%
  filter(!is.na(Secchi_m), !is.na(Rain_lag23), !is.na(AirTemp_lag0))

# === 3. Forecast setup ===
n <- nrow(smoothed_data)
trainN <- n - 30
testN <- trainN + 1

train <- smoothed_data[1:trainN, ]
test  <- smoothed_data[testN:n, ]

ts.train <- ts(train$Secchi_m, frequency = 365)
xreg_train <- as.matrix(train %>% select(Rain_lag23, AirTemp_lag0))
xreg_test  <- as.matrix(test %>% select(Rain_lag23, AirTemp_lag0))

# === 4. Fit and forecast ===
fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = 30, newxreg = xreg_test)

z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

# === 5. Forecast dataframe with smoothed Secchi comparison ===
forecast_df <- tibble(
  datetime = test$datetime,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

# Join with smoothed Secchi for comparison
comparison_df <- forecast_df %>%
  left_join(fcre_smoothed %>% select(datetime, Secchi_smoothed = Secchi_m), by = "datetime") %>%
  filter(!is.na(Secchi_smoothed))

# === 6. Plot forecast vs smoothed Secchi ===
ggplot(comparison_df, aes(x = datetime)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6d854", alpha = 0.3) +
  geom_line(aes(y = forecast), color = "#33a02c", linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(y = forecast), color = "#33a02c", size = 2) +
  geom_line(aes(y = Secchi_smoothed), color = "blue", linewidth = 1.2) +
  geom_point(aes(y = Secchi_smoothed), color = "blue", size = 2) +
  labs(
    title = "30-Day Forecast vs Smoothed Secchi (fcre)",
    subtitle = "Forecast (dashed green) vs Smoothed Secchi (blue)",
    x = "Date", y = "Secchi Depth (m)"
  ) +
  theme_minimal(base_size = 14)

# === 7. Metrics comparing forecast vs smoothed ===
metrics <- tibble(
  Model = "STL + ARIMA (Rain_lag23 + AirTemp)",
  RMSE = rmse(comparison_df$Secchi_smoothed, comparison_df$forecast),
  MAE = mae(comparison_df$Secchi_smoothed, comparison_df$forecast),
  CRPS = mean(crps_norm(comparison_df$Secchi_smoothed, comparison_df$forecast, sd_est), na.rm = TRUE)
)

print(metrics)
