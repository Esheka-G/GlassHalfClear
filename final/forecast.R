# Load libraries
library(tidyverse)
library(forecast)
library(Metrics)
library(scoringRules)
library(zoo)

# === 1. Interpolate daily (smoothing missing dates) ===
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

# === 2. Prepare smoothed data with lags ===
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

ts.train <- ts(train$Secchi_m, frequency = 7)
xreg_train <- as.matrix(train %>% select(Rain_lag23, AirTemp_lag0))
xreg_test  <- as.matrix(test %>% select(Rain_lag23, AirTemp_lag0))

# === 4. Fit and forecast ===
fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = 30, newxreg = xreg_test)

z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

# === 5. Forecast dataframe with dates ===
forecast_df <- tibble(
  datetime = test$datetime,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

# === 6. Join raw Secchi observations for comparison (no filtering) ===
raw_obs <- fcre_Combined %>%
  select(datetime, Secchi_m) %>%
  rename(observed_secchi = Secchi_m)

comparison_df <- forecast_df %>%
  left_join(raw_obs, by = "datetime")  # Keep full forecast period

# === 7. Plot forecast vs raw ===
ggplot(comparison_df, aes(x = datetime)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6d854", alpha = 0.3) +
  geom_line(aes(y = forecast), color = "#33a02c", linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(y = forecast), color = "#33a02c", size = 2) +
  geom_line(aes(y = observed_secchi), color = "black", linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = observed_secchi), color = "black", size = 2, na.rm = TRUE) +
  labs(
    title = "30-Day Forecast vs Raw Secchi (fcre)",
    subtitle = "Model: Rain lag 23 + AirTemp unlagged",
    x = "Date", y = "Secchi Depth (m)"
  ) +
  theme_minimal(base_size = 14)

# === 8. Performance metrics (only on available obs) ===
scored_df <- comparison_df %>%
  filter(!is.na(observed_secchi))

metrics <- tibble(
  Model = "STL + ARIMA (Rain_lag23 + AirTemp)",
  RMSE = rmse(scored_df$observed_secchi, scored_df$forecast),
  MAE = mae(scored_df$observed_secchi, scored_df$forecast),
  CRPS = mean(crps_norm(scored_df$observed_secchi, scored_df$forecast, sd_est[1:nrow(scored_df)]), na.rm = TRUE)
)

print(metrics)

# === Filter to 30-day forecast window only ===
start_date <- min(forecast_df$datetime)
end_date <- max(forecast_df$datetime)

plot_df <- plot_df %>%
  mutate(datetime = as.Date(datetime)) %>%  # <- this line is the fix
  filter(datetime >= start_date & datetime <= end_date)


