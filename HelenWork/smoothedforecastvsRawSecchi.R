# Load libraries
library(tidyverse)
library(forecast)
library(Metrics)
library(scoringRules)
library(zoo)

# === 1. Interpolation function for smoothed dataset ===
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

# === 2. Smooth the raw dataset ===
fcre_smoothed <- interpolate_daily(fcre_Combined, "fcre")

# === 3. Prepare smoothed dataset with lagged precipitation ===
smoothed_data <- fcre_smoothed %>%
  select(datetime, Secchi_m, Rain_mm_sum, AirTemp_C_mean) %>%
  mutate(Rain_lag23 = lag(Rain_mm_sum, 23)) %>%
  filter(!is.na(Secchi_m), !is.na(Rain_lag23), !is.na(AirTemp_C_mean))

# === 4. Get original raw Secchi observations ===
raw_obs <- fcre_Combined %>%
  select(datetime, Secchi_m) %>%
  filter(!is.na(Secchi_m)) %>%
  rename(observed_raw = Secchi_m)

# === 5. Forecast using smoothed data ===
n <- nrow(smoothed_data)
trainN <- n - 30
testN <- trainN + 1

train <- smoothed_data[1:trainN, ]
test  <- smoothed_data[testN:n, ]

ts.train <- ts(train$Secchi_m, frequency = 7)
xreg_train <- as.matrix(train %>% select(Rain_lag23, AirTemp_C_mean))
xreg_test  <- as.matrix(test %>% select(Rain_lag23, AirTemp_C_mean))

fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = 30, newxreg = xreg_test)

z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

# === 6. Build forecast + join raw obs for plotting ===
forecast_df <- tibble(
  date = test$datetime,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

plot_df <- forecast_df %>%
  left_join(raw_obs, by = c("date" = "datetime")) %>%
  filter(!is.na(observed_raw))  # Drop rows without raw observations

# === 7. Plot ===
ggplot(plot_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6d854", alpha = 0.3) +
  geom_line(aes(y = observed_raw), color = "black", linewidth = 1.2) +
  geom_line(aes(y = forecast), color = "#33a02c", linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(y = forecast), color = "#33a02c", size = 2) +
  labs(
    title = "30-Day Forecast (Smoothed Model vs. Raw Observed Secchi)",
    subtitle = "Model input: Smoothed data | Black = raw observed Secchi",
    x = "Date", y = "Secchi Depth (m)"
  ) +
  theme_minimal(base_size = 14)

# === 8. Performance metrics ===
metrics <- tibble(
  Model = "Smoothed forecast vs Raw observed",
  RMSE = rmse(plot_df$observed_raw, plot_df$forecast),
  MAE = mae(plot_df$observed_raw, plot_df$forecast),
  CRPS = mean(crps_norm(plot_df$observed_raw, plot_df$forecast, sd_est), na.rm = TRUE)
)

print(metrics)
