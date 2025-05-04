library(tidyverse)
library(zoo)
library(lubridate)
library(forecast)
library(Metrics)

# ---- Step 1: Create interpolation function ----
interpolate_daily <- function(df, site_label) {
  df <- df %>%
    mutate(datetime = as.Date(datetime)) %>%
    arrange(datetime)

  numeric_vars <- df %>%
    select(where(is.numeric)) %>%
    select(-any_of("depth_m")) %>%
    names()

  daily_grid <- tibble(datetime = seq.Date(min(df$datetime), max(df$datetime), by = "day")) %>%
    left_join(df %>% select(datetime, all_of(numeric_vars)), by = "datetime") %>%
    arrange(datetime) %>%
    mutate(across(all_of(numeric_vars), ~ na.approx(., na.rm = FALSE, rule = 2)))

  daily_grid$site_id <- site_label
  return(daily_grid)
}

# ---- Step 2: Load & prepare historical data ----
# Assuming you've already created fcre_Combined from your original code
fcre_smoothed <- interpolate_daily(fcre_Combined, "fcre")

# ---- Step 3: Get NOAA future weather forecast ----
start_forecast_date <- format(Sys.Date() - 1, "%Y-%m-%d")
weather <- vera4castHelpers::noaa_stage2(start_date = start_forecast_date)
df_future <- weather %>% filter(site_id == "fcre") %>% collect()

# ---- Step 4: Aggregate noon forecasts ----
agg_noon <- function(.data, value_col, fun = mean) {
  .data %>%
    filter(hour(datetime) == 12, minute(datetime) == 0) %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(date) %>%
    summarise(value = fun({{ value_col }}, na.rm = TRUE), .groups = "drop")
}

airtemp_noon <- df_future %>%
  filter(variable == "air_temperature") %>%
  mutate(pred_C = prediction - 273.15) %>%
  agg_noon(pred_C) %>%
  rename(AirTemp_C_mean = value)

precip_noon <- df_future %>%
  filter(variable == "precipitation_flux",
         hour(datetime) == 12, minute(datetime) == 0) %>%
  mutate(date = as.Date(datetime),
         flux = prediction,
         precip_mm = flux * 86400) %>%
  group_by(date) %>%
  slice(1) %>%
  ungroup() %>%
  select(date, precip_mm) %>%
  rename(Rain_mm_sum = precip_mm)

# ---- Step 5: Combine future meteorology and compute lagged predictor ----
noon_future_regs <- airtemp_noon %>%
  left_join(precip_noon, by = "date") %>%
  rename(datetime = date)

all_meteo <- fcre_smoothed %>%
  select(datetime, Rain_mm_sum, AirTemp_C_mean) %>%
  bind_rows(noon_future_regs) %>%
  arrange(datetime) %>%
  mutate(Rain_lag23 = lag(Rain_mm_sum, 23))

future_predictors <- all_meteo %>%
  filter(datetime %in% noon_future_regs$datetime) %>%
  filter(!is.na(Rain_lag23), !is.na(AirTemp_C_mean))

# ---- Step 6: Build training dataset for Secchi ----
secchi_data <- fcre_smoothed %>%
  select(datetime, Secchi_m, Rain_mm_sum, AirTemp_C_mean) %>%
  mutate(Rain_lag23 = lag(Rain_mm_sum, 23)) %>%
  filter(!is.na(Secchi_m), !is.na(Rain_lag23), !is.na(AirTemp_C_mean))

# Training/testing split (last 30 days for testing)
n <- nrow(secchi_data)
trainN <- n - 30
train <- secchi_data[1:trainN, ]

ts.train <- ts(train$Secchi_m, frequency = 7)
xreg_train <- as.matrix(train %>% select(Rain_lag23, AirTemp_C_mean))

# ---- Step 7: Forecast future Secchi ----
future_xreg <- as.matrix(future_predictors %>% select(Rain_lag23, AirTemp_C_mean))

fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = nrow(future_predictors), newxreg = future_xreg)

z_80 <- qnorm(0.9)
forecast_df <- tibble(
  date = future_predictors$datetime,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

# ---- Step 8: Plot the forecast ----
ggplot(forecast_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6bddb", alpha = 0.4) +
  geom_line(aes(y = forecast), color = "#0570b0", size = 1.2, linetype = "dashed") +
  geom_point(aes(y = forecast), color = "#0570b0", size = 2) +
  labs(
    title = "30-Day Forecast of Secchi Depth",
    subtitle = "Model: Rain (lag 23) + Air Temp",
    x = "Date", y = "Secchi Depth (m)"
  ) +
  theme_minimal(base_size = 14)
