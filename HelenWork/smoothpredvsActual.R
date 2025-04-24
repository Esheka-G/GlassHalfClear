# Load libraries
library(tidyverse)
library(forecast)
library(Metrics)
library(lubridate)

# STEP 1: Get real, raw Secchi data (not smoothed)
real_secchi <- targets %>%
  filter(site_id == "fcre", variable == "Secchi_m_sample") %>%
  mutate(datetime = as.Date(datetime)) %>%
  select(datetime, Secchi_m_actual = observation) %>%
  arrange(datetime)

# STEP 2: Prepare your smoothed data for forecasting (exclude 2024+ for training)
secchi_data <- fcre_smoothed %>%
  filter(year(datetime) < 2024)

# Forecast horizon
horizon <- 60  # feel free to go longer or shorter

# Train/test split
n <- nrow(secchi_data)
train <- secchi_data[1:(n - horizon), ]
ts.train <- ts(train$Secchi_m, frequency = 365)

# STEP 3: Fit STL model on smoothed data
stl.fit <- stlm(ts.train, s.window = "periodic", method = "arima")
stl.forecasts <- forecast(stl.fit, h = horizon)

# Build forecast dataframe
forecast_df <- data.frame(
  datetime = seq.Date(from = max(train$datetime) + 1, by = "day", length.out = horizon),
  Secchi_m_pred = stl.forecasts$mean
)

# STEP 4: Join forecast with actual (raw) Secchi observations
forecast_vs_actual <- forecast_df %>%
  left_join(real_secchi, by = "datetime") %>%
  filter(!is.na(Secchi_m_actual))  # keep only days with actual data

# STEP 5: Plot forecast vs raw observations
ggplot(forecast_vs_actual, aes(x = datetime)) +
  geom_line(aes(y = Secchi_m_pred), color = "red", linetype = "dashed", size = 1.2) +
  geom_point(aes(y = Secchi_m_pred), color = "red") +
  geom_line(aes(y = Secchi_m_actual), color = "blue", size = 1.2) +
  geom_point(aes(y = Secchi_m_actual), color = "blue") +
  labs(title = "Forecast (from Smoothed) vs Actual Raw Secchi Observations",
       y = "Secchi (m)", x = "Date") +
  theme_minimal()

# STEP 6: Metrics comparing forecast to real observations
stl.rmse <- Metrics::rmse(forecast_vs_actual$Secchi_m_actual, forecast_vs_actual$Secchi_m_pred)
stl.mae <- Metrics::mae(forecast_vs_actual$Secchi_m_actual, forecast_vs_actual$Secchi_m_pred)

comparison_metrics <- data.frame(
  Model = "STL on Smoothed vs Real Data",
  RMSE = stl.rmse,
  MAE = stl.mae
)

print(comparison_metrics)



ggplot(forecast_vs_actual, aes(x = datetime)) +
  geom_line(aes(y = Secchi_m_pred, color = "Forecast (Smoothed)"), linetype = "dashed", size = 1.2) +
  geom_point(aes(y = Secchi_m_pred, color = "Forecast (Smoothed)")) +
  geom_line(aes(y = Secchi_m_actual, color = "Actual (Raw)"), size = 1.2) +
  geom_point(aes(y = Secchi_m_actual, color = "Actual (Raw)")) +
  scale_color_manual(name = "Legend", values = c("Forecast (Smoothed)" = "red", "Actual (Raw)" = "blue")) +
  labs(title = "Forecast (Smoothed) vs Actual Raw Secchi Observations",
       y = "Secchi Depth (m)", x = "Date") +
  theme_minimal() +
  theme(legend.position = "bottom")

