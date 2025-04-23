# Core time series packages
library(forecast)
library(tidyverse)
library(Metrics)
library(dplyr)

# Filter and join
fcre_secchi_data <- fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013")

secchi_data <- fcre_smoothed %>%
  left_join(fcreData_WaterTemp %>% select(datetime, Temp_C_mean),
            by = "datetime") %>%
  left_join(fcre_secchi_data %>%
              select(datetime, Chla_ugL_mean, Bloom_binary_mean, Rain_mm_sum),
            by = "datetime")

# Clean and create variables
secchi_data <- secchi_data %>%
  mutate(
    Temp_C_mean_log = log1p(Temp_C_mean.x)  # log(1 + x) to handle 0s safely
  ) %>%
  drop_na(Secchi_m, Temp_C_mean_log, Bloom_binary_mean.x)  # drop rows with NAs

# Set training and testing periods
n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

train <- secchi_data[1:trainN, ]
test <- secchi_data[testN:n, ]
ts.train <- ts(train$Secchi_m, frequency = 365)

# Multivariate regressor matrix using log-transformed temperature
xreg_all <- as.matrix(secchi_data %>%
                        select(Temp_C_mean_log, Bloom_binary_mean.x))

xreg_train <- xreg_all[1:trainN, ]
xreg_test <- xreg_all[testN:n, ]

# Fit STL + ARIMA with log temp and bloom as regressors
stl.fit.multi <- stlm(ts.train, s.window = "periodic",
                      method = "arima", xreg = xreg_train)

# Forecast
stl.forecast.multi <- forecast(stl.fit.multi, h = 30, newxreg = xreg_test)

# Make comparison dataframe
multi_forecast_df <- data.frame(
  time = 1:30,
  observed = test$Secchi_m,
  forecast = stl.forecast.multi$mean
)

# Long format for plotting
multi_forecast_df_long <- multi_forecast_df %>%
  pivot_longer(cols = c(observed, forecast),
               names_to = "type",
               values_to = "Secchi")

# Plot
ggplot(multi_forecast_df_long, aes(x = time, y = Secchi, color = type)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(
    values = c("observed" = "blue", "forecast" = "darkred"),
    labels = c("Observed", "Forecast"),
    name = "Legend"
  ) +
  labs(
    title = "Multivariate STL + ARIMA Forecast (Log Temp + Bloom)",
    y = "Secchi Depth",
    x = "Time (Days)"
  ) +
  theme_minimal()

# Accuracy metrics
rmse_multi <- rmse(multi_forecast_df$observed, multi_forecast_df$forecast)
mae_multi <- mae(multi_forecast_df$observed, multi_forecast_df$forecast)

cat("Multivariate STL + ARIMA with log(Temp_C_mean) and Bloom_binary_mean\n")
cat("RMSE:", round(rmse_multi, 4), "\n")
cat("MAE: ", round(mae_multi, 4), "\n")
