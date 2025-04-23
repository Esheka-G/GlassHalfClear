# Core time series packages
library(forecast)
library(tidyverse)
library(Metrics)
library(dplyr)

# Filter out year 2013
fcre_secchi_data <- fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013")

# Join datasets
secchi_data <- fcre_smoothed %>%
  left_join(fcreData_WaterTemp %>% select(datetime, Temp_C_mean), by = "datetime") %>%
  left_join(fcre_secchi_data %>%
              select(datetime, Chla_ugL_mean, Bloom_binary_mean, Rain_mm_sum),
            by = "datetime")

# Create transformed variables and drop NAs
secchi_data <- secchi_data %>%
  mutate(
    Temp_C_mean_sq = Temp_C_mean.x^2,
    Temp_C_mean_log = log1p(Temp_C_mean.x),
    interaction = Temp_C_mean.x * Bloom_binary_mean.x
  ) %>%
  drop_na(Secchi_m, Temp_C_mean.x, Bloom_binary_mean.x, interaction)

# Set training and testing periods
n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

train <- secchi_data[1:trainN, ]
test <- secchi_data[testN:n, ]
ts.train <- ts(train$Secchi_m, frequency = 365)

# Regressor matrix
xreg_all <- as.matrix(secchi_data %>%
                        select(Temp_C_mean.x, Bloom_binary_mean.x, interaction))

xreg_train <- xreg_all[1:trainN, ]
xreg_test <- xreg_all[testN:n, ]

# Fit STL + ARIMA
stl.fit.int <- stlm(ts.train, s.window = "periodic",
                    method = "arima", xreg = xreg_train)

# Forecast
stl.forecast.int <- forecast(stl.fit.int, h = 30, newxreg = xreg_test)

# Compare
forecast_df_int <- data.frame(
  time = 1:30,
  observed = test$Secchi_m,
  forecast = stl.forecast.int$mean
)

forecast_long_int <- forecast_df_int %>%
  pivot_longer(cols = c(observed, forecast), names_to = "type", values_to = "Secchi")

ggplot(forecast_long_int, aes(x = time, y = Secchi, color = type)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(values = c("observed" = "blue", "forecast" = "darkred"),
                     labels = c("Observed", "Forecast"), name = "Legend") +
  labs(title = "STL + ARIMA with Temp, Bloom, and Interaction",
       y = "Secchi Depth", x = "Time (Days)") +
  theme_minimal()

cat("RMSE:", round(rmse(forecast_df_int$observed, forecast_df_int$forecast), 4), "\n")
cat("MAE: ", round(mae(forecast_df_int$observed, forecast_df_int$forecast), 4), "\n")
