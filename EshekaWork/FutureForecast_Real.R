library(forecast)
library(tidyverse)
library(Metrics)
library(scoringRules)

# --- Step 1: Prepare original (unsmoothed) data with lags ---
secchi_data <- fcre_Combined %>%
  arrange(datetime) %>%
  mutate(
    Chla_lag2 = lag(Chla_ugL_mean, 2),
    Bloom_lag1 = lag(Bloom_binary_mean, 1)
  ) %>%
  filter(!is.na(Secchi_m), !is.na(Chla_lag2), !is.na(Bloom_lag1))

secchi_mod <- secchi_data



# --- Step 2: Split into training and testing sets ---
n <- nrow(secchi_mod)

secchi_data$Temp_C_mean[testN:n] <- airtemp_noon$air_temp_C[1:30]


trainN <- n - 30
testN <- trainN + 1

train <- secchi_mod[1:trainN, ]
test  <- secchi_mod[testN:n,]

ts.train <- ts(train$Secchi_m, frequency = 7)  # Weekly seasonality for short series

# --- Step 3: Prepare regressors ---
xreg_train <- as.matrix(train %>% select(Chla_lag2, Bloom_lag1))
xreg_test  <- as.matrix(test  %>% select(Chla_lag2, Bloom_lag1))

# --- Step 4: Fit STL+ARIMA model and forecast ---
fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = 30, newxreg = xreg_test)

# Estimate standard deviation from 80% interval
z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

start_forecast_date <-Sys.Date() - 1

future_dates <- seq.Date(start_forecast_date, by = "day", length.out = 30)
future_dates

# --- Step 5: Plot forecast ---
forecast_df <- tibble(
  date = future_dates,
  observed = test$Secchi_m,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

ggplot(forecast_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6bddb", alpha = 0.4) +
  geom_line(aes(y = forecast), color = "#0570b0", size = 1.2, linetype = "dashed") +
  geom_point(aes(y = forecast), color = "#0570b0", size = 2) +
  labs(
    title = "30-Day Forecast of Secchi Depth (Original Data)",
    subtitle = "Model: Chla (lag 2) + Bloom (lag 1)",
    x = "Date", y = "Secchi Depth (m)",
    caption = "Dashed = Forecast | Shaded = 80% Confidence Interval"
  ) +
  theme_minimal(base_size = 14)

# # --- Step 6: Calculate performance metrics ---
# metrics <- data.frame(
#   Model = "Chla_lag2 + Bloom_lag1",
#   RMSE = rmse(test$Secchi_m, fc$mean),
#   MAE = mae(test$Secchi_m, fc$mean),
#   CRPS = mean(crps_norm(test$Secchi_m, fc$mean, sd_est), na.rm = TRUE)
# )
#
# print(metrics)
