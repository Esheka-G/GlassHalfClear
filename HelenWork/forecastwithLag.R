# Load required libraries
library(forecast)
library(tidyverse)
library(Metrics)
library(scoringRules)

# --- STEP 1: Prepare dataset with lagged predictors ---
secchi_data <- fcre_smoothed %>%
  select(datetime, Secchi_m, Chla_ugL_mean, Bloom_binary_mean) %>%
  mutate(
    Chla_lag2 = lag(Chla_ugL_mean, 2),
    Bloom_lag1 = lag(Bloom_binary_mean, 1)
  ) %>%
  filter(!is.na(Secchi_m), !is.na(Chla_lag2), !is.na(Bloom_lag1))

# --- STEP 2: Split into training and test data ---
n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

train <- secchi_data[1:trainN, ]
test  <- secchi_data[testN:n, ]

# Time series of Secchi depth
ts.train <- ts(train$Secchi_m, frequency = 7)

# Regressor matrices using best lags
xreg_train <- as.matrix(train %>% select(Chla_lag2, Bloom_lag1))
xreg_test  <- as.matrix(test %>% select(Chla_lag2, Bloom_lag1))

# --- STEP 3: Fit STL + ARIMA and forecast 30 days ---
fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc  <- forecast(fit, h = 30, newxreg = xreg_test)

# Estimate forecast standard deviation (from 80% CI)
z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

# --- STEP 4: Build forecast output dataframe ---
forecast_df <- tibble(
  date = test$datetime,
  observed = test$Secchi_m,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

# --- STEP 5: Plot forecast ---
ggplot(forecast_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6bddb", alpha = 0.4) +
  geom_line(aes(y = observed), color = "black", size = 1.2) +
  geom_line(aes(y = forecast), color = "#0570b0", size = 1.2, linetype = "dashed") +
  geom_point(aes(y = forecast), color = "#0570b0", size = 2) +
  labs(
    title = "30-Day Forecast of Secchi Depth",
    subtitle = "Model: Chla (lag 2) + Bloom (lag 1)",
    x = "Date", y = "Secchi Depth (m)",
    caption = "Dashed = Forecast | Shaded = 80% Confidence Interval"
  ) +
  theme_minimal(base_size = 14)

# --- STEP 6: Print performance metrics ---
metrics <- data.frame(
  Model = "Chla_lag2 + Bloom_lag1",
  RMSE = rmse(test$Secchi_m, fc$mean),
  MAE = mae(test$Secchi_m, fc$mean),
  CRPS = mean(crps_norm(test$Secchi_m, fc$mean, sd_est), na.rm = TRUE)
)

print(metrics)
