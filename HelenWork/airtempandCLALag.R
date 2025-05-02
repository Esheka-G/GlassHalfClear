library(forecast)
library(tidyverse)
library(Metrics)
library(scoringRules)

# --- Step 1: Create lagged + unlagged predictor set ---
secchi_data <- fcre_smoothed %>%
  left_join(
    met_targets %>%
      filter(variable == "AirTemp_C_mean") %>%
      rename(AirTemp = observation) %>%
      select(datetime, AirTemp),
    by = "datetime"
  ) %>%
  mutate(
    Chla_lag2 = lag(Chla_ugL_mean, 2)
  ) %>%
  filter(!is.na(Secchi_m), !is.na(Chla_lag2), !is.na(AirTemp))

# --- Step 2: Train/Test split ---
n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

train <- secchi_data[1:trainN, ]
test  <- secchi_data[testN:n, ]

ts.train <- ts(train$Secchi_m, frequency = 365)

# --- Step 3: Regressor matrices ---
xreg_train <- as.matrix(train %>% select(Chla_lag2, AirTemp))
xreg_test  <- as.matrix(test  %>% select(Chla_lag2, AirTemp))

# --- Step 4: Fit and Forecast ---
fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = 30, newxreg = xreg_test)

# Estimate uncertainty
z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

# --- Step 5: Build forecast frame ---
forecast_df <- tibble(
  date = test$datetime,
  observed = test$Secchi_m,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

# --- Step 6: Plot (AirTemp-style) ---
ggplot(forecast_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6bddb", alpha = 0.4) +
  geom_line(aes(y = observed), color = "black", linewidth = 1.2) +
  geom_line(aes(y = forecast), color = "#0570b0", linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = forecast), color = "#0570b0", size = 2) +
  labs(
    title = "30-Day Secchi Forecast: Chla (lag 2) + AirTemp",
    subtitle = "STL + ARIMA model with lagged and current predictors",
    x = "Date", y = "Secchi Depth (m)",
    caption = "Dashed = Forecast | Shaded = 80% Confidence Interval"
  ) +
  theme_minimal(base_size = 14)

# --- Step 7: Performance metrics ---
metrics <- data.frame(
  Model = "Chla_lag2 + AirTemp",
  RMSE = rmse(test$Secchi_m, fc$mean),
  MAE = mae(test$Secchi_m, fc$mean),
  CRPS = mean(crps_norm(test$Secchi_m, fc$mean, sd_est), na.rm = TRUE)
)

print(metrics)

