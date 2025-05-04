library(forecast)
library(Metrics)

# Ensure data is complete for Secchi and Rain
ccf_data <- fcre_smoothed %>%
  select(datetime, Secchi_m, Rain_mm_sum) %>%
  filter(!is.na(Secchi_m), !is.na(Rain_mm_sum))

# Calculate cross-correlation
ccf_res <- ccf(ccf_data$Rain_mm_sum, ccf_data$Secchi_m, lag.max = 30, plot = TRUE)

# Forecast lag

secchi_data <- fcre_smoothed %>%
  select(datetime, Secchi_m, Rain_mm_sum, AirTemp_C_mean) %>%
  mutate(Rain_lag23 = lag(Rain_mm_sum, 23)) %>%
  filter(!is.na(Secchi_m), !is.na(Rain_lag23), !is.na(AirTemp_C_mean))


n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

train <- secchi_data[1:trainN, ]
test  <- secchi_data[testN:n, ]

ts.train <- ts(train$Secchi_m, frequency = 7)
xreg_train <- as.matrix(train %>% select(Rain_lag23, AirTemp_C_mean))
xreg_test  <- as.matrix(test %>% select(Rain_lag23, AirTemp_C_mean))

fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = 30, newxreg = xreg_test)

z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)


forecast_df <- tibble(
  date = test$datetime,
  observed = test$Secchi_m,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

ggplot(forecast_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6bddb", alpha = 0.4) +
  geom_line(aes(y = observed), color = "black", size = 1.2) +
  geom_line(aes(y = forecast), color = "#0570b0", size = 1.2, linetype = "dashed") +
  geom_point(aes(y = forecast), color = "#0570b0", size = 2) +
  labs(
    title = "30-Day Forecast of Secchi Depth",
    subtitle = "Model: Rain (lag 23) + Air Temp",
    x = "Date", y = "Secchi Depth (m)"
  ) +
  theme_minimal(base_size = 14)

# Print metrics
metrics <- data.frame(
  Model = "Rain_lag23 + AirTemp",
  RMSE = rmse(test$Secchi_m, fc$mean),
  MAE = mae(test$Secchi_m, fc$mean),
  CRPS = mean(crps_norm(test$Secchi_m, fc$mean, sd_est), na.rm = TRUE)
)

print(metrics)




