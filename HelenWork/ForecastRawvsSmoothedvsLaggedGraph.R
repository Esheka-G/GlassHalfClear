library(forecast)
library(scoringRules)
library(Metrics)
library(ggplot2)
library(dplyr)
library(tidyr)

# --- Step 1: Prepare raw Secchi (not interpolated) ---
secchi_raw <- fcreData %>%
  filter(variable == "Secchi_m_sample") %>%
  rename(Secchi_m_raw = observation) %>%
  select(datetime, Secchi_m_raw)

# --- Step 2: Create lagged features in the smoothed dataset ---
fcre_lagged <- fcre_smoothed %>%
  mutate(
    AirTemp_lag15 = lag(AirTemp_C_mean.x, 15),
    Rain_lag6 = lag(Rain_mm_sum.x, 6)
  ) %>%
  filter(!is.na(Secchi_m), !is.na(AirTemp_lag15), !is.na(Rain_lag6))

# --- Step 3: Join raw Secchi into lagged dataset ---
fcre_lagged <- fcre_lagged %>%
  left_join(secchi_raw, by = "datetime")

# --- Step 4: Train/test split (last 30 days as test) ---
n <- nrow(fcre_lagged)
trainN <- n - 30
testN <- trainN + 1

train <- fcre_lagged[1:trainN, ]
test <- fcre_lagged[testN:n, ]

ts_train <- ts(train$Secchi_m, frequency = 365)
xreg_train <- as.matrix(train %>% select(AirTemp_lag15, Rain_lag6))
xreg_test <- as.matrix(test %>% select(AirTemp_lag15, Rain_lag6))

# --- Step 5: Forecast with STL + ARIMA + regressors ---
fit <- stlm(ts_train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = 30, newxreg = xreg_test)

z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

# --- Step 6: Compile results ---
results <- tibble(
  datetime = test$datetime,
  forecast = as.numeric(fc$mean),
  secchi_smoothed = test$Secchi_m,
  secchi_raw = test$Secchi_m_raw,
  sd = sd_est
)

# --- Step 7: Performance Metrics ---
metrics <- tibble(
  Target = c("Smoothed Secchi", "Raw Secchi"),
  RMSE = c(rmse(results$secchi_smoothed, results$forecast),
           rmse(results$secchi_raw, results$forecast)),
  MAE = c(mae(results$secchi_smoothed, results$forecast),
          mae(results$secchi_raw, results$forecast)),
  CRPS = c(mean(crps_norm(results$secchi_smoothed, results$forecast, results$sd), na.rm = TRUE),
           mean(crps_norm(results$secchi_raw, results$forecast, results$sd), na.rm = TRUE))
)

print(metrics)

# --- Step 8: Plot forecast vs both targets ---
results_long <- results %>%
  pivot_longer(cols = c(secchi_smoothed, secchi_raw), names_to = "target", values_to = "observed")

ggplot(results_long, aes(x = datetime)) +
  geom_point(data = results_long %>% filter(target == "secchi_raw"),
             aes(y = observed), color = "red", shape = 1, size = 2) +
  geom_line(data = results_long %>% filter(target == "secchi_smoothed"),
            aes(y = observed), color = "blue", size = 1) +
  geom_line(aes(y = forecast), color = "black", linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = forecast), color = "black") +
  labs(title = "Forecast vs Raw and Smoothed Secchi Depth",
       subtitle = "Using Lagged Predictors (AirTemp lag 15, Rain lag 6)",
       x = "Date", y = "Secchi Depth (m)",
       color = "Target") +
  theme_minimal(base_size = 14)


# Plot with legend and customized colors
ggplot(results_long, aes(x = datetime)) +
  geom_point(data = results_long %>% filter(target == "secchi_raw"),
             aes(y = observed, color = "Raw Secchi"),
             shape = 16, size = 2) +
  geom_line(data = results_long %>% filter(target == "secchi_smoothed"),
            aes(y = observed, color = "Smoothed Secchi"),
            linewidth = 1) +
  geom_line(aes(y = forecast, color = "Forecast"), linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = forecast, color = "Forecast"), size = 2) +
  scale_color_manual(values = c("Raw Secchi" = "red",
                                "Smoothed Secchi" = "blue",
                                "Forecast" = "black")) +
  labs(title = "Forecast vs Raw and Smoothed Secchi Depth",
       subtitle = "Using Lagged Predictors (AirTemp lag 15, Rain lag 6)",
       x = "Date", y = "Secchi Depth (m)",
       color = "Legend") +
  theme_minimal(base_size = 14)

ggplot(results_long, aes(x = datetime)) +
  # Raw Secchi as large outlined points
  geom_point(data = results_long %>% filter(target == "secchi_raw"),
             aes(y = observed, color = "Raw Secchi"),
             shape = 21, size = 3, fill = "black", stroke = 1.2) +

  # Smoothed Secchi as blue line
  geom_line(data = results_long %>% filter(target == "secchi_smoothed"),
            aes(y = observed, color = "Smoothed Secchi"),
            linewidth = 1.2) +

  # Forecast as dashed black line with points
  geom_line(aes(y = forecast, color = "Forecast"),
            linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = forecast, color = "Forecast"), size = 2.5) +

  scale_color_manual(values = c(
    "Raw Secchi" = "black",
    "Smoothed Secchi" = "red",
    "Forecast" = "blue"
  )) +

  labs(title = "Forecast vs Raw and Smoothed Secchi Depth",
       subtitle = "Using Lagged Predictors (AirTemp lag 15, Rain lag 6)",
       x = "Date", y = "Secchi Depth (m)",
       color = "Legend") +

  theme_minimal(base_size = 14)

range(results_long$datetime)

