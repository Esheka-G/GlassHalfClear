# Attach correct datetime to the forecast
test_dates <- secchi_data$datetime[testN:n]
forecast_comparison <- forecast_comparison %>%
  mutate(datetime = test_dates)

# Load the real original raw Secchi data
real_secchi <- fcreData_Secchi %>%
  mutate(datetime = as.Date(datetime)) %>%
  select(datetime, Secchi_m_actual = Secchi_m) %>%
  arrange(datetime)

# Join forecast and raw actual observations
forecast_vs_actual <- forecast_comparison %>%
  left_join(real_secchi, by = "datetime") %>%
  filter(!is.na(Secchi_m_actual))

# Plot forecast

ggplot(forecast_vs_actual, aes(x = datetime)) +
  geom_line(aes(y = forecast, color = "Forecast (AirTemp model)"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = forecast, color = "Forecast (AirTemp model)")) +
  geom_line(aes(y = Secchi_m_actual, color = "Actual Raw Secchi"), size = 1.2) +
  geom_point(aes(y = Secchi_m_actual, color = "Actual Raw Secchi")) +
  scale_color_manual(name = "Legend",
                     values = c("Forecast (AirTemp model)" = "red", "Actual Raw Secchi" = "blue")) +
  labs(title = "Forecast (AirTemp Model) vs Actual Raw Secchi Depth",
       x = "Date",
       y = "Secchi Depth (m)") +
  theme_minimal() +
  theme(legend.position = "bottom")

## ── 1. Make sure sd_est came out of run_model() ──────────────────────────────
## (add “sd_est = sd_est” to forecast_df inside run_model() if you haven’t yet)

# ── 2. Performance vs. the RAW Secchi observations ───────────────────────────
stl.raw_rmse <- rmse(forecast_vs_actual$Secchi_m_actual,
                 forecast_vs_actual$forecast)

stl.raw_mae  <- mae (forecast_vs_actual$Secchi_m_actual,
                 forecast_vs_actual$forecast)

crps_vals <- crps_norm(y = forecast_vs_actual$Secchi_m_actual,
                       forecast_vs_actual$forecast, scale = sd_est)
crps_score <- mean(crps_vals, na.rm = TRUE)

(comparisonDF <- data.frame(model = "stl",
                            RMSE = stl.raw_rmse,
                            MAE = stl.raw_mae, CRPS = crps_score))
print(comparisonDF)
# # Gaussian-CRPS: use the same sd_est vector you stored in forecast_vs_actual
# raw_crps <- mean(
#   crps_norm(y        = forecast_vs_actual$Secchi_m_actual,
#             location = forecast_vs_actual$forecast,
#             scale    = forecast_vs_actual$sd_est),
#   na.rm = TRUE
# )
#
# performance_raw <- data.frame(
#   Model = "Air Temperature",
#   RMSE  = raw_rmse,
#   MAE   = raw_mae,
#   CRPS  = raw_crps
# )
#
# print(performance_raw)
