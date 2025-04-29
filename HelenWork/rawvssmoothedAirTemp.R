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
