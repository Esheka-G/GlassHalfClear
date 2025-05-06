library(forecast)
library(tidyverse)
library(Metrics)
library(scoringRules)

# Filter and add lagged air temp
secchi_data <- fcre_smoothed %>%
  filter(!is.na(Secchi_m_smoothed), !is.na(AirTemp_C_mean)) %>%
  mutate(AirTemp_lag15 = lag(AirTemp_C_mean, 15)) %>%
  drop_na()

# Split into training + last 30 days for testing
n <- nrow(secchi_data)
trainN <- n - 30
train <- secchi_data[1:trainN, ]
test <- secchi_data[(trainN + 1):n, ]

# Forecast using UNLAGGED Air Temp
fit_unlag <- auto.arima(train$Secchi_m_smoothed, xreg = train$AirTemp_C_mean)
fc_unlag <- forecast(fit_unlag, xreg = test$AirTemp_C_mean)

# Forecast using LAGGED Air Temp
fit_lag <- auto.arima(train$Secchi_m_smoothed, xreg = train$AirTemp_lag15)
fc_lag <- forecast(fit_lag, xreg = test$AirTemp_lag15)

# Prepare data frame for plot
plot_df <- tibble(
  date = test$datetime,
  observed = test$Secchi_m_smoothed,
  pred_unlag = as.numeric(fc_unlag$mean),
  pred_lag = as.numeric(fc_lag$mean)
) %>%
  pivot_longer(cols = -date, names_to = "type", values_to = "secchi")

# Plot
ggplot(plot_df, aes(x = date, y = secchi, color = type)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "30-Day Comparison: Lagged vs Unlagged Forecasts",
    subtitle = "Air Temp as Regressor for Secchi Depth",
    x = "Date", y = "Secchi Depth (m)",
    color = "Type"
  ) +
  scale_color_manual(values = c(
    "observed" = "black",
    "pred_unlag" = "blue",
    "pred_lag" = "red"
  ),
  labels = c("Observed", "Predicted (Unlagged)", "Predicted (Lagged)")) +
  theme_minimal(base_size = 14)
