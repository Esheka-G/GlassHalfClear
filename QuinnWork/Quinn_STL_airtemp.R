# Core time series packages
library(forecast)
library(tidyverse)
library(Metrics)
library(dplyr)

# Filter and join
fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013") %>%
  count(year)

secchi_data <- fcre_smoothed %>%
  left_join(meterology_AirTemp %>% select(datetime, AirTemp_C_mean),  # Correct column reference here
            by = "datetime")

n <- nrow(secchi_data)
trainN <- n - 30
testN <- trainN + 1

# Training and testing splits
train <- secchi_data[1:trainN, ]
test <- secchi_data[testN:n, ]
ts.train <- ts(train$Secchi_m, frequency = 365)

# Helper function to train and forecast using one regressor
run_model <- function(regressor_all, regressor_name) {
  xreg_train <- matrix(regressor_all[1:trainN], ncol = 1)
  xreg_test <- matrix(regressor_all[testN:n], ncol = 1)

  stl.fit <- stlm(ts.train, s.window = "periodic",
                  method = "arima", xreg = xreg_train)

  stl.forecasts <- forecast(stl.fit, h = 30, newxreg = xreg_test)

  forecast_df <- data.frame(
    time = 1:30,
    observed = test$Secchi_m,
    forecast = stl.forecasts$mean,
    model = regressor_name
  )

  list(df = forecast_df, rmse = rmse(test$Secchi_m, stl.forecasts$mean),
       mae = mae(test$Secchi_m, stl.forecasts$mean))
}

# Run the model using AirTemp_C_mean as the regressor
results_original <- run_model(secchi_data$AirTemp_C_mean, "Air Temperature")

# Combine results
forecast_comparison <- results_original$df

# Plot forecast comparison
ggplot(forecast_comparison, aes(x = time)) +
  geom_line(aes(y = observed), color = "black", linewidth = 1) +
  geom_line(aes(y = forecast, color = model), linewidth = 1) +
  geom_point(aes(y = forecast, color = model)) +
  labs(title = "Forecast Comparison by Air Temperature",
       y = "Secchi Depth", x = "Time (Days)",
       color = "Model") +
  theme_minimal()

# Numeric performance comparison
comparison_metrics <- data.frame(
  Model = c("Air Temperature"),
  RMSE = c(results_original$rmse),
  MAE = c(results_original$mae)
)

print(comparison_metrics)
