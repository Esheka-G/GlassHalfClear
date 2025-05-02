# Core time series packages
library(forecast)
library(tidyverse)
library(Metrics)
library(dplyr)
library(scoringRules)  # For CRPS

# Filter and join
fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013") %>%
  count(year)

secchi_data <- fcre_smoothed %>%
  left_join(meterology_AirTemp %>% select(datetime, AirTemp_C_mean, ),
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
  stl.df <- as.data.frame(stl.forecasts)

  # Estimate forecast standard deviation using 80% interval (z ≈ 1.28)
  z_80 <- qnorm(0.9)
  sd_est <- (stl.df$`Hi 80` - stl.df$`Lo 80`) / (2 * z_80)

  # Compute CRPS
  crps_vals <- crps_norm(y = test$Secchi_m, location = stl.df$`Point Forecast`, scale = sd_est)
  crps_score <- mean(crps_vals, na.rm = TRUE)

  forecast_df <- data.frame(
    time = 1:30,
    observed = test$Secchi_m,
    forecast = stl.forecasts$mean,
    model = regressor_name
  )

  list(
    df = forecast_df,
    rmse = rmse(test$Secchi_m, stl.forecasts$mean),
    mae = mae(test$Secchi_m, stl.forecasts$mean),
    crps = crps_score
  )
}

# Run the model using AirTemp_C_mean as the regressor
results_original <- run_model(secchi_data$AirTemp_C_mean.x, "Air Temperature")

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

# Numeric performance comparison including CRPS
comparison_metrics <- data.frame(
  Model = c("Air Temperature"),
  RMSE = c(results_original$rmse),
  MAE = c(results_original$mae),
  CRPS = c(results_original$crps)
)

print(comparison_metrics)
