# Core time series packages
library(forecast)
library(tidyverse)
library(Metrics)
library(dplyr)
library(scoringRules)  # For CRPS
library(vera4castHelpers)

# Filter and join
fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013") %>%
  count(year)

secchi_data <- fcre_smoothed %>%
  left_join(meterology_AirTemp %>% select(datetime, AirTemp_C_mean),
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
                  method = "arima",
                  xreg = xreg_train)

  stl.forecasts <- forecast(stl.fit, h = 30, newxreg = xreg_test)
  stl.df <- as.data.frame(stl.forecasts)

  # Estimate forecast standard deviation using 80% interval (z ≈ 1.28)
  z_80 <- qnorm(0.9)
  sd_est <- (stl.df$`Hi 80` - stl.df$`Lo 80`) / (2 * z_80)

  # Compute CRPS
  crps_vals <- crps_norm(y = test$Secchi_m,
                         location = stl.df$`Point Forecast`,
                         scale = sd_est)
  crps_score <- mean(crps_vals, na.rm = TRUE)

  forecast_df <- data.frame(
    time = 1:30,
    observed = test$Secchi_m,
    forecast  = stl.df$`Point Forecast`,
    sd        = sd_est,              # <-- keep σ̂ for later
    lo80      = stl.df$`Lo 80`,      # optional, for ribbons
    hi80      = stl.df$`Hi 80`,
    lo95      = stl.df$`Lo 95`,
    hi95      = stl.df$`Hi 95`,
    model     = regressor_name
  )

  list(
    df = forecast_df,
    rmse = rmse(test$Secchi_m, stl.df$`Point Forecast`),
    mae = mae (test$Secchi_m, stl.df$`Point Forecast`),
    crps = crps_score
  )
}

# Run the model using AirTemp_C_mean as the regressor
results_original   <- run_model(secchi_data$AirTemp_C_mean.x, "Air Temperature")

# Combine results
forecast_comparison <- results_original$df

test_dates <- secchi_data$datetime[testN:n]
forecast_comparison <- forecast_comparison |>
  mutate(datetime = test_dates)

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

ggplot(forecast_vs_actual, aes(x = datetime)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.15, fill = "grey70") +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = 0.25, fill = "grey50") +
  geom_line  (aes(y = forecast, colour = "Forecast"), linetype = "dashed") +
  geom_point (aes(y = forecast, colour = "Forecast")) +
  geom_line  (aes(y = Secchi_m_actual, colour = "Actual"), linewidth = 1.2) +
  geom_point (aes(y = Secchi_m_actual, colour = "Actual")) +
  scale_colour_manual(NULL, values = c("Forecast" = "red", "Actual" = "blue")) +
  theme_minimal() +
  labs(title = "STL forecast with 80 % & 95 % prediction intervals",
       y = "Secchi Depth (m)", x = NULL,
       caption = "Shaded bands represent predictive uncertainty")

#Set metadata for VERA4cast
project_id <- "vera4cast"
model_id <- "stl_airtemp"
reference_datetime <- format(min(test_dates), "%Y-%m-%d 00:00:00", tz = "UTC")
duration <- "P1D"
site_id <- "fcre"
depth_m <- NA
family <- "normal"
variable <- "secchi"

#Build our long-format forecast for export
vera4cast_df <- forecast_comparison %>%
  select(datetime, forecast, sd) %>%
  mutate(datetime = format(as.POSIXct(datetime), "%Y-%m-%d 00:00:00", tz = "UTC")) %>%
  pivot_longer(cols = c(forecast, sd),
               names_to = "parameter",
               values_to = "prediction") %>%
  mutate(parameter = recode(parameter,
                            "forecast" = "mu",
                            "sd"       = "sigma"),
         project_id = project_id,
         model_id = model_id,
         reference_datetime = reference_datetime,
         duration = duration,
         site_id = site_id,
         depth_m = depth_m,
         family = family,
         variable = variable) %>%
  select(project_id, model_id, datetime, reference_datetime,
         duration, site_id, depth_m, family, parameter, variable, prediction)

# Export to CSV
write_csv(vera4cast_df, "stl_airtemp_forecast.csv")

read_csv("stl_airtemp_forecast.csv") %>% head()

#Check forecast format
vera4castHelpers::forecast_output_validator("stl_airtemp_forecast.csv")
