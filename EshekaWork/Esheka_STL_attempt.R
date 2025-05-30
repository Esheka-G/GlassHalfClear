# Core time series packages
library(forecast)   # For STL and ARIMA
library(tidyverse)  # For data wrangling and plotting
library(Metrics)
library(dplyr)


# Example: Load your data
# secchi_data <- fcreData_Secchi

fcre_secchi_data %>%
  mutate(year = format(as.Date(datetime), "%Y")) %>%
  filter(year != "2013") %>%         # remove 2013
  count(year)

# secchi_data <- fcre_smoothed

# secchi_data <- fcre_smoothed %>%
#   left_join(fcreData_WaterTemp %>% select(datetime, fcreData_WaterTemp$Temp_C_Mean),
#             by = "datetime")

# Preview the structure
head(secchi_data)

n <- nrow(secchi_data)

# The length of the data set minus the most recent 30 days
trainN <- n-30
testN <- trainN+1

# Index the earlier rows for training
train <- secchi_data[1:trainN,]

# Index the later 30 for testing
test <- secchi_data[testN:n,]
nrow(test) # Should be 30

# STL requires a UNIVARIATE time series object
# ts.train <- ts(train$observation, frequency = 35)
ts.train <- ts(train$Secchi_m, frequency = 365)

# REGRESSOR PROCESSING
# xreg_all <- fcreData_WaterTemp$Temp_C_Mean
# xreg_train <- matrix(xreg_all[1:trainN], ncol = 1)
# xreg_test <- matrix(xreg_all[testN:n], ncol = 1)


stl.fit <- stlm(ts.train, s.window = "periodic",
                method = "arima")

summary(stl.fit$model)
hist(stl.fit$residuals)
stl.forecasts <- forecast(stl.fit, h = 30)

#stl.forecasts <- forecast(stl.fit, h = 30, newxreg = xreg_test)

## The forecast function gives us point forecasts, as well as prediction intervals
stl.forecasts
# xreg = regressors
# arimax model
# in a matrix?
# to forecast = newxreg
str(stl.forecasts)
stl.df <- as.data.frame(stl.forecasts)
plot(stl.forecasts)


# First make a data frame with both in there
compare <- data.frame(time = seq(1:30),
                      observed = test$Secchi_m,
                      forecast = stl.df$`Point Forecast`)

library(tidyr)

compare_long <- compare |>
  pivot_longer(cols = c(observed, forecast),
               names_to  = "series",
               values_to = "value")

ggplot(compare_long, aes(x = time, y = value, colour = series)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c(observed = "blue",
                                 forecast = "red"),
                      labels = c(observed = "Observed",
                                 forecast = "STL Forecast"),
                      name   = NULL) +
  labs(title = "Observed vs STL Forecast (Falling Creek)",
       x     = "Time (Days)",
       y     = "Secchi Depth (m)") +
  theme_minimal() +
  theme(legend.position = "bottom")

stl.rmse <- rmse(compare$observed, compare$forecast)
stl.mae <- mae(compare$observed, compare$forecast)
crps_vals <- crps_norm(y = compare$observed, location = compare$forecast, scale = sd_est)
crps_score <- mean(crps_vals, na.rm = TRUE)

(comparisonDF <- data.frame(model = "stl",
                            RMSE = stl.rmse,
                            MAE = stl.mae, CRPS = crps_score))
