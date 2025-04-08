# Core time series packages
library(forecast)   # For STL and ARIMA
library(tidyverse)  # For data wrangling and plotting
library(Metrics)

# Example: Load your data
secchi_data <- fcreData_Secchi

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
ts.train <- ts(train$observation, frequency = 35)

stl.fit <- stlm(ts.train, s.window = "periodic",
                method = "arima")

summary(stl.fit$model)
hist(stl.fit$residuals)
stl.forecasts <- forecast(stl.fit, h = 30)

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
                      observed = test$observation,
                      forecast = stl.df$`Point Forecast`)

# What do you think??
ggplot(data = compare, aes(x = time, y = observed))+
  geom_line(color = "blue")+
  geom_point(color = "blue")+
  geom_line(aes(y = forecast), color = "red")+
  geom_point(aes(y = forecast), color = "red")


stl.rmse <- rmse(compare$observed, compare$forecast)
stl.mae <- mae(compare$observed, compare$forecast)

(comparisonDF <- data.frame(model = "stl",
                            RMSE = stl.rmse,
                            MAE = stl.mae))
