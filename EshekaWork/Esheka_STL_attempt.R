# Core time series packages
library(forecast)   # For STL and ARIMA
library(tidyverse)  # For data wrangling and plotting

# Example: Load your data
secchi_data <- fcreData_Secchi

# Preview the structure
head(secchi_data)

# STL requires a UNIVARIATE time series object
ts.secchi <- ts(secchi_data$observation, frequency = 35)

stl.fit <- stlm(ts.secchi, s.window = "periodic",
                method = "arima")


##stl.fit <- stlm(ts.train, s.window = "periodic",
##                method = "ets",
##                etsmodel = "ANN")

summary(stl.fit$model)
hist(stl.fit$residuals)
stl.forecasts <- forecast(stl.fit, h = 5)

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
compare <- data.frame(time = seq(1:5),
                      observed = secchi_data$observation,
                      forecast = stl.df$`Point Forecast`)

# What do you think??
ggplot(data = compare, aes(x = time, y = observed))+
  geom_line(color = "blue")+
  geom_point(color = "blue")+
  geom_line(aes(y = forecast), color = "red")+
  geom_point(aes(y = forecast), color = "red")
