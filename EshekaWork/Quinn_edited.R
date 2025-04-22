library(forecast)
library(tidyverse)
library(tsibble)    # tidy time‑series tools
library(slider)     # rolling windows for CV

# ── 0.  Assemble data ─────────────────────────────────────────────
library(dplyr)
library(lubridate)   # for yday()

secchi_data <- fcre_smoothed %>%
  left_join(fcreData_WaterTemp %>%
              select(datetime, Temp_C_mean),   # keep clean name here
            by = "datetime") %>%
  # If fcre_smoothed already had a temp column, drop it
  select(-matches("\\.x$")) %>%                 # drops any *.x columns
  arrange(datetime) %>%                         # make sure rows are in order
  mutate(
    Temp_lag1   = dplyr::lag(Temp_C_mean, 1),
    Temp_lag2   = dplyr::lag(Temp_C_mean, 2),
    Temp_change = Temp_C_mean - Temp_lag1,
    doy         = yday(datetime)               # day‑of‑year if you need it
  ) %>%
  drop_na()                                     # removes first 2 rows w/ NA lags


# ── 1.  Train‑test split ──────────────────────────────────────────
h        <- 30                                          # forecast horizon
train_df <- head(secchi_data,  nrow(secchi_data) - h)
test_df  <- tail(secchi_data,  h)

y_train  <- ts(train_df$Secchi_m, frequency = 365)

# ── 2.  Build xreg matrices ───────────────────────────────────────
# deterministic seasonality via Fourier terms
K <- 2                                                  # 2 harmonics → sin(2πk t/T)
fourier_train <- fourier(y_train, K = K)
fourier_test  <- fourier(ts(test_df$Secchi_m,
                            frequency = 365,
                            start = end(y_train) + c(0, 1)),
                         K = K)

xreg_train <- cbind(fourier_train,
                    Temp        = train_df$Temp_C_mean,
                    Temp_lag1   = train_df$Temp_lag1,
                    Temp_change = train_df$Temp_change)

xreg_test  <- cbind(fourier_test,
                    Temp        = test_df$Temp_C_mean,
                    Temp_lag1   = test_df$Temp_lag1,
                    Temp_change = test_df$Temp_change)

# ── 3.  Fit dynamic harmonic‑regression ARIMA ─────────────────────
fit <- auto.arima(y_train,
                  xreg   = xreg_train,
                  lambda = "auto",     # Box‑Cox if needed
                  stepwise = FALSE, approximation = FALSE)

fc  <- forecast(fit, h = h, xreg = xreg_test)

# ── 4.  Accuracy check ────────────────────────────────────────────
accuracy(fc, test_df$Secchi_m)[ , c("RMSE","MAE")]

# ── 5.  Plot ──────────────────────────────────────────────────────
bind_rows(
  tibble(time = seq_along(test_df$Secchi_m),
         value = test_df$Secchi_m,
         series = "Observed"),
  tibble(time = seq_along(fc$mean),
         value = as.numeric(fc$mean),
         series = "Forecast")
) |>
  ggplot(aes(time, value, colour = series)) +
  geom_line(size = 1) +
  geom_point(data = subset(., series == "Forecast")) +
  scale_colour_manual(values = c("black","dodgerblue")) +
  labs(y = "Secchi Depth (m)",
       x = "Time (days)",
       colour = "") +
  theme_minimal(base_size = 13)
