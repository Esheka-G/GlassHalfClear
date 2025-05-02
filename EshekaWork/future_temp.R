remotes::install_github("ltreb-reservoirs/vera4castHelpers")
library(dplyr)

start_forecast_date <- format(Sys.Date() - 1, "%Y-%m-%d")

weather <- vera4castHelpers::noaa_stage2(start_date = start_forecast_date)
df_future <- weather |>
  dplyr::filter(site_id == "fcre") |>
  dplyr::collect()

library(lubridate)   # hour(), minute()

airtemp_noon <- df_future %>%
  ## 1. keep only the air‑temperature rows
  filter(variable == "air_temperature") %>%

  ## 2. keep only the rows whose timestamp is exactly 12:00:00
  filter(hour(datetime) == 12, minute(datetime) == 0) %>%

  ## 3. drop any forecast metadata you don’t need
  mutate(date = as.Date(datetime)) %>%
  select(date, air_temp = prediction)                # <-- change “prediction”
#     to the actual value
#     column in your data
