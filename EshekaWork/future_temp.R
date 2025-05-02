remotes::install_github("ltreb-reservoirs/vera4castHelpers")
library(dplyr)

start_forecast_date <- format(Sys.Date() - 1, "%Y-%m-%d")

weather <- vera4castHelpers::noaa_stage2(start_date = start_forecast_date)
df_future <- weather |>
  dplyr::filter(site_id == "fcre") |>
  dplyr::collect()

library(lubridate)   # hour(), minute()


airtemp_noon <- df_future %>%
  filter(variable == "air_temperature") %>%          # keep only air‑temp rows
  filter(hour(datetime) == 12, minute(datetime) == 0) %>%   # keep 12:00:00
  mutate(date        = as.Date(datetime),
         air_temp_K  = prediction,        # <‑‑ value column in Kelvin
         air_temp_C  = air_temp_K - 273.15) %>%      # convert to °C
  select(date, air_temp_C)                           # keep what you need

# result:
#   date       air_temp_C
#  2025-05-01     19.57
#  2025-05-02     21.13
#  ...

