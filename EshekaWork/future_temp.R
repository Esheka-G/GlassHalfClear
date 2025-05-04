remotes::install_github("ltreb-reservoirs/vera4castHelpers")
library(dplyr)

start_forecast_date <- format(Sys.Date() - 1, "%Y-%m-%d")

weather <- vera4castHelpers::noaa_stage2(start_date = start_forecast_date)
df_future <- weather |>
  dplyr::filter(site_id == "fcre") |>
  dplyr::collect()

library(lubridate)   # hour(), minute()


agg_noon <- function(.data, value_col, fun = mean) {
  .data %>%
    filter(hour(datetime) == 12, minute(datetime) == 0) %>%
    mutate(date = as.Date(datetime)) %>%
    group_by(date) %>%
    summarise(value = fun({{ value_col }}, na.rm = TRUE), .groups = "drop")
}

airtemp_noon <- df_future %>%
  filter(variable == "air_temperature") %>%
  mutate(pred_C = prediction - 273.15) %>%
  agg_noon(pred_C) %>%
  rename(air_temp_C = value)

# 1. extract noon precipitation‑flux rows once per date
precip_noon <- df_future %>%
  filter(variable == "precipitation_flux",
         hour(datetime) == 12, minute(datetime) == 0) %>%
  mutate(date        = as.Date(datetime),
         flux        = prediction,          # kg m^-2 s^-1
         precip_mm = flux * 86400) %>%
  group_by(date) %>%
  slice(1) %>%
  ungroup() %>%
  select(date, precip_mm)

# 2. join to the noon‑air‑temp table you already built
airtemp_noon <- airtemp_noon %>%
  left_join(precip_noon, by = "date")
