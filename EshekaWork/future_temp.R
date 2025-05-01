remotes::install_github("ltreb-reservoirs/vera4castHelpers")
library(dplyr)

start_forecast_date <- format(Sys.Date() - 1, "%Y-%m-%d")

weather <- vera4castHelpers::noaa_stage2(start_date = start_forecast_date)
df_future <- weather |>
  dplyr::filter(site_id == "fcre") |>
  dplyr::collect()
