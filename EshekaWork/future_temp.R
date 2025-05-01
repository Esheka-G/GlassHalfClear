remotes::install_github("ltreb-reservoirs/vera4castHelpers")
weather <- vera4castHelpers::noaa_stage2(start_date = "2025-04-30")
df_future <- weather |>
  dplyr::filter(site_id == "fcre") |>
  dplyr::collect()
