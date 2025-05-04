library(tidyverse)
library(zoo)

interpolate_forecast_vars <- function(df, site_label) {
  df <- df %>%
    mutate(datetime = as.Date(datetime)) %>%
    arrange(datetime)

  # Focused on just these three variables
  vars <- c("Secchi_m", "Rain_mm_sum", "AirTemp_C_mean")

  # Daily date sequence
  daily_grid <- tibble(datetime = seq.Date(min(df$datetime), max(df$datetime), by = "day"))

  # Join in the three variables
  daily_grid <- daily_grid %>%
    left_join(df %>% select(datetime, all_of(vars)), by = "datetime") %>%
    arrange(datetime)

  # Preserve original values in new columns
  for (var in vars) {
    daily_grid[[paste0(var, "_original")]] <- daily_grid[[var]]
  }

  # Interpolate only missing values
  daily_grid <- daily_grid %>%
    mutate(across(all_of(vars), ~ ifelse(is.na(.), na.approx(., na.rm = FALSE, rule = 2), .)))

  # Add site label
  daily_grid$site_id <- site_label

  return(daily_grid)
}

forecast_smoothed <- interpolate_forecast_vars(forecast_dataset, site_label = "fcre")

