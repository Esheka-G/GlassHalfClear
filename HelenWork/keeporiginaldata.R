library(tidyverse)
library(zoo)

# Define a NEW version of smoothing that is actually interpolation only
interpolate_daily <- function(df, site_label) {
  # Make sure datetime is Date and sorted
  df <- df %>%
    mutate(datetime = as.Date(datetime)) %>%
    arrange(datetime)

  # Extract numeric variable names (excluding depth_m if it exists)
  numeric_vars <- df %>%
    select(where(is.numeric)) %>%
    select(-any_of("depth_m")) %>%
    names()

  # Create full daily date grid
  daily_grid <- tibble(datetime = seq.Date(min(df$datetime), max(df$datetime), by = "day"))

  # Left join original data into daily grid
  daily_grid <- daily_grid %>%
    left_join(df %>% select(datetime, all_of(numeric_vars)), by = "datetime") %>%
    arrange(datetime)

  # Interpolate all numeric variables
  daily_grid <- daily_grid %>%
    mutate(across(all_of(numeric_vars), ~ na.approx(., na.rm = FALSE, rule = 2)))

  # Add site label
  daily_grid$site_id <- site_label

  return(daily_grid)
}

# Now apply it to your reservoirs
fcre_smoothed <- interpolate_daily(fcre_Combined, site_label = "fcre")
bvre_smoothed <- interpolate_daily(bvre_Combined, site_label = "bvre")
