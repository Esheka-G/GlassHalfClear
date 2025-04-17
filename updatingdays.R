library(tidyverse)
library(zoo)

# FCRE
# This version forces the time grid, then interpolates all numeric columns
fcre_secchi_data <- fcre_Combined |>
  mutate(datetime = as.Date(datetime)) |>
  select(datetime, Secchi_m, Temp_C_mean, Chla_ugL_mean, Bloom_binary_mean,
         AirTemp_C_mean, Rain_mm_sum) |>
  arrange(datetime)

# Create full daily time grid
full_dates <- tibble(datetime = seq.Date(min(secchi_data$datetime), max(secchi_data$datetime), by = "day"))

# Left join to get full daily records, then interpolate missing values
fcre_secchi_data <- full_dates |>
  left_join(fcre_secchi_data, by = "datetime") |>
  arrange(datetime) |>
  mutate(across(where(is.numeric), ~ na.approx(., na.rm = FALSE, rule = 2)))


# BVRE
# This version forces the time grid, then interpolates all numeric columns
bvre_secchi_data <- bvre_Combined |>
  mutate(datetime = as.Date(datetime)) |>
  select(datetime, Secchi_m, Temp_C_mean, Chla_ugL_mean, Bloom_binary_mean,
         AirTemp_C_mean, Rain_mm_sum) |>
  arrange(datetime)

# Create full daily time grid
full_dates <- tibble(datetime = seq.Date(min(secchi_data$datetime), max(secchi_data$datetime), by = "day"))

# Left join to get full daily records, then interpolate missing values
bvre_secchi_data <- full_dates |>
  left_join(bvre_secchi_data, by = "datetime") |>
  arrange(datetime) |>
  mutate(across(where(is.numeric), ~ na.approx(., na.rm = FALSE, rule = 2)))

