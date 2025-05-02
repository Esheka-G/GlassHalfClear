library(tidyverse)

url3 <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"
met_targets <- read_csv(url3, show_col_types = FALSE)

meterology_AirTemp <- met_targets |>
  filter(variable == "AirTemp_C_mean") |>
  rename(AirTemp_C_mean = observation) |>
  select(-variable)

meterology_Precip <- met_targets |>
  filter(variable == "Rain_mm_sum") |>
  rename(Rain_mm_sum = observation) |>
  select(-variable)

meterology_Humid <- met_targets |>
  filter(variable == "RH_percent_mean") |>
  rename(RH_percent_mean = observation) |>
  select(-variable)


# Join air temperature into Falling Creek combined data
fcre_Combined <- fcre_Combined |>
  left_join(meterology_AirTemp |>
              select(datetime, site_id, AirTemp_C_mean),
            by = c("datetime", "site_id"))

# Join daily sum of precipitation for FCRE
fcre_Combined <- fcre_Combined |>
  left_join(meterology_Precip |>
              select(datetime, site_id, Rain_mm_sum),
            by = c("datetime", "site_id"))

# Join daily sum of precipitation for FCRE
fcre_Combined <- fcre_Combined |>
  left_join(meterology_Humid |>
              select(datetime, site_id, RH_percent_mean),
            by = c("datetime", "site_id"))


# Join air temperature into Beaverdam combined data using FCR Data
bvre_Combined <- bvre_Combined |>
  left_join(meterology_AirTemp |>
              select(datetime, AirTemp_C_mean),
            by = "datetime")

# Join daily sum of precipitation for BVRE combined data using FCR Data
bvre_Combined <- bvre_Combined |>
  left_join(meterology_Precip |>
              select(datetime, Rain_mm_sum),
            by = "datetime")

# Join daily sum of precipitation for BVRE combined data using FCR Data
bvre_Combined <- bvre_Combined |>
  left_join(meterology_Humid |>
              select(datetime, RH_percent_mean),
            by = "datetime")

