library(tidyverse)
url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

url_inflow <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"

site_list <- read_csv("https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv", show_col_types = FALSE)

targets <- read_csv(url, show_col_types = FALSE)
inflow_targets <- read_csv(url_inflow, show_col_types = FALSE)

# Combining all variables together
targets <- dplyr::bind_rows(targets, inflow_targets)

#Split by reservoir
# Falling Creek only
fcreData <- targets |>
  filter(site_id == "fcre")

# Beaverdam only
bvreData <- targets |>
  filter(site_id == "bvre")


# Find unique variables in variable column
unique(fcreData$variable)

unique(bvreData$variable)

# "RDOTemp_C_6" & "RDOTemp_C_13"
# These are only in bvre
# daily mean oxygen in milligrams per liter from EXO;
# focal depths bvre = 1.5 m, fcre = 1.6 m, measured using EXO and RDO

# Reshape Data

# Falling Creek only
fcreData <- targets |>
  filter(site_id == "fcre")

# Isolating fcre variables
fcreData_Secchi <- fcreData |>
  filter(variable == "Secchi_m_sample") |>
  rename(Secchi_m = observation) |>
  select(-variable)

observations_per_year <- fcreData_Secchi |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  summarise(count = n())

print(observations_per_year)

fcreData_WaterTemp <- fcreData |>
  filter(variable == "Temp_C_mean") |>
  filter(depth_m == 1.6) |>
  rename(Temp_C_mean = observation) |>
  select(-variable)

fcreData_ChloroA <- fcreData |>
  filter(variable == "Chla_ugL_mean") |>
  rename(Chla_ugL_mean = observation) |>
  select(-variable)

fcreData_Bloom <- fcreData |>
  filter(variable == "Bloom_binary_mean") |>
  rename(Bloom_binary_mean = observation) |>
  select(-variable)

# Beaverdam only
bvreData <- targets |>
  filter(site_id == "bvre")

# Isolating bvre variables
bvreData_Secchi <- bvreData |>
  filter(variable == "Secchi_m_sample") |>
  rename(Secchi_m = observation) |>
  select(-variable)

bvreData_WaterTemp <- bvreData |>
  filter(variable == "Temp_C_mean") |>
  filter(depth_m == 1.5) |>
  rename(Temp_C_mean = observation) |>
  select(-variable)

bvreData_ChloroA <- bvreData |>
  filter(variable == "Chla_ugL_mean") |>
  rename(Chla_ugL_mean = observation) |>
  select(-variable)

bvreData_Bloom <- bvreData |>
  filter(variable == "Bloom_binary_mean") |>
  rename(Bloom_binary_mean = observation) |>
  select(-variable)

# Combine Datasets

fcre_Combined <- fcreData_Secchi |>
  left_join(fcreData_WaterTemp |>
              select(datetime, site_id, depth_m, Temp_C_mean),
            by = c("datetime", "site_id")) |>
  left_join(fcreData_ChloroA |>
              select(datetime, site_id, Chla_ugL_mean),
            by = c("datetime", "site_id")) |>
  left_join(fcreData_Bloom |>
              select(datetime, site_id, Bloom_binary_mean),
            by = c("datetime", "site_id")) |>
  select(-starts_with("depth_m")) |>
  mutate(depth_m = 1.6) |>
  relocate(depth_m, .after = Secchi_m)

bvre_Combined <- bvreData_Secchi |>
  left_join(bvreData_WaterTemp |>
              select(datetime, site_id, depth_m, Temp_C_mean),
            by = c("datetime", "site_id")) |>
  left_join(bvreData_ChloroA |>
              select(datetime, site_id, Chla_ugL_mean),
            by = c("datetime", "site_id")) |>
  left_join(bvreData_Bloom |>
              select(datetime, site_id, Bloom_binary_mean),
            by = c("datetime", "site_id")) |>
  select(-starts_with("depth_m")) |>
  mutate(depth_m = 1.5) |>
  relocate(depth_m, .after = Secchi_m)
