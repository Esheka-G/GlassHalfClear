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
  filter(variable == "Secchi_m_sample")

fcreData_WaterTemp <- fcreData |>
  filter(variable == "Temp_C_mean")

fcreData_ChloroA <- fcreData |>
  filter(variable == "Chla_ugL_mean")

fcreData_Bloom <- fcreData |>
  filter(variable == "Bloom_binary_mean")

# Beaverdam only
bvreData <- targets |>
  filter(site_id == "bvre")

# Isolating bvre variables
bvreData_Secchi <- bvreData |>
  filter(variable == "Secchi_m_sample")

bvreData_WaterTemp <- bvreData |>
  filter(variable == "Temp_C_mean")

bvreData_ChloroA <- bvreData |>
  filter(variable == "Chla_ugL_mean")

bvreData_Bloom <- bvreData |>
  filter(variable == "Bloom_binary_mean")


