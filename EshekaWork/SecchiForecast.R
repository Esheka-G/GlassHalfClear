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
fcreData_Secchi <- targets |>
  filter(site_id == "fcre") |>
  filter(variable == "Secchi_m_sample")

# Beaverdam only
bvreData_Secchi <- targets |>
  filter(site_id == "bvre") |>
  filter(variable == "Secchi_m_sample")

