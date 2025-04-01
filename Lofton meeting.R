url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

url_inflow <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"

library(tidyverse)
targets <- read_csv(url, show_col_types = FALSE)
inflow_targets <- read_csv(url_inflow, show_col_types = FALSE)

targets <- dplyr::bind_rows(targets, inflow_targets)

site_list <- read_csv("https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv", show_col_types = FALSE)


url2 <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"
inflow_targets <- read_csv(url2, show_col_types = FALSE)

# Combine all data
all_targets <- bind_rows(targets, inflow_targets)

# Filter to Secchi observations only
secchi_targets <- all_targets |>
  filter(variable == "Secchi_m_sample")

# Join with site metadata
secchi_with_site_info <- secchi_targets |>
  left_join(site_list, by = c("site_id"))

#Split by reservoir
# Falling Creek only
fcre_secchi <- secchi_with_site_info %>%
  filter(site_id == "fcre")

# Beaverdam only
bvre_secchi <- secchi_with_site_info %>%
  filter(site_id == "bvre")

