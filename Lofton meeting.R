url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

url_inflow <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"

site_list <- read_csv("https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv", show_col_types = FALSE)

library(tidyverse)
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


