url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

url_inflow <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"

library(tidyverse)
targets <- read_csv(url, show_col_types = FALSE)
inflow_targets <- read_csv(url_inflow, show_col_types = FALSE)

targets <- dplyr::bind_rows(targets, inflow_targets)
