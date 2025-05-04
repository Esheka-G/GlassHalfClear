library(tidyverse)

get_secchi_airtemp_precip_data <- function(site = "fcre") {
  # URLs
  url_insitu <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
  url_inflow <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"
  url_met <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"

  # Load data
  insitu <- read_csv(url_insitu, show_col_types = FALSE)
  inflow <- read_csv(url_inflow, show_col_types = FALSE)
  met <- read_csv(url_met, show_col_types = FALSE)

  # Combine and properly filter to site
  targets <- bind_rows(insitu, inflow) %>%
    filter(site_id == !!site)

  # Secchi
  secchi <- targets %>%
    filter(variable == "Secchi_m_sample") %>%
    rename(Secchi_m = observation) %>%
    select(datetime, site_id, Secchi_m)

  # Meteorology (site-agnostic)
  airtemp <- met %>%
    filter(variable == "AirTemp_C_mean") %>%
    rename(AirTemp_C_mean = observation) %>%
    select(datetime, AirTemp_C_mean)

  precip <- met %>%
    filter(variable == "Rain_mm_sum") %>%
    rename(Rain_mm_sum = observation) %>%
    select(datetime, Rain_mm_sum)

  # Join
  combined <- secchi %>%
    left_join(airtemp, by = "datetime") %>%
    left_join(precip, by = "datetime")

  return(combined)
}

fcre_Combined <- get_secchi_airtemp_precip_data("fcre")
bvre_Combined <- get_secchi_airtemp_precip_data("bvre")
