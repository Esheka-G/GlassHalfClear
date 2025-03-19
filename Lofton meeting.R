url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

library(tidyverse)
targets <- read_csv(url, show_col_types = FALSE) %>%
  filter(depth_m %in% c(1.5,1.6))

bvre_nuts <- targets

unique(targets$variable)
unique(targets$depth_m)
unique(targets$site_id)

unique(bvre_nuts$depth_m)

hist(bvre_nuts$depth_m)

# Surface Conditions are ore important
