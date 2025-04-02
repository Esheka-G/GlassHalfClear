url3 <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"
met_targets <- read_csv(url3, show_col_types = FALSE)
#glimpse(met_targets)
