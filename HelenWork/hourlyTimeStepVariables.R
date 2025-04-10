url3 <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=PT1H/hourly-met-targets.csv.gz"
hourly_targets <- read_csv(url3, show_col_types = FALSE)
