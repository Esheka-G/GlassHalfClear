# Load required package
library(dplyr)

# Aggregate dataset by datetime and compute mean for other columns
CTD_avg <- CTD %>%
  group_by(DateTime) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# View the result
print(CTD_avg)
