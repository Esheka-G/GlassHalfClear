# Load necessary library
library(tidyverse)

# Aggregate Secchi Depth Data: Ensure it remains the primary dataset
SecchiAgg <- SecchiDepth %>%
  group_by(Reservoir, Site, DateTime) %>%
  summarize(Secchi_m = mean(Secchi_m, na.rm = TRUE), .groups = "drop")

# Aggregate CTD Data based on SecchiDepth dates
CTDAgg <- CTD %>%
  filter(DateTime %in% SecchiAgg$DateTime) %>%
  group_by(Reservoir, Site, DateTime) %>%
  summarize(across(c(Depth_m, Temp_C, DO_mgL, DOsat_percent, Cond_uScm, SpCond_uScm,
                     Chla_ugL, Turbidity_NTU, pH, ORP_mV, PAR_umolm2s,
                     CDOM_ugL, Phycoerythrin_ugL, Phycocyanin_ugL),
                   mean, na.rm = TRUE), .groups = "drop")

# Aggregate Sensor Data based on SecchiDepth dates
SensorAgg <- SensorData %>%
  filter(DateTime %in% SecchiAgg$DateTime) %>%
  group_by(Reservoir, Site, DateTime) %>%
  summarize(across(c(Depth_m, Temp_C, DO_mgL, DOsat_percent, Cond_uScm, SpCond_uScm,
                     PAR_umolm2s, ORP_mV, pH),
                   mean, na.rm = TRUE), .groups = "drop")

# Aggregate Chemistry Data based on SecchiDepth dates
ChemistryAgg <- ChemistryData %>%
  filter(DateTime %in% SecchiAgg$DateTime) %>%
  group_by(Reservoir, Site, DateTime) %>%
  summarize(across(c(Depth_m, TN_ugL, TP_ugL, NH4_ugL, NO3NO2_ugL, SRP_ugL,
                     DOC_mgL, DIC_mgL, DC_mgL, DN_mgL),
                   mean, na.rm = TRUE), .groups = "drop")

# Merge all datasets, keeping SecchiDepth as the primary dataset
FinalAggregatedData <- SecchiAgg %>%
  left_join(CTDAgg, by = c("Reservoir", "Site", "DateTime")) %>%
  left_join(SensorAgg, by = c("Reservoir", "Site", "DateTime")) %>%
  left_join(ChemistryAgg, by = c("Reservoir", "Site", "DateTime"))
