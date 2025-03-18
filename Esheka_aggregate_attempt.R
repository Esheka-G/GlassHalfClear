# Load required package
library(dplyr)

# Aggregate dataset by datetime and compute mean for other columns
CTD_avg <- CTD %>%
  group_by(DateTime) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# View the result
print(CTD_avg)

# Combining Secchi Depth and CTD
# Load required packages
library(dplyr)
library(fuzzyjoin)

# Perform a fuzzy join based on nearest datetime
Merge <- difference_inner_join(SecchiDepth, CTD_avg, by = "DateTime", max_dist = 3600, distance_col = "time_diff")

# Correlation of Secchi Depth and Turbity
x <- Merge$Secchi_m
y <- Merge$Turbidity_NTU

correlation <- cor(x, y, use = "complete.obs")
print(correlation)

# Create a dual-axis plot
ggplot(Merge, aes(x = DateTime.x)) +
  geom_line(aes(y = Secchi_m, color = "Secchi Depth (m)"), size = 1) +
  geom_line(aes(y = Turbidity_NTU, color = "Turbidity (NTU)"), size = 1) +
  scale_y_continuous(
    name = "Secchi Depth (m)",
    sec.axis = sec_axis(~ ., name = "Turbidity (NTU)")
  ) +
  labs(x = "DateTime", title = "Secchi Depth and Turbidity Over Time") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())


colnames(Merge)

