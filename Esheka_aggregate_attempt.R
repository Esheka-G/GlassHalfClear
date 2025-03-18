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



# Load required packages
library(ggplot2)
library(dplyr)
library(GGally)

# Select relevant columns for correlation analysis
cor_data <- Merge %>%
  select(Secchi_m, Turbidity_NTU, Depth_m, Temp_C, DO_mgL, Chla_ugL)

# Compute correlation matrix for Pearson, Spearman, and Kendall
cor_pearson <- cor(cor_data, use = "complete.obs", method = "pearson")
cor_spearman <- cor(cor_data, use = "complete.obs", method = "spearman")
cor_kendall <- cor(cor_data, use = "complete.obs", method = "kendall")

# Print correlation matrices
print("Pearson Correlation:")
print(cor_pearson)

print("Spearman Correlation:")
print(cor_spearman)

print("Kendall Correlation:")
print(cor_kendall)






# Load required libraries
library(ggplot2)
library(gridExtra)

# Function to create scatter plots with trend lines
plot_correlation <- function(x_var, y_var, data, x_label, y_label, line_color) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.5) +  # Scatter points with transparency
    geom_smooth(method = "lm", se = TRUE, color = line_color) +  # Add trend line with confidence interval
    labs(x = x_label, y = y_label, title = paste(y_label, "vs", x_label)) +
    theme_minimal()
}

# Generate individual plots
p1 <- plot_correlation("Secchi_m", "Turbidity_NTU", Merge, "Secchi Depth (m)", "Turbidity (NTU)", "red")
p2 <- plot_correlation("Secchi_m", "Chla_ugL", Merge, "Secchi Depth (m)", "Chlorophyll-a (ug/L)", "green")
p3 <- plot_correlation("Secchi_m", "Temp_C", Merge, "Secchi Depth (m)", "Temperature (°C)", "blue")
p4 <- plot_correlation("Secchi_m", "DO_mgL", Merge, "Secchi Depth (m)", "Dissolved Oxygen (mg/L)", "purple")

# Arrange plots in a grid layout
grid.arrange(p1, p2, p3, p4, ncol = 2)










