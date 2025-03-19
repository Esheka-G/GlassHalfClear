# Load necessary library
library(ggplot2)

# Create the pH boxplot
ggplot(SensorData, aes(x = Reservoir, y = pH, fill = Reservoir)) +
  geom_boxplot() +
  guides(fill = "none") +
  labs(title = "pH Levels in BVR vs. FCR", x = "Reservoir", y = "pH")  +
  theme(
    plot.title = element_text(size = 28, face = "bold"),  # Title size
    axis.title.x = element_text(size = 24),  # X-axis label size
    axis.title.y = element_text(size = 24),  # Y-axis label size
    axis.text.x = element_text(size = 20),  # X-axis tick labels
    axis.text.y = element_text(size = 20),  # Y-axis tick labels
    legend.text = element_text(size = 20),  # Legend text size
    legend.title = element_text(size = 22)  # Legend title size
  )

ggsave("pH_levels_boxplot.png", dpi = 300, width = 12, height = 8)

# Turbidity Graph
ggplot(CTD, aes(x = DateTime, y = Turbidity_NTU, color = Reservoir), lwd = 2) +
  geom_line() +
  labs(title = "Turbidity Over Time",
       x = "Year",
       y = " Turbidity in NTU")  +
  theme(
    plot.title = element_text(size = 28, face = "bold"),  # Title size
    axis.title.x = element_text(size = 24),  # X-axis label size
    axis.title.y = element_text(size = 24),  # Y-axis label size
    axis.text.x = element_text(size = 20),  # X-axis tick labels
    axis.text.y = element_text(size = 20),  # Y-axis tick labels
    legend.text = element_text(size = 20),  # Legend text size
    legend.title = element_text(size = 22)  # Legend title size
  )

ggsave("turbiditygraph.png", dpi = 300, width = 12, height = 8)


# Time-series plot of Secchi Depth
ggplot(SecchiDepth, aes(x = DateTime, y = Secchi_m)) +
  geom_line(color = "black", linewidth = 1) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Secchi Depth Over Time", x = "Date", y = "Secchi Depth (m)") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),  # Title size
    axis.title.x = element_text(size = 24),  # X-axis label size
    axis.title.y = element_text(size = 24),  # Y-axis label size
    axis.text.x = element_text(size = 20),  # X-axis tick labels
    axis.text.y = element_text(size = 20),  # Y-axis tick labels
    legend.text = element_text(size = 20),  # Legend text size
    legend.title = element_text(size = 22)  # Legend title size
  )

ggsave("Secchigraph.png", dpi = 300, width = 12, height = 8)

# Seasonal Secchi Depth

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure DateTime is properly formatted in SecchiDepth dataset
SecchiDepth$Month <- format(as.Date(SecchiDepth$DateTime), "%b")

# Order months correctly from January to December
SecchiDepth$Month <- factor(SecchiDepth$Month, levels = month.abb)

# Create the boxplot for seasonal Secchi Depth variation
ggplot(SecchiDepth, aes(x = Month, y = Secchi_m, fill = Month)) +
  geom_boxplot() +
  labs(title = "Seasonal Variation in Secchi Depth", x = "Month", y = "Secchi Depth (m)")+
  guides(fill = "none") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),  # Title size
    axis.title.x = element_text(size = 24),  # X-axis label size
    axis.title.y = element_text(size = 24),  # Y-axis label size
    axis.text.x = element_text(size = 20),  # X-axis tick labels
    axis.text.y = element_text(size = 20),  # Y-axis tick labels
    legend.text = element_text(size = 20),  # Legend text size
    legend.title = element_text(size = 22)  # Legend title size
  )

ggsave("SeasonalSecchigraph.png", dpi = 300, width = 12, height = 8)


# Load required libraries
library(ggplot2)
library(gridExtra)

# Function to generate scatter plots with black points and colored trend lines
plot_correlation <- function(x_var, y_var, data, title, x_label, y_label, line_color) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "black", alpha = 0.5) +  # Black scatter points
    geom_smooth(method = "lm", color = line_color, se = TRUE) +  # Custom trend line color
    labs(title = title, x = x_label, y = y_label) +
    theme(
      plot.title = element_text(size = 24, face = "bold"),  # Title size
      axis.title.x = element_text(size = 20),  # X-axis label size
      axis.title.y = element_text(size = 20),  # Y-axis label size
      axis.text.x = element_text(size = 18),  # X-axis tick labels
      axis.text.y = element_text(size = 18)   # Y-axis tick labels
    )
}

# Generate individual plots
p1 <- plot_correlation("Secchi_m", "Turbidity_NTU", Merge,
                       "Secchi Depth vs Turbidity",
                       "Secchi Depth (m)", "Turbidity (NTU)", "#F8766D")  # Red-like ggplot default

p2 <- plot_correlation("Secchi_m", "DO_mgL", Merge,
                       "Secchi Depth vs DO",
                       "Secchi Depth (m)", "Dissolved Oxygen (mg/L)", "#00BFC4")  # Blue-like ggplot default

# Arrange plots in a grid (Ensuring correct layout)
combined_plot <- grid.arrange(p1, p2, ncol = 2)  # Horizontal layout

# Save the arranged plots as an image
ggsave("Correlations.png", plot = combined_plot, dpi = 300, width = 12, height = 8)
