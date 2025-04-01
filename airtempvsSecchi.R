# Load required libraries
library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(fuzzyjoin)
SecchiDepth_org <- SecchiDepth %>% arrange(DateTime)


# 🟢 Read the JSON file into an R dataframe
weather_data <- fromJSON("weather_data.json")

# Convert JSON data to a dataframe
weather_df <- as.data.frame(do.call(rbind, weather_data))

# Add the DateTime column (rownames are the dates in JSON)
weather_df$DateTime <- rownames(weather_df)

# Convert DateTime to Date format
weather_df$DateTime <- as.Date(weather_df$DateTime, format="%Y-%m-%d")

# Convert temperature column to numeric
weather_df$`Avg Temp (°C)` <- as.numeric(weather_df$`Avg Temp (°C)`)



# 🟢 Ensure DateTime in SecchiDepth_org is in the same format
SecchiDepth_org$DateTime <- as.Date(SecchiDepth_org$DateTime, format="%Y-%m-%d")

# 🟢 Perform a fuzzy join (allowing for closest date match)
merged_df <- difference_join(
  SecchiDepth_org, weather_df,
  by = "DateTime",
  mode = "left",
  max_dist = 3,  # Allow a maximum of 3 days difference for matching
  distance_col = "Date_Difference"
)

ggplot(merged_df, aes(x=`Avg Temp (°C)`, y=Secchi_m)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", color="red") +
  labs(title="Temperature vs. Secchi Depth",
       x="Average Temperature (°C)",
       y="Secchi Depth (meters)") +
  theme_minimal()

# Load required libraries
library(dplyr)
library(ggplot2)

# Group by Average Temperature and compute mean Secchi Depth
summary_df <- merged_df %>%
  group_by(`Avg Temp (°C)`) %>%
  summarise(Secchi_m = mean(Secchi_m, na.rm = TRUE))

# Scatter plot with averaged data
ggplot(summary_df, aes(x=`Avg Temp (°C)`, y=Secchi_m)) +
  geom_point(color="black") +  # Scatter plot with averaged values
  geom_smooth(method="lm", color="red") +  # Trend line
  labs(title="Air Temperature vs. Secchi Depth",
       x="Air Temperature (°C)",
       y="Secchi Depth (meters)") +
theme(
  plot.title = element_text(size = 28, face = "bold"),  # Title size
  axis.title.x = element_text(size = 24),  # X-axis label size
  axis.title.y = element_text(size = 24),  # Y-axis label size
  axis.text.x = element_text(size = 20),  # X-axis tick labels
  axis.text.y = element_text(size = 20),  # Y-axis tick labels
  legend.text = element_text(size = 20),  # Legend text size
  legend.title = element_text(size = 22)  # Legend title size
)
ggsave("airtemp.png", dpi = 300, width = 12, height = 8)


# Compute correlation
correlation_result <- cor.test(summary_df$`Avg Temp (°C)`, summary_df$Secchi_m, method = "pearson")+


# Print correlation coefficient and p-value
print(paste("Pearson Correlation: ", round(correlation_result$estimate, 3)))
print(paste("P-value: ", correlation_result$p.value))







