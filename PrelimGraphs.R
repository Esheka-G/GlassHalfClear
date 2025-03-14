# Temperature Profile by Depth
ggplot(SensorData, aes(x = Temp_C, y = Depth_m, color = Reservoir)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_y_reverse() +
  labs(title = "Temperature Profile by Depth", x = "Temperature (°C)", y = "Depth (m)") +
  theme_minimal()

# Dissolved Oxygen By Depth
ggplot(SensorData, aes(x = DO_mgL, y = Depth_m, color = Reservoir)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_y_reverse() +
  labs(title = "Dissolved Oxygen by Depth", x = "DO (mg/L)", y = "Depth (m)") +
  theme_minimal()


# Nutrient Levels Over Time(TN and TP)
ggplot(ChemistryData, aes(x = DateTime)) +
  geom_line(aes(y = TN_ugL, color = "Total Nitrogen")) +
  geom_line(aes(y = TP_ugL, color = "Total Phosphorus")) +
  facet_wrap(~Reservoir, scales = "free_y") +
  labs(title = "Nutrient Levels Over Time", x = "Date", y = "Concentration (ug/L)") +
  scale_color_manual(values = c("Total Nitrogen" = "blue", "Total Phosphorus" = "red")) +
  theme_minimal()


# Boxplot of pH Levels
ggplot(SensorData, aes(x = Reservoir, y = pH, fill = Reservoir)) +
  geom_boxplot() +
  labs(title = "pH Levels in BVR vs. FCR", x = "Reservoir", y = "pH") +
  theme_minimal()





