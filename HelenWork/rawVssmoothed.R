# Prepare data for FCRE comparison
fcre_raw_secchi <- fcre_Combined %>%
  select(datetime, Secchi_m_raw = Secchi_m)

fcre_smoothed_secchi <- fcre_smoothed %>%
  select(datetime, Secchi_m_smooth = Secchi_m)

# Join
fcre_secchi_compare <- full_join(fcre_raw_secchi, fcre_smoothed_secchi, by = "datetime")

# Plot
ggplot(fcre_secchi_compare, aes(x = datetime)) +
  geom_point(aes(y = Secchi_m_raw, color = "Raw Data"), size = 1.2) +
  geom_line(aes(y = Secchi_m_smooth, color = "Smoothed (LOESS)"), size = 1) +
  scale_color_manual(values = c("Raw Data" = "blue", "Smoothed (LOESS)" = "red"),
                     name = "Legend") +
  labs(title = "FCRE: Raw vs LOESS-Smoothed Secchi Depth",
       x = "Date", y = "Secchi Depth (m)") +
  theme_minimal() +
  theme(legend.position = "bottom")
