library(ggplot2)

# Nitrogen and Phosphourous Levels

NAFixChem <- ChemistryData |>
  na.omit(ChemistryData)


ggplot(NAFixChem, aes(x = DateTime, y = TN_ugL, color = Reservoir)) +
  geom_line() +
  labs(title = "Nirogen Over Time", x = "Temperature (°C)", y = "Depth (m)")

ggplot(NAFixChem, aes(x = DateTime, y = TP_ugL, color = Reservoir)) +
  geom_line() +
  labs(title = "Phosphorous Over Time")


# Turbidity Graph

ggplot(CTD, aes(x = DateTime, y = Turbidity_NTU, color = Reservoir)) +
  geom_line() +
  labs(title = "Turbity over time")

# Secchi Depth

# SecchiDepth Data
ggplot(SecchiDepth, aes(x = DateTime, y = Secchi_m)) +
  geom_line() +
  labs(title = "Secchi Depth vs Time",
       x = "Time",
       y = "Secchi in Meters")


## IF YOU LOOK SECCHI DEPTH HAS GONE DOWN AS TURBITY IS INCREASED SHOWING THE
## NEGATIVE CORRELATION SHICH MAKES SENSE

