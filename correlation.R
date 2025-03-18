Secchi50 <- Secchi50 |>
  mutate(Normalized_Secchi = (Secchi_m - min(Secchi_m, na.rm = TRUE)) /
         (max(Secchi_m, na.rm = TRUE) - min(Secchi_m, na.rm = TRUE)))

Merge <- merge(Secchi50, CTD, by = "DateTime")


# Correlation of Secchi Depth and Turbity
x <- Merge$Normalized_Secchi
y <- Merge$Turbidity_NTU

correlation <- cor(x, y, use = "complete.obs")
print(correlation)

