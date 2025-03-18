Merge <- merge(Secchi50, CTD, by = "DateTime")


# Correlation of Secchi Depth and Turbity
x <- Merge$Secchi_m
y <- Merge$Turbidity_NTU

correlation <- cor(x, y, use = "complete.obs")
print(correlation)



