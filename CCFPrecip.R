# Precip leads Secchi — correct order
ccf_data_precip <- fcre_Combined %>%
  filter(!is.na(Secchi_m), !is.na(Rain_mm_sum)) %>%
  arrange(datetime)

secchi_vec <- ccf_data_precip$Secchi_m
precip_vec <- ccf_data_precip$Rain_mm_sum

# Correct CCF: Secchi first
ccf_result_precip <- ccf(secchi_vec, precip_vec, lag.max = 30, plot = TRUE,
                         main = "CCF: Secchi vs Precipitation")

lags_precip <- ccf_result_precip$lag
acf_precip <- ccf_result_precip$acf

neg_lag_idx_p <- which(lags_precip < 0)
best_lag_precip <- lags_precip[neg_lag_idx_p][which.max(abs(acf_precip[neg_lag_idx_p]))]
best_corr_precip <- acf_precip[lags_precip == best_lag_precip]

cat("Best lag (Precip leads Secchi):", abs(best_lag_precip), "days\n")
cat("Correlation:", round(best_corr_precip, 3), "\n")


