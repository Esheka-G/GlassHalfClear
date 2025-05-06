# AirTemp leads Secchi — correct order
ccf_data_temp <- fcre_Combined %>%
  filter(!is.na(Secchi_m), !is.na(AirTemp_C_mean)) %>%
  arrange(datetime)

secchi_vec <- ccf_data_temp$Secchi_m
airtemp_vec <- ccf_data_temp$AirTemp_C_mean

# Correct CCF: Secchi first
ccf_result_temp <- ccf(secchi_vec, airtemp_vec, lag.max = 30, plot = TRUE,
                       main = "CCF: Secchi vs Air Temperature")

lags_temp <- ccf_result_temp$lag
acf_temp <- ccf_result_temp$acf

neg_lag_idx_t <- which(lags_temp < 0)
best_lag_temp <- lags_temp[neg_lag_idx_t][which.max(abs(acf_temp[neg_lag_idx_t]))]
best_corr_temp <- acf_temp[lags_temp == best_lag_temp]

cat("Best lag (AirTemp leads Secchi):", abs(best_lag_temp),"days \n")
cat("Correlation:", round(best_corr_temp, 3), "\n")
