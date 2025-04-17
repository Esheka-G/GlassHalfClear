smooth_daily_loess <- function(df, site_label, span = 0.2) {
  # Make sure datetime is Date and sorted
  df <- df |>
    mutate(datetime = as.Date(datetime)) |>
    arrange(datetime)

  # Extract numeric variable names (excluding depth_m)
  numeric_vars <- df |>
    select(where(is.numeric)) |>
    select(-any_of("depth_m")) |>
    names()

  # Create full daily date grid
  daily_grid <- tibble(datetime = seq.Date(min(df$datetime), max(df$datetime), by = "day"))

  # Fit and predict loess for each variable
  for (var in numeric_vars) {
    model <- loess(as.formula(paste(var, "~ as.numeric(datetime)")), data = df, span = span)
    daily_grid[[var]] <- predict(model, newdata = data.frame(datetime = as.numeric(daily_grid$datetime)))
  }

  # Add site label
  daily_grid$site_id <- site_label

  return(daily_grid)
}
fcre_smoothed <- smooth_daily_loess(fcre_Combined, site_label = "fcre")
bvre_smoothed <- smooth_daily_loess(bvre_Combined, site_label = "bvre")



