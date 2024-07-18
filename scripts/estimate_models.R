
devtools::load_all("package/swingfastslow")
swing <- data.table::fread("data/swing.csv")

data <- swing |>
  remove_partial_swings() |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  dplyr::mutate(
    batter_side_id = paste0(batter_id, bat_side),
    plate_x_ref = ifelse(bat_side == "R", plate_x, -plate_x),
    plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
    plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
    plate_speed = 0.6818182 * sqrt(
      (ax * plate_time + bx)^2 + (ay * plate_time + by)^2 + (az * plate_time + bz)^2
    ),
    squared_up = ifelse(
      test = description == "hit_into_play" & !is.na(launch_speed),
      yes = (launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) > 0.8,
      no = FALSE
    )
  )

intention_model <- fit_intention_model(data)
