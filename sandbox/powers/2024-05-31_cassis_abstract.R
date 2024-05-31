
savant <- read.csv("data/savant.csv")

swing <- savant |>
  dplyr::mutate(

    # First we have to calculate the speed of the pitch at the plate, to reproduce squared-up rate
    cx = release_pos_x,
    cy = release_pos_y,
    cz = release_pos_z,
    # y0 is the value of y at the time when y = 50 (so it is defintionally 50)
    y0 = 50,
    # t0 is the time corresponding to vx0, vy0, vz0, x0, z0 (i.e. the time when y = 50)
    # Calculate time from y0 to release point, and then negate that time
    t0 = -(-vy0 - sqrt(vy0^2 - 4 * (ay / 2) * (y0 - cy))) / (2 * (ay / 2)),
    # Backtrack velocities by t0 time
    bx = vx0 + (-t0) * ax,
    by = vy0 + (-t0) * ay,
    bz = vz0 + (-t0) * az,
    plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
    plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
    plate_speed = 0.6818182 * sqrt((ax * plate_time + bx)^2 + (ay * plate_time + by)^2 + (az * plate_time + bz)^2),
    squared_up = ifelse(
      test = description == "hit_into_play" & !is.na(launch_speed),
      yes = (launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) > 0.8,
      no = FALSE
    ),

    # Infer spray angle from hit coordinates, and reflect spray angle and plate x for LHBs
    spray_angle = (atan((hc_x - 125) / (205 - hc_y))) * 180 / pi, # radian to degree
    spray_angle_ref = ifelse(stand == "L", -spray_angle, spray_angle),
    plate_x_ref = ifelse(stand == "L", -plate_x, plate_x),
    is_bunt = stringr::str_detect(description, "bunt") |
      (description == "hit_into_play" & stringr::str_detect(des, " bunt")),

    pitch_type_cat = dplyr::case_when(
      pitch_type %in% c("FF", "SI") ~ "fastball",
      pitch_type %in% c("SL", "FC", "CU", "ST", "KC", "SV") ~ "breaking",
      pitch_type %in% c("CH", "FS") ~ "offspeed",
    ),
    count = paste0(balls, "-", strikes),
    xwoba = estimated_woba_using_speedangle,  # just creating a shorter variable name here
    batter_id = paste0(batter, stand)   # treat switch hitters as two players
  ) |>
  dplyr::filter(
    !is.na(swing_length),
    !is.na(pitch_type_cat),
    !is_bunt,
    bat_speed > 50
  ) |>
  dplyr::ungroup()

player_name <- swing |>
  dplyr::distinct(batter_id, player_name)

squared_up_data <- swing |>
  dplyr::filter(squared_up) |>
  dplyr::group_by(batter_id) |>
  dplyr::filter(dplyr::n() > 30) |>
  dplyr::ungroup()

squared_up_bat_speed <- lme4::lmer(
  bat_speed ~ balls + strikes + plate_x_ref + plate_z + (1 + strikes + plate_x_ref + plate_z | batter_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = squared_up_data
)

squared_up_swing_length <- lme4::lmer(
  swing_length ~ balls + strikes + plate_x_ref + plate_z + (1 + strikes + plate_x_ref + plate_z | batter_id),
  control = lme4::lmerControl(optimizer = "bobyqa"),
  data = squared_up_data
)


# Measuring approach (variation that co-varies with count) ----

approach <- tibble::tibble(
  batter_id = rownames(lme4::ranef(squared_up_swing_length)$batter_id),
  strikes_bat_speed = lme4::fixef(squared_up_bat_speed)["strikes"] +
    lme4::ranef(squared_up_bat_speed)$batter_id[, "strikes"],
  strikes_swing_length = lme4::fixef(squared_up_swing_length)["strikes"] +
    lme4::ranef(squared_up_swing_length)$batter_id[, "strikes"]
)

# Measuring swing adaptation (variation that co-varies with pitch location) ----

num_points <- 1000
plate_grid <- swing |>
  dplyr::slice(
    rep(sample(1:dplyr::n(), size = num_points), times = nrow(approach))
  ) |>
  dplyr::select(plate_x_ref, plate_z) |>
  dplyr::mutate(
    balls = 0,
    strikes = 0,
    batter_id = rep(approach$batter_id, each = num_points)
  )
adaptation <- plate_grid |>
  dplyr::mutate(
    intended_bat_speed = predict(squared_up_bat_speed, newdata = plate_grid),
    intended_swing_length = predict(squared_up_swing_length, newdata = plate_grid)
  ) |>
  dplyr::group_by(batter_id) |>
  dplyr::summarize(
    var_bat_speed = var(intended_bat_speed),
    var_swing_length = var(intended_swing_length),
    .groups = "drop"
  )


# Measuring timing (unexplained variation) ----

swing_subset <- swing |>
  dplyr::group_by(batter_id) |>
  dplyr::filter(sum(squared_up) > 30) |>
  dplyr::ungroup()

timing <- swing_subset |>
  dplyr::mutate(
    intended_bat_speed = predict(squared_up_bat_speed, newdata = swing_subset),
    intended_swing_length = predict(squared_up_swing_length, newdata = swing_subset),
    residual_bat_speed = bat_speed - intended_bat_speed,
    residual_swing_length = swing_length - intended_swing_length
  )

timing_summary <- timing |>
  dplyr::group_by(batter_id) |>
  dplyr::summarize(
    var_bat_speed = var(residual_bat_speed),
    var_swing_length = var(residual_swing_length)
  )


# Plotting ----

# What is the relationship between intended bat speed and intended swing length?
{
  ppi <- 300
  png("figure/intention.png", width = 5 * ppi, height = 4 * ppi, res = ppi)
  plot_intention <- timing |>
    ggplot2::ggplot(ggplot2::aes(x = intended_swing_length, y = intended_bat_speed)) +
    ggplot2::geom_hex(bins = 50) +
    ggplot2::geom_smooth(formula = y ~ s(x, bs = "cs"), method = "gam", color = "darkorange") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.position = c(0.1, 0.77)
    ) +
    ggplot2::labs(
      x = "Intended Swing Length (feet)",
      y = "Intended Bat Speed (mph)",
      title = "Real variation in swing mechanics"
    )
  print(plot_intention)
  dev.off()
}

# What is the relationship between residual swing length (aka timing?) and residual bat speed?
{
  ppi <- 300
  png("figure/residual.png", width = 5 * ppi, height = 4 * ppi, res = ppi)
  plot_residual <- timing |>
    ggplot2::ggplot(ggplot2::aes(x = residual_swing_length, y = residual_bat_speed)) +
    ggplot2::geom_hex(bins = 50) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::geom_smooth(formula = y ~ s(x, bs = "cs"), method = "gam", color = "darkorange") +
    ggplot2::xlim(c(-2.5, 2.5)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.position = c(0.1, 0.77)
    ) +
    ggplot2::labs(
      x = "Residual Swing Length (feet), a proxy for timing",
      y = "Residual Bat Speed (mph)",
      title = "Artifactual variation in swing mechanics"
    )
  print(plot_residual)
  dev.off()
}
