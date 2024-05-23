
savant <- read.csv("data/savant.csv")

dodger_blue <- rgb(0.1176471, 0.5647059, 1, alpha = 0.5)
dark_orange <- rgb(1, 0.5490196, 0, alpha = 0.5)
gray <- rgb(0.745098, 0.745098, 0.745098, alpha = 0.5)

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

    pitch_type_cat = dplyr::case_when(
      pitch_type %in% c("FF", "SI") ~ "fastball",
      pitch_type %in% c("SL", "FC", "CU", "ST", "KC", "SV") ~ "breaking",
      pitch_type %in% c("CH", "FS") ~ "offspeed",
    ),
    xwoba = estimated_woba_using_speedangle   # just creating a shorter variable name here
  ) |>
  dplyr::filter(
    !is.na(swing_length),
    !is.na(pitch_type_cat),
    !grepl("bunt", description)
  ) |>
  # Filter out bottom 10% of swings by each batter, according to bat speed (this is what MLB does)
  dplyr::group_by(batter) |>
  dplyr::filter(bat_speed > quantile(bat_speed, probs = 0.1, na.rm = TRUE)) |>
  dplyr::mutate(
    bat_speed_delta = bat_speed - mean(bat_speed),
    swing_length_delta = swing_length - mean(swing_length),
  ) |>
  dplyr::ungroup()


squared_up_data <- swing |>
  dplyr::filter(squared_up) |>
  dplyr::mutate(batter_id = paste0(batter, stand)) |>   # treat switch hitters as two players
  dplyr::group_by(batter) |>
  dplyr::filter(dplyr::n() > 30) |>
  dplyr::ungroup()

squared_up_model <- squared_up_data |>
  with(lme4::lmer(swing_length ~ plate_x_ref + plate_z + (1 + plate_x_ref + plate_z | batter_id)))

lme4::ranef(squared_up_model)$batter_id |>
  dplyr::arrange(plate_z) |>
  head()







{
  ppi <- 300
  png("figure/morel_winn.png", width = 10 * ppi, height = 7 * ppi, res = ppi)
  par(mfrow = c(1, 2))


  grid <- expand.grid(
    plate_x_ref = seq(from = -1, to = 1, by = 0.01),
    plate_z = seq(from = 1, to = 4, by = 0.01)
  ) |>
    dplyr::mutate(batter_id = "666624R")
  grid <- grid |>
    dplyr::mutate(pred_swing_length = predict(squared_up_model, newdata = grid)) |>
    with(
      predpitchscore::plot_2d_function(
        x = plate_x_ref,
        y = plate_z,
        z = -pred_swing_length,
        z_min = -9.1,
        z_max = -5.5,
        axes = FALSE,
        xlab = "Swing length by pitch location on squared-up swings",
        ylab = "",
        main = "Christopher Morel"
      )
    )
  squared_up_data |>
    dplyr::filter(batter == 666624) |>  # Christopher Morel
    with(text(plate_x_ref, plate_z, label = sprintf("%.1f", swing_length)))
  # Draw strike zone
  squared_up_data |>
    dplyr::filter(batter == 666624) |>  # Christopher Morel
    with(
      graphics::rect(
        xleft = -17 / 24,
        ybottom = mean(sz_bot),
        xright = 17 / 24,
        ytop = mean(sz_top),
        lty = 2
      )
    )

  grid <- expand.grid(
    plate_x_ref = seq(from = -1, to = 1, by = 0.01),
    plate_z = seq(from = 1, to = 4, by = 0.01)
  ) |>
    dplyr::mutate(batter_id = "691026R")
  grid <- grid |>
    dplyr::mutate(pred_swing_length = predict(squared_up_model, newdata = grid)) |>
    with(
      predpitchscore::plot_2d_function(
        x = plate_x_ref,
        y = plate_z,
        z = -pred_swing_length,
        z_min = -9.1,
        z_max = -5.5,
        axes = FALSE,
        xlab = "Swing length by pitch location on squared-up swings",
        ylab = "",
        main = "Masyn Winn"
      )
    )
  squared_up_data |>
    dplyr::filter(batter == 691026) |>  # Masyn Winn
    with(text(plate_x_ref, plate_z, label = sprintf("%.1f", swing_length)))
  # Draw strike zone
  squared_up_data |>
    dplyr::filter(batter == 691026) |>  # Masyn Winn
    with(
      graphics::rect(
        xleft = -17 / 24,
        ybottom = mean(sz_bot),
        xright = 17 / 24,
        ytop = mean(sz_top),
        lty = 2
      )
    )

  dev.off()
} 
