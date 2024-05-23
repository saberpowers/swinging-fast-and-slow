
savant <- read.csv("data/savant.csv")

dodger_blue <- rgb(0.1176471, 0.5647059, 1, alpha = 0.5)
dark_orange <- rgb(1, 0.5490196, 0, alpha = 0.5)
gray <- rgb(0.745098, 0.745098, 0.745098, alpha = 0.5)

# Filter out bottom 10% of swings by each batter, according to bat speed
swing <- savant |>
  dplyr::mutate(
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
    spray_angle = (atan((hc_x - 125) / (205 - hc_y))) * 180 / pi, # radian to degree
    spray_angle_ref = ifelse(stand == "L", -spray_angle, spray_angle),
    plate_x_ref = ifelse(stand == "L", -plate_x, plate_x),
    pitch_type_cat = dplyr::case_when(
      pitch_type %in% c("FF", "SI") ~ "fastball",
      pitch_type %in% c("SL", "FC", "CU", "ST", "KC", "SV") ~ "breaking",
      pitch_type %in% c("CH", "FS") ~ "offspeed",
    ),
    xwoba = estimated_woba_using_speedangle
  ) |>
  dplyr::group_by(batter) |>
  dplyr::filter(
    !is.na(swing_length),
    bat_speed > quantile(bat_speed, probs = 0.1, na.rm = TRUE),
    !is.na(pitch_type_cat),
    !grepl("bunt", description),
  ) |>
  dplyr::mutate(
    bat_speed_delta = bat_speed - mean(bat_speed),
    swing_length_delta = swing_length - mean(swing_length),
  ) |>
  dplyr::ungroup()

model_swing_length <- lme4::lmer(
  log(swing_length) ~ (1 | batter) + release_speed + plate_x_ref + plate_z,
  data = swing
)
residual_variance <- lme4::VarCorr(model_swing_length) |>
  as.data.frame() |>
  dplyr::filter(grp == "Residual") |>
  with(vcov)
swing_pred <- data.frame(
  batter = sort(unique(swing$batter)),
  release_speed = mean(swing$release_speed, na.rm = TRUE),
  plate_x_ref = mean(swing$plate_x_ref),
  plate_z = mean(swing$plate_z)
)
swing_pred$pred_swing_length <- exp(predict(model_swing_length, newdata = swing_pred) + residual_variance / 2)

swing_batter <- swing |>
  dplyr::filter(pitch_type_cat == "fastball") |>
  dplyr::group_by(batter, player_name) |>
  dplyr::summarize(
    n = dplyr::n(),
    mean_bat_speed = mean(bat_speed),
    sd_bat_speed = sd(swing_length),
    mean_swing_length = mean(swing_length),
    sd_swing_length = sd(swing_length),
    mean_contact = mean(!grepl("swinging_strike", description)),
    mean_fair = mean(description == "hit_into_play"),
    mean_squared_up = mean(squared_up),
    .groups = "drop"
  ) |>
  dplyr::left_join(swing_pred, by = "batter") |>
  dplyr::filter(n > 30)

model_squared_up_batter <- swing_batter |>
  with(lm(mean_squared_up ~ mean_bat_speed, w = n))

swing_batter |>
  dplyr::mutate(bat_speed_cat = ifelse(mean_bat_speed > mean(mean_bat_speed), "fast", "slow")) |>
  dplyr::group_by(bat_speed_cat) |>
  dplyr::summarize(
    mean_contact = weighted.mean(mean_contact, w = n),
    mean_fair = weighted.mean(mean_fair, w = n),
    mean_squared_up = weighted.mean(mean_squared_up, w = n),
    .groups = "drop"
  )

batter_squared_up <- swing |>
  dplyr::filter(pitch_type_cat == "fastball") |>
  dplyr::group_by(batter) |>
  dplyr::mutate(bat_speed_cat = ifelse(bat_speed > mean(bat_speed), "fast", "slow")) |>
  dplyr::ungroup() |>
  dplyr::group_by(batter, player_name, bat_speed_cat) |>
  dplyr::summarize(n = dplyr::n(), mean_squared_up = mean(squared_up), .groups = "drop") |>
  tidyr::pivot_longer(cols = dplyr::all_of(c("n", "mean_squared_up"))) |>
  dplyr::mutate(name = paste0(name, "_", bat_speed_cat)) |>
  dplyr::select(-bat_speed_cat) |>
  tidyr::pivot_wider() |>
  dplyr::filter((n_fast + n_slow) > 50)

batter_squared_up |>
  dplyr::mutate(
    weight = 1 / (1 / n_fast + 1 / n_slow)
  ) |>
  dplyr::summarize(
    mean_squared_up_fast = weighted.mean(mean_squared_up_fast, w = weight),
    mean_squared_up_slow = weighted.mean(mean_squared_up_slow, w = weight)
  )

swing |>
  dplyr::filter(pitch_type_cat == "offspeed") |>
  dplyr::group_by(batter) |>
  dplyr::mutate(bat_speed_cat = ifelse(bat_speed > mean(bat_speed), "fast", "slow")) |>
  dplyr::ungroup() |>
  dplyr::group_by(bat_speed_cat) |>
  dplyr::summarize(
    mean_contact = mean(!grepl("swinging_strike", description)),
    mean_fair = mean(description == "hit_into_play"),
    mean_squared_up = mean(squared_up)
  )



{
  ppi <- 300
  png("figures/bat_speed_v_swing_length_not_2k.png", width = 7 * ppi, height = 7 * ppi, res = ppi)
  swing_batter |>
    with(
      plot(
        x = mean_bat_speed,
        y = mean_swing_length,
        col = dodger_blue,
        pch = 16,
        main = "2024 MLB Swing Length vs. Bat Speed (min. 30 competitive swings)",
        xlab = "Average Bat Speed (mph)",
        ylab = "Average Swing Length (ft)",
        axes = FALSE
      )
    )
  axis(1)
  axis(2)
  dev.off()
}

{
  ppi <- 300
  png("figures/cruz.png", width = 7 * ppi, height = 7 * ppi, res = ppi)
  swing |>
    dplyr::filter(batter == 665833) |>
    with(
      plot(
        x = bat_speed,
        y = swing_length,
        col = dark_orange,
        pch = 16,
        main = "2024 (Competitive) Swings: Oneil Cruz",
        xlab = "Bat Speed (mph)",
        ylab = "Swing Length (ft)",
        axes = FALSE
      )
    )
  axis(1)
  axis(2)
  dev.off()
}


{
  ppi <- 300
  png("figures/soto_cruz.png", width = 7 * ppi, height = 7 * ppi, res = ppi)
  swing |>
    dplyr::filter(batter == 665833) |>
    with(
      plot(
        x = bat_speed,
        y = swing_length,
        col = ifelse(grepl("swinging_strike", description), "darkorange", dark_orange),
        pch = ifelse(grepl("swinging_strike", description), 1, 16),
        main = "2024 (Competitive) Swings: Oneil Cruz v. Juan Soto",
        xlab = "Bat Speed (mph)",
        ylab = "Swing Length (ft)",
        axes = FALSE
      )
    )
  swing |>
    dplyr::filter(batter == 665742) |>
    with(
      points(
        x = bat_speed,
        y = swing_length,
        col = ifelse(grepl("swinging_strike", description), "dodgerblue", dodger_blue),
        pch = ifelse(grepl("swinging_strike", description), 1, 16)
      )
    )
  axis(1)
  axis(2)
  legend(
    "bottomright",
    legend = c("Contact", "Miss", "Juan Soto", "Oneil Cruz"),
    pch = c(16, 1, 16, 16),
    col = c("gray", gray, dodger_blue, dark_orange),
    bty = "n"
  )
  dev.off()
}

# Swing length distribution by contact/miss
{
  ppi <- 300
  png("figures/swing_length_by_pitch_type.png", height = 8 * ppi, width = 4 * ppi, res = ppi)
  par(mfrow = c(3, 1))
  for (ptc in c("fastball", "breaking", "offspeed")) {
    swing |>
      dplyr::filter(
        pitch_type_cat == ptc,
        !grepl("swinging_strike", description)
      ) |>
      with(
        plot(
          density(swing_length),
          col = dodger_blue,
          xlim = c(5, 10),
          ylim = c(0, 0.8),
          axes = FALSE,
          main = glue::glue("MLB Distribution of Swing Length on
            {
              dplyr::case_when(
                ptc == 'fastball' ~ 'Fastballs',
                ptc == 'breaking' ~ 'Breaking Balls',
                ptc == 'offspeed' ~ 'Off-speed Pitches'
              )
            }
          "),
          xlab = "",
          ylab = "Density"
        )
      )
    swing |>
      dplyr::filter(
        pitch_type_cat == ptc,
        grepl("swinging_strike", description)
      ) |>
      with(lines(density(swing_length), col = "gray", lty = 2))
    axis(1)
    legend("topleft", c("Contact", "Miss"), col = c("dodgerblue", "gray"), lty = 1:2, bty = "n")
  }
  dev.off()
}

{
  ppi <- 300
  png("figures/adjusted_swing_length.png", height = 7 * ppi, width = 7 * ppi, res = ppi)
  swing_batter |>
    with(
      plot(
        x = mean_swing_length,
        y = pred_swing_length,
        col = dodger_blue,
        pch = 16,
        axes = FALSE,
        xlab = "Average Swing Length (ft)",
        ylab = "Adjusted Swing Length (ft)",
        main = "Swing Length Adjusted for Pitch Characteristics (min. 30 swings)"
      )
    )
    abline(0, 1, col = "gray", lty = 2)
    axis(1)
    axis(2)
  dev.off()
}

{
  ppi <- 300
  png("figures/swinging_fast_and_slow_not2k.png", width = 7 * ppi, height = 7 * ppi, res = ppi)
  batter_squared_up |>
    with(
      plot(
        x = mean_squared_up_fast,
        y = mean_squared_up_slow,
        main = "Swinging, Fast and Slow",
        xlab = "Squared-up Rate on FAST Swings (ABOVE-player-average Bat Speed)",
        ylab = "Squared-up Rate on SLOW Swings (BELOW-player-average Bat Speed)",
        pch = 16,
        col = dodger_blue,
        axes = FALSE
      )
    )
  abline(0, 1, lty = 2, col = "gray")
  axis(1, at = seq(from = 0.1, to = 0.5, by = 0.1), labels = c("10%", "20%", "30%", "40%", "50%"))
  axis(2, at = seq(from = 0.1, to = 0.5, by = 0.1), labels = c("10%", "20%", "30%", "40%", "50%"))
  legend("topleft", c("fastballs only", "min. 50 swings"), bty = "n")
  legend("bottomright", c('squared-up rate is higher on', '"fast" swings for most hitters'), bty = "n")
  dev.off()
}
