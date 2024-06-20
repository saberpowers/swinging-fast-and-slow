
pitch <- data.table::fread("data/pitch/mlb/2024.csv")

pitch_outcome_model <- readRDS("model/pitch_outcome_model.rds")
hit_outcome_model <- readRDS("model/hit_outcome_model.rds")

swing <- data.table::fread("data/swing/mlb/2024.csv") |>
  dplyr::mutate(

    game_id = game_pk,
    event_index = at_bat_number - 1,
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
    hit_bearing = (atan((hc_x - 125) / (205 - hc_y))) * 180 / pi, # radian to degree
    plate_x_ref = ifelse(stand == "L", -plate_x, plate_x),
    is_bunt = stringr::str_detect(description, "bunt") |
      (description == "hit_into_play" & stringr::str_detect(des, " bunt")),
    is_rhb = stand == "R",
    is_contact = description %in% c("foul", "hit_into_play"),

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
  dplyr::ungroup() |>
  dplyr::select(
    game_id, event_index, pitch_number, batter_id, player_name, is_rhb,
    is_contact, squared_up, bat_speed, swing_length, plate_x, plate_x_ref, plate_z, hit_bearing
  )

data <- swing |>
  dplyr::inner_join(pitch, by = c("game_id", "event_index", "pitch_number"))

pitch_pred <- predpitchscore:::predict.pitch_outcome_model(pitch_outcome_model, newpitch = data)
data$prob_contact <- pitch_pred$prob_contact
data$pred_damage <- pitch_pred$pred_hit

data$damage <- predict(
  object = hit_outcome_model,
  newdata = data |>
    dplyr::select(launch_speed, launch_angle, hit_bearing) |>
    as.matrix()
)




player_name <- data |>
  dplyr::distinct(batter_id, player_name)

squared_up_data <- data |>
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

approach <- tibble::tibble(
  batter_id = rownames(lme4::ranef(squared_up_swing_length)$batter_id),
  strikes_bat_speed = lme4::fixef(squared_up_bat_speed)["strikes"] +
    lme4::ranef(squared_up_bat_speed)$batter_id[, "strikes"],
  strikes_swing_length = lme4::fixef(squared_up_swing_length)["strikes"] +
    lme4::ranef(squared_up_swing_length)$batter_id[, "strikes"]
)

contact <- data |>
  dplyr::group_by(strikes, batter_id) |>
  dplyr::summarize(
    n_contact = dplyr::n(),
    mean_contact = mean(is_contact),
    pred_contact = mean(prob_contact),
    delta_contact = mean_contact - pred_contact,
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(cols = dplyr::ends_with("_contact")) |>
  dplyr::mutate(name = glue::glue("{name}_{strikes}")) |>
  dplyr::select(-strikes) |>
  tidyr::pivot_wider() |>
  dplyr::mutate(delta_contact_delta = delta_contact_2 - delta_contact_0) |>
  dplyr::select(batter_id, n_contact_0, n_contact_2, delta_contact_delta)

damage <- data |>
  dplyr::group_by(strikes, batter_id) |>
  dplyr::summarize(
    n_damage = dplyr::n(),
    mean_damage = mean(damage),
    pred_damage = mean(pred_damage),
    delta_damage = mean_damage - pred_damage,
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(cols = dplyr::ends_with("_damage")) |>
  dplyr::mutate(name = glue::glue("{name}_{strikes}")) |>
  dplyr::select(-strikes) |>
  tidyr::pivot_wider() |>
  dplyr::mutate(delta_damage_delta = delta_damage_2 - delta_damage_0) |>
  dplyr::select(batter_id, n_damage_0, n_damage_2, delta_damage_delta)

data_approach <- approach |>
  dplyr::inner_join(contact, by = "batter_id") |>
  dplyr::inner_join(damage, by = "batter_id")

with(data_approach, lm(delta_contact_delta ~ strikes_bat_speed))
with(data_approach, lm(delta_damage_delta ~ strikes_bat_speed))
with(data_approach, lm(delta_contact_delta ~ strikes_swing_length))
with(data_approach, lm(delta_damage_delta ~ strikes_swing_length))

{
  ppi <- 300
  png("figure/approach_speed_contact.png", width = 7 * ppi, height = 7 * ppi, res = ppi)
  plot_approach_speed_contact <- data_approach |>
    dplyr::left_join(player_name, by = "batter_id") |>
    ggplot2::ggplot(ggplot2::aes(x = strikes_bat_speed, y = delta_contact_delta)) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::geom_text(ggplot2::aes(label = player_name), size = 2) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = "Batters who slow their swings with two strikes make more contact",
      subtitle = "Contact expectation is based on count and pitch charateristics",
      x = "Change in bat speed per strike added to count (mph)",
      y = "Change in contact above expectation in 2K counts vs. 0K counts",
    )
  print(plot_approach_speed_contact)
  dev.off()
}

{
  ppi <- 300
  png("figure/approach_speed_damage.png", width = 7 * ppi, height = 7 * ppi, res = ppi)
  plot_approach_speed_damage <- data_approach |>
    dplyr::left_join(player_name, by = "batter_id") |>
    ggplot2::ggplot(ggplot2::aes(x = strikes_bat_speed, y = delta_damage_delta)) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::geom_text(ggplot2::aes(label = player_name), size = 2) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = "Batters who slow their swings with two strikes do less damage",
      subtitle = "Damage expectation is based on count and pitch charateristics",
      x = "Change in bat speed per strike added to count (mph)",
      y = "Change in xwOBA above expectation in 2K counts vs. 0K counts",
    )
  print(plot_approach_speed_damage)
  dev.off()
}

{
  ppi <- 300
  png("figure/approach_length_contact.png", width = 7 * ppi, height = 7 * ppi, res = ppi)
  plot_approach_length_contact <- data_approach |>
    dplyr::left_join(player_name, by = "batter_id") |>
    ggplot2::ggplot(ggplot2::aes(x = strikes_swing_length, y = delta_contact_delta)) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::geom_text(ggplot2::aes(label = player_name), size = 2) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = "Batters who shorten their swings with two strikes make more contact",
      subtitle = "Contact expectation is based on count and pitch charateristics",
      x = "Change in swing length per strike added to count (ft)",
      y = "Change in contact above expectation in 2K counts vs. 0K counts",
    )
  print(plot_approach_length_contact)
  dev.off()
}

{
  ppi <- 300
  png("figure/approach_length_damage.png", width = 7 * ppi, height = 7 * ppi, res = ppi)
  plot_approach_length_damage <- data_approach |>
    dplyr::left_join(player_name, by = "batter_id") |>
    ggplot2::ggplot(ggplot2::aes(x = strikes_swing_length, y = delta_damage_delta)) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::geom_text(ggplot2::aes(label = player_name), size = 2) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = "Batters who shorten their swings with two strikes do less damage",
      subtitle = "Damage expectation is based on count and pitch charateristics",
      x = "Change in swing length per strike added to count (ft)",
      y = "Change in xwOBA above expectation in 2K counts vs. 0K counts",
    )
  print(plot_approach_length_damage)
  dev.off()
}
