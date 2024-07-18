
fit_intention_model <- function(data) {

  bat_speed <- lme4::lmer(
    bat_speed ~ balls + strikes + plate_x_ref + plate_z +
      (1 + strikes + plate_x_ref + plate_z | batter_side_id),
    control = lme4::lmerControl(optimizer = "bobyqa"),
    data = data |>
      dplyr::filter(squared_up)
  )

  swing_length <- lme4::lmer(
    swing_length ~ balls + strikes + plate_x_ref + plate_z + (1 + strikes + plate_x_ref + plate_z | batter_id),
    control = lme4::lmerControl(optimizer = "bobyqa"),
    data = data |>
      dplyr::filter(squared_up)
  )

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
    rep(sample(1:dplyr::n(), size = num_points), times = length(swing_length_strikes))
  ) |>
  dplyr::select(plate_x_ref, plate_z) |>
  dplyr::mutate(
    balls = 0,
    strikes = 0,
    batter_id = rep(names(swing_length_strikes), each = num_points)
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

timing <- swing |>
  dplyr::mutate(
    intended_bat_speed = predict(squared_up_bat_speed, newdata = swing_subset),
    intended_swing_length = predict(squared_up_swing_length, newdata = swing_subset),
    residual_bat_speed = bat_speed - intended_bat_speed,
    residual_swing_length = swing_length - intended_swing_length
  ) |>
  dplyr::group_by(batter_id) |>
  dplyr::summarize(
    var_bat_speed = var(residual_bat_speed),
    var_swing_length = var(residual_swing_length),
    .groups = "drop"
  )

  return(
    list(
      bat_speed = bat_speed,
      swing_length = swing_length,
      approach = approach,
      adaptation = adaptation,
      timing = timing
    )
  )
}
