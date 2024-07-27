
devtools::load_all("package/swingfastslow")

# Load data and upstream models ----

data <- data.table::fread("data/baseballsavant.csv")
linear_weight <- data.table::fread("models/linear_weight.csv")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
hit_outcome_model <- readRDS("models/hit_outcome_model.rds")

data_with_pred <- data |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics() |>
  predict_pitch_hit_outcomes(
    pitch_outcome_model = pitch_outcome_model,
    hit_outcome_model = hit_outcome_model
  )


# Estimate intention model ----

data_intention <- data_with_pred |>
  dplyr::filter(!is.na(bat_speed), !is.na(swing_length)) |>
  remove_partial_swings() |>
  recreate_squared_up() |>
  dplyr::mutate(
    batter_side_id = paste0(batter_id, bat_side),
    plate_x_ref = ifelse(bat_side == "R", plate_x, -plate_x),
    is_contact = description %in% c("foul", "hit_into_play"),
    is_fair = description == "hit_into_play"
  )

intention_model <- fit_intention_model(data_intention)


# Estimate approach model ----

data_approach <- data_intention |>
  dplyr::inner_join(intention_model$approach, by = "batter_side_id")

approach_model <- fit_approach_model(data_approach)


# Caculate the run value of different approaches ----

approach_grid <- intention_model$approach |>
  with(
    data.frame(
      strikes_bat_speed = c(-3:3, -1, 1) * sd(strikes_bat_speed),
      strikes_swing_length = c(-3:3, 1, -1) * sd(strikes_swing_length)
    )
  )

pred_outcome_pitch_adjusted <- adjust_outcome_for_approach(
  pred_outcome_pitch = data_with_pred,
  approach = approach_grid,
  approach_model = approach_model
)

# Print a pitch-level summary of approach effects
pred_outcome_pitch_adjusted |>
  dplyr::group_by(bat_speed = strikes_bat_speed, swing_length = strikes_swing_length, strikes) |>
  dplyr::summarize(
    mean_prob_contact = weighted.mean(prob_contact, w = prob_swing),
    mean_pred_hit = weighted.mean(pred_hit, w = prob_swing * prob_contact * prob_fair),
    .groups = "drop"
  ) |>
  print(n = 30)

# Calculate a plate-appearance level summary of approach effects
approach_run_value <- pred_outcome_pitch_adjusted |>
  dplyr::group_by(strikes_bat_speed, strikes_swing_length, balls, strikes) |>
  dplyr::summarize(   # NOTE: This part should probably be functionized
    mean_prob_hbp = mean((1 - prob_swing) * prob_hbp),
    mean_prob_ball = mean((1 - prob_swing) * (1 - prob_hbp) * (1 - prob_strike)),
    mean_prob_strike = mean((1 - prob_swing) * (1 - prob_hbp) * prob_strike + prob_swing * (1 - prob_contact)),
    mean_prob_foul = mean(prob_swing * prob_contact * (1 - prob_fair)),
    mean_prob_fair = mean(prob_swing * prob_contact * prob_fair),
    mean_pred_hit = weighted.mean(pred_hit, w = prob_swing * prob_contact * prob_fair),
    .groups = "drop"
  ) |>
  dplyr::group_by(strikes_bat_speed, strikes_swing_length) |>
  calculate_pred_outcome_pa(linear_weight = linear_weight)
