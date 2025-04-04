
fit_intention_model <- TRUE
fit_causal_model <- TRUE


# Load data and upstream models ----

pitch <- data.table::fread("data/baseballsavant.csv")
linear_weight <- data.table::fread("models/linear_weight.csv")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
hit_outcome_model <- readRDS("models/hit_outcome_model.rds")

data <- pitch |>
  dplyr::filter(balls < 4, strikes < 3) |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics() |>
  swingfastslow::recreate_squared_up() |>
  dplyr::mutate(
    batter_side_id = paste0(batter_id, bat_side),
    plate_x_ref = ifelse(bat_side == "R", plate_x, -plate_x),
    is_contact = description %in% c("foul", "hit_into_play"),
    is_fair = description == "hit_into_play"
  )
  

# Estimate intention model ----

data_intention <- data |>
  dplyr::filter(!is.na(bat_speed), !is.na(swing_length)) |>
  swingfastslow::remove_partial_swings()

if (fit_intention_model) {
  intention_model_bat_speed <- swingfastslow::fit_intention_model(
    data_intention = data_intention,
    swing_metric = "bat_speed"
  )
  intention_model_swing_length <- swingfastslow::fit_intention_model(
    data_intention = data_intention,
    swing_metric = "swing_length"
  )
} else {
  intention_model_bat_speed <- readr::read_rds("models/intent_bat_speed_full.rds")
  intention_model_swing_length <- readr::read_rds("models/intent_swing_length_full.rds")
}

approach <- swingfastslow::get_intention_model_summary(
  intention_model_bat_speed = intention_model_bat_speed,
  intention_model_swing_length = intention_model_swing_length
)


# Estimate causal model ----

if (fit_causal_model) {
  data_causal <- data_intention |>
    dplyr::inner_join(approach, by = "batter_side_id") |>
    swingfastslow::predict_pitch_hit_outcomes(
      pitch_outcome_model = pitch_outcome_model,
      hit_outcome_model = hit_outcome_model
    )
  causal_model <- fit_approach_model(data_causal)
} else {
  causal_model <- readRDS("models/causal.rds")
}


# Write results to file ----

saveRDS(intention_model_bat_speed, file = "models/intent_bat_speed_full.rds")
saveRDS(intention_model_swing_length, file = "models/intent_swing_length_full.rds")
saveRDS(approach_model, file = "models/approach.rds")
