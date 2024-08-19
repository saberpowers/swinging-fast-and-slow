
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
  dplyr::filter(!is.na(balls), !is.na(strikes), !is.na(bat_speed), !is.na(swing_length)) |>
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


# Write results to file ----

saveRDS(intention_model, file = "models/intention.rds")
saveRDS(approach_model, file = "models/approach.rds")
