
devtools::load_all("package/swingfastslow")

# Load data and upstream models ----

swing <- data.table::fread("data/swing.csv")
pitch_outcome_model <- readRDS("models/pitch_outcome_model.rds")
hit_outcome_model <- readRDS("models/hit_outcome_model.rds")

# Estimate intention model ----

data_intention <- swing |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics() |>
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
  predict_pitch_hit_outcomes(pitch_outcome_model = pitch_outcome_model, hit_outcome_model = hit_outcome_model) |>
  dplyr::inner_join(intention_model$approach, by = "batter_side_id")

approach_model <- fit_approach_model(data_approach)
