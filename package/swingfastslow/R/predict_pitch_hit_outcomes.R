#' Predict pitch outcomes and hit outcomes
#'  
#' This function takes a dataframe of pitch tracking and hit tracking data and appends columns
#' of predictions from a provided pitch outcome model and hit outcome model.
#' 
#' @param data a dataframe with at least columns `balls`, `strikes`, `is_rhb`, `strike_zone_top`,
#'   `strike_zone_bottom`, `x0` and `z0` (for pitch outcome predictions) and columns
#'   `launch_speed`, `launch_angle` and `hit_bearing` (for hit oucome predictions)
#' @param pitch_outcome_model an object of class "pitch_outcome_model" returned by
#'   `predpitchscore::train_pitch_outcome_model`
#' @param hit_outcome_model an object of class "xgb.Booster" returned by
#'   `predpitchscore::train_hit_outcome_model`
#' 
#' @returns a dataframe with the same rows and columns as `data`, with the following columns added:
#' \itemize{
#'   \item{"fit_bat_speed"}{a fitted lme4 model for intended bat speed}
#'   \item{"fit_swing_length"}{a fitted lme4 model for intended swing length}
#'   \item{"approach"}{a dataframe summarizing estimated batter approach}
#'   \item{"adaptation"}{a dataframe summarizing estimated batter adaptation}
#'   \item{"timing"}{a dataframe summarizing estimated batter timing}
#' }
#'
#' @references https://github.com/saberpowers/predictive-pitch-score
#' 
#' @export
predict_pitch_hit_outcomes <- function(data, pitch_outcome_model, hit_outcome_model) {

  data_enhanced <- data

  # Derive required columns if missing ----

  if (!"is_rhb" %in% colnames(data)) {
    if (!"bat_side" %in% colnames(data)) {
      stop("`data` must include column `is_rhb` or `bat_side`")
    }
    data_enhanced <- data_enhanced |>
      dplyr::mutate(is_rhb = bat_side == "R")
  }
  if (!"hit_bearing" %in% colnames(data)) {
    if (!all(c("hit_coord_x", "hit_coord_y") %in% colnames(data))) {
      stop("`data` must include column(s) `hit_bearing` or (`hit_coord_x` and `hit_coord_y`)")
    }
    data_enhanced <- data_enhanced |>
      dplyr::mutate(hit_bearing = (atan((hit_coord_x - 125) / (205 - hit_coord_y))) * 180 / pi)
  }

  # Predict hit outcomes ----
  # We start with hit outcomes because prediction is faster,
  # so if there is an error, we find out faster

  is_hit <- with(data_enhanced,
    description == "hit_into_play" &
    !is.na(launch_speed) &
    !is.na(launch_angle) &
    !is.na(hit_bearing)
  )

  pred_outcome_hit <- xgboost:::predict.xgb.Booster(
    object = hit_outcome_model,
    newdata = data_enhanced |>
      dplyr::filter(is_hit) |>
      dplyr::select(launch_speed, launch_angle, hit_bearing) |>
      as.matrix()
  )

  data_enhanced$hit_pred <- NA
  data_enhanced$hit_pred[is_hit] <- pred_outcome_hit

  # Predict pitch outcomes ----

  pred_outcome_pitch <- predpitchscore:::predict.pitch_outcome_model(
    object = pitch_outcome_model,
    newpitch = data_enhanced
  )

  data_enhanced <- dplyr::bind_cols(data_enhanced, pred_outcome_pitch) |>
    dplyr::select(
      dplyr::all_of(colnames(data)),
      dplyr::all_of(colnames(pred_outcome_pitch)),
      hit_pred
    )

  return(data_enhanced)
}
