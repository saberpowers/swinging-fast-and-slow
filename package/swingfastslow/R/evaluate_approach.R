#' Evaluate an approach on the run scale
#' 
#' This function takes some predicted pitch outcomes; adjusts them for approach; aggregates the
#' predictions by count; and rolls up the predictions into a run value assuming a Markov model for
#' the progression of a plate apperance. Approach is specified as a dataframe with a
#' `strikes_swing_length` column and a `strikes_bat_speed` column. This function can handle
#' multiple approaches (multiple rows in the approach table), but it can run into memory issues if
#' you try to pass too many (more than a hundred).
#' 
#' @param approach a table with columns `strikes_bat_speed` and `strikes_swing_length`
#' @param pred_outcome_pitch a table with columns `prob_contact`, `prob_fair` and `pred_hit`
#' @param approach_model a list of fitted GLM models from \code{\link{fit_approach_model}}
#' @param linear_weight a table with columns `event` and `linear_weight`
#' 
#' @return a table with columns `strikes_bat_speed`, `strikes_swing_length` and `runs` 
#'
#' @export
#' 
evaluate_approach <- function(approach, pred_outcome_pitch, approach_model, linear_weight) {

  pred_outcome_pitch_adjusted <- adjust_outcome_for_approach(
    approach = approach,
    pred_outcome_pitch = pred_outcome_pitch,
    approach_model = approach_model
  )

  # Calculate a plate-appearance level summary of approach effects
  approach_runs <- pred_outcome_pitch_adjusted |>
    dplyr::group_by(strikes_bat_speed, strikes_swing_length, balls, strikes) |>
    summarize_pitch_outcome() |>
    dplyr::group_by(strikes_bat_speed, strikes_swing_length) |>
    calculate_pred_outcome_pa(linear_weight = linear_weight)
  
  approach_runs
}
