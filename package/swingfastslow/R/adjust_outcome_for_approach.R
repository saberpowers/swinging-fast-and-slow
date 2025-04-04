#' Adjust pitch outcome predictions for batter approach
#' 
#' This function takes a table of n pitches (expressed as outcome predictions for `prob_contact`,
#' `prob_fair` and `pred_hit`) and a table of m approaches (expressed as `strikes_bat_speed` and
#' `strikes_swing_length`) and returns a table of n * m predictions, one prediction for each pitch
#' for each approach.
#' 
#' @param approach a table with columns `strikes_bat_speed` and `strikes_swing_length`
#' @param pred_outcome_pitch a table with columns `prob_contact`, `prob_fair` and `pred_hit`
#' @param causal_model a list of fitted GLM models from \code{\link{fit_causal_model}}
#' 
#' @returns a table with approach-adjusted `prob_contact`, `prob_fair` and `pred_hit`
#' 
#' @export
#' 
adjust_outcome_for_approach <- function(approach,
                                        pred_outcome_pitch,
                                        causal_model) {

  pred_outcome_pitch_expanded <- pred_outcome_pitch |>
    dplyr::mutate(join = TRUE) |>   # get all combinations of one row from each table
    dplyr::left_join(
      y = dplyr::mutate(approach, join = TRUE),
      by = "join",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      approach_bat_speed = strikes * strikes_bat_speed,
      approach_swing_length = strikes * strikes_swing_length
    )
  
  pred_outcome_pitch_adjusted <- pred_outcome_pitch_expanded |>
    dplyr::mutate(
      prob_contact = predict(
        object = causal_model$fit_contact,
        newdata = pred_outcome_pitch_expanded,
        type = "response"
      ),
      prob_fair = predict(
        object = causal_model$fit_fair,
        newdata = pred_outcome_pitch_expanded,
        type = "response"
      ),
      pred_hit = predict(
        object = causal_model$fit_hit,
        newdata = pred_outcome_pitch_expanded,
        type = "response"
      )
    )

  return(pred_outcome_pitch_adjusted)
}
