#' Summarize pitch outcome predictions
#' 
#' Given pre-specified group variables, this function summarizes the average pitch outcome within
#' each group. Group variables are dropped in the returned table. Whereas pitch outcome predictions
#' pertain to conditional probabilities (e.g. probability of contact given swing), the summary
#' probabilities pertain to the multinomial outcome of the pitch (hbp, ball, strike, foul, fair and
#' expected run value given fair).
#' 
#' @param pred_outcome_pitch a table with columns `prob_swing`, `prob_hpb`, `prob_strike`,
#'   `prob_contact`, `prob_fair` and `pred_hit`
#' 
#' @returns a table with columns `mean_prob_hbp`, `mean_prob_ball`, `mean_prob_strike`,
#'   `mean_prob_foul`, `mean_prob_fair`, and `mean_pred_hit`
#' 
#' @export
#' 
summarize_pitch_outcome <- function(pred_outcome_pitch) {

  pred_outcome_summarized <- pred_outcome_pitch |>
    dplyr::summarize(
      mean_prob_hbp = mean((1 - prob_swing) * prob_hbp),
      mean_prob_ball = mean((1 - prob_swing) * (1 - prob_hbp) * (1 - prob_strike)),
      mean_prob_strike = mean((1 - prob_swing) * (1 - prob_hbp) * prob_strike + prob_swing * (1 - prob_contact)),
      mean_prob_foul = mean(prob_swing * prob_contact * (1 - prob_fair)),
      mean_prob_fair = mean(prob_swing * prob_contact * prob_fair),
      mean_pred_hit = weighted.mean(pred_hit, w = prob_swing * prob_contact * prob_fair),
      .groups = "drop"
    )

  return(pred_outcome_summarized)
}
