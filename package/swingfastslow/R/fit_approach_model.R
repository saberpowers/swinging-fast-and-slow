#' Fit approach model
#' 
#' This model estimates the effect of approach (modulating bat speed and swing length according to
#' count) on contact probability, fair probability and expected hit outcome.
#' 
#' @param data_approach a dataframe with at least the following columns: `strikes`,
#'   `strikes_bat_speed`, `strikes_swing_length`, `prob_contact`, `is_contact`, `prob_fair`,
#'   `is_fair`, `pred_hit` and `hit_pred`.
#' 
#' @returns a list with the following elements:
#' \itemize{
#'   \item{"fit_contact"}{a fitted glm model for contact probability}
#'   \item{"fit_fair"}{a fitted glm model for fair probability}
#'   \item{"fit_hit"}{a fitted lm model for expected hit outcome}
#' }
#' 
#' @export
#' 
fit_approach_model <- function(data_approach) {

  data_approach <- data_approach |>
    dplyr::mutate(
      approach_bat_speed = strikes * strikes_bat_speed,
      approach_swing_length = strikes * strikes_swing_length
    )

  fit_contact <- data_approach |>
    with(
      glm(
        formula = is_contact ~ approach_bat_speed + approach_swing_length,
        family = binomial(),
        offset = log_odds(prob_contact)
      )
    )

  fit_fair <- data_approach |>
    dplyr::filter(is_contact) |>
    with(
      glm(
        formula = is_fair ~ approach_bat_speed + approach_swing_length,
        family = binomial(),
        offset = log_odds(prob_fair)
      )
    )

  fit_hit <- data_approach |>
    dplyr::filter(is_fair) |>
    with(
      lm(
        formula = hit_pred ~ approach_bat_speed + approach_swing_length,
        offset = pred_hit
      )
    )

  return(list(fit_contact = fit_contact, fit_fair = fit_fair, fit_hit = fit_hit))
}
