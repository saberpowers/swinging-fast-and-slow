#' Fit swing intention model
#' 
#' Fit a model to predict bat speed and swing length conditional on "squared-up" contact (a
#' surrogated for well-timed swings). Report estimates of batter approach, adaptation and timing.
#'
#' @param data_intention a dataframe with columns `balls`, `strikes`, `plate_x_ref`, `plate_z`,
#'   `batter_side_id`, `bat_speed` and `swing_length`
#' @param adaptation_grid_size the number of pitch locations to sample for estimating batter swing
#'   adaptation. Larger values require more computation but yield more precise estimates.
#' 
#' @returns a list with the following elements:
#' \itemize{
#'   \item{"fit_bat_speed"}{a fitted lme4 model for intended bat speed}
#'   \item{"fit_swing_length"}{a fitted lme4 model for intended swing length}
#'   \item{"approach"}{a dataframe summarizing estimated batter approach}
#'   \item{"adaptation"}{a dataframe summarizing estimated batter adaptation}
#'   \item{"timing"}{a dataframe summarizing estimated batter timing}
#' }
#' 
#' @export
#' 
fit_intention_model <- function(data_intention, adaptation_grid_size = 1000) {

  # Fit mixed-effects models ----

  data_intention_filtered <- data_intention |>
    dplyr::filter(squared_up)

  fit_bat_speed <- lme4::lmer(
    bat_speed ~ balls + strikes + plate_x_ref + plate_z +
      (1 + strikes + plate_x_ref + plate_z | batter_side_id),
    control = lme4::lmerControl(optimizer = "bobyqa"),
    data = data_intention_filtered
  )

  fit_swing_length <- lme4::lmer(
    swing_length ~ balls + strikes + plate_x_ref + plate_z +
      (1 + strikes + plate_x_ref + plate_z | batter_side_id),
    control = lme4::lmerControl(optimizer = "bobyqa"),
    data = data_intention_filtered
  )

  # Caution: Fitted values and residuals include non-squared-up swings (from outside training set)
  # But we can only make a prediction for batters observed in the training set
  data_intention_pred <- data_intention |>
    dplyr::filter(batter_side_id %in% data_intention_filtered$batter_side_id)

  fitted_values <- tibble::tibble(
    swing_length = predict(fit_swing_length, newdata = data_intention_pred),
    bat_speed = predict(fit_bat_speed, newdata = data_intention_pred)
  )

  residuals <- tibble::tibble(
    swing_length = data_intention_pred$swing_length - fitted_values$swing_length,
    bat_speed = data_intention_pred$bat_speed - fitted_values$bat_speed
  )


  # Summarize approach, adaptation and timing ----

  # Approach is variation that co-varies with count
  # We express approach as the batter random slope for strikes
  approach <- tibble::tibble(
    batter_side_id = rownames(lme4::ranef(fit_bat_speed)$batter_side_id),
    strikes_bat_speed = lme4::ranef(fit_bat_speed)$batter_side_id[, "strikes"],
    strikes_swing_length = lme4::ranef(fit_swing_length)$batter_side_id[, "strikes"]
  )

  # Swing adaptation is variation that co-varies with pitch location
  # We express adaptation as the covariance between pitch location and each swing metric
  # To estimate this, we calculate all players' intentions for the same random sample of locations
  plate_grid <- data_intention |>
    dplyr::slice(
      rep(sample(1:dplyr::n(), size = adaptation_grid_size), times = nrow(approach))
    ) |>
    dplyr::select(plate_x_ref, plate_z) |>
    dplyr::mutate(
      balls = 0,
      strikes = 0,
      batter_side_id = rep(approach$batter_side_id, each = adaptation_grid_size)
    )
  adaptation <- plate_grid |>
    dplyr::mutate(
      intended_bat_speed = predict(fit_bat_speed, newdata = plate_grid),
      intended_swing_length = predict(fit_swing_length, newdata = plate_grid)
    ) |>
    dplyr::group_by(batter_side_id) |>
    dplyr::summarize(
      var_bat_speed = var(intended_bat_speed),
      var_swing_length = var(intended_swing_length),
      .groups = "drop"
    )

  # Timing is unexplained variation (residuals from the intention model)
  # We express timing as the residual variance in each swing metric from the intention model
  data_subset <- data_intention |>
    dplyr::filter(batter_side_id %in% rownames(lme4::ranef(fit_bat_speed)$batter_side_id))
  timing <- data_subset |>
    dplyr::mutate(
      intended_bat_speed = predict(fit_bat_speed, newdata = data_subset),
      intended_swing_length = predict(fit_swing_length, newdata = data_subset),
      residual_bat_speed = bat_speed - intended_bat_speed,
      residual_swing_length = swing_length - intended_swing_length
    ) |>
    dplyr::group_by(batter_side_id) |>
    dplyr::summarize(
      var_bat_speed = var(residual_bat_speed),
      var_swing_length = var(residual_swing_length),
      .groups = "drop"
    )

  return(
    list(
      fit_bat_speed = fit_bat_speed,
      fit_swing_length = fit_swing_length,
      fitted_values = fitted_values,
      residuals = residuals,
      approach = approach,
      adaptation = adaptation,
      timing = timing
    )
  )
}
