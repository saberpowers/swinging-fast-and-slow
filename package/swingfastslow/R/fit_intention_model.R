#' Fit skew-normal swing intention model
#' 
#' This function first a skew-normal swing intention model using BRMS.
#' 
#' @param swing a dataframe of swings
#' @param swing_metric the response variable: "bat_speed" or "swing_length"
#' 
#' @return a brmsfit object
#' 
#' @export
fit_intention_model <- function(swing, swing_metric = c("bat_speed", "swing_length")) {

  swing_metric <- match.arg(swing_metric)

  primary_fastballs <- get_primary_fastballs(swing)

  intent_swing_data <- swing |>
    dplyr::filter(pitch_type %in% c("FF", "SI", "FC")) |>
    dplyr::left_join(primary_fastballs, by = "pitcher_id") |>
    dplyr::mutate(is_primary = pitch_type == primary_fastball) |>
    # only use primary fastballs and squared up swings
    dplyr::filter(is_primary, squared_up)

  # Fit the intent model ----------------------------------------------------

  intention_model <- brms::brm(
    formula = brms::bf(
      bat_speed ~ balls + strikes + plate_x_ref + plate_z + (1 | pitcher_id) +
        (1 + strikes + plate_x_ref + plate_z | batter_side_id),
      sigma ~ 1,
      alpha ~ 1 + (1 | batter_side_id)
    ),
    family = brms::skew_normal(),
    data = intent_swing_data,
    chains = 4,
    cores = 4,
    iter = 4000
  )

  return(intention_model)
}
