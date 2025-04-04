#' Get intention model summary
#' 
#' This function extracts only approach random effects from the BRMS intension model because the
#' full model structure is massive.
#' 
#' @param intention_model_bat_speed a fitted BRMS intention model for bat speed
#' @param intention_model_swing_length a fitted BRMS intention model for swing length
#' 
#' @return a table of approach random slopes for bat speed and for swing length
#' 
#' @export
get_intention_model_summary <- function(intention_model_bat_speed, intention_model_swing_length) {

  # Create the approach table -----------------------------------------------

  # Get the posterior means for the random effects in each model:
  bat_speed_re <- intention_model_bat_speed |>
    tidybayes::spread_draws(r_batter_side_id[batter_side_id, term]) |>
    # already grouped by batter_side_id and term:
    dplyr::summarize(value = mean(r_batter_side_id), .groups = "drop") |>
    # Make a wide dataset with a column for each:
    tidyr::pivot_wider(names_from = term, names_glue = "{term}_bat_speed", values_from = value)

  # Repeat for swing length
  swing_length_re <- intention_model_swing_length |>
    tidybayes::spread_draws(r_batter_side_id[batter_side_id, term]) |>
    # already grouped by batter_side_id and term:
    dplyr::summarize(value = mean(r_batter_side_id), .groups = "drop") |>
    # Make a wide dataset with a column for each:
    tidyr::pivot_wider(names_from = term, names_glue = "{term}_swing_length", values_from = value)

  # Create and save the approach table:
  approach <- bat_speed_re |>
    dplyr::select(batter_side_id, strikes_bat_speed) |>
    dplyr::inner_join(
      y = dplyr::select(swing_length_re, batter_side_id, strikes_swing_length),
      by = "batter_side_id"
    )
  
  return(approach)
}
