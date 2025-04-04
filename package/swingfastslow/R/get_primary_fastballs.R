#' Get primary fastball type
#' 
#' This function finds the primary type of fastball for every pitcher in the dataset.
#' 
#' @param data a dataset of pitches downloaded by \code{sabRmetrics::download_baseballsavant}
#' 
#' @return a table of pitch types, indexed by pitcher_id
#' 
#' @export
get_primary_fastballs <- function(data) {
  
  # Filter to fastballs -----------------------------------------------------
  
  fastball_data <- data |>
    dplyr::filter(pitch_type %in% c("FF", "SI", "FC"))
  
  # Find primary fastball for each pitcher ----------------------------------
  
  pitcher_fastball_type <- fastball_data |>
    dplyr::group_by(pitcher_id, pitch_type) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::group_by(pitcher_id) |>
    dplyr::slice_max(order_by = `n`, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-n) |>
    dplyr::rename(primary_fastball = pitch_type)

  return(pitcher_fastball_type)
}
