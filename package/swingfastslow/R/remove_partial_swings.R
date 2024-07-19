#' Remove partial swings
#' 
#' Filter out bunt attempts and check swings based on bat speed.
#' 
#' @param swing a dataframe with the following coulmns: `description`, `des`, `bat_speed`
#' 
#' @return a filtered dataframe after removing rows identified as bunts and checked swings
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#'    data <- sabRmetrics::download_baseballsavant(
#'      start_date = "2024-07-01",
#'      end_date = "2024-07-01"
#'    )
#'    data_filtered <- data |>
#'      remove_partial_swings()
#' }
remove_partial_swings <- function(swing) {

  swing_filtered <- swing |>
    dplyr::filter(
      # remove bunt attempts
      !(
        stringr::str_detect(description, "bunt") |  # only detects missed and foul bunt attempts
        (description == "hit_into_play" & stringr::str_detect(des, " bunt"))  # covers fair bunts
      ),
      # remove checked swings (which only count as swings if they accidentally result in contact)
      bat_speed > 50  # this seemingly arbitrary cutoff is the result of extensive EDA
    )

  return(swing_filtered)
}
