#' Recreate MLB's "squared-up" metric
#'
#' MLB defines a batted ball as squared up if it is a fair ball and the exit velocity is at least
#' 80% of (a rough approximation of) its theoretical maximum, given the bat speed and the speed of
#' the pitch as it crosses the front of home plate. This function calculates the speed of the pitch
#' at the front of home plate and then tests whether the exit velocity is at least 80% of its
#' theoretical max.
#'  
#' @param data a dataframe with at least the following columns: `ay`, `by`, `cy`, 'bx', 'bz'
#'   (quadractic coefficients used for calculating plate speed), `bat_speed` and `launch_speed`.
#' 
#' @returns a dataframe with the same rows and columns as `data`, with the logical column
#'   `squared_up` appended.
#' 
#' @export
#' 
#' @references
#'   http://tangotiger.com/index.php/site/article/statcast-lab-collisions-and-the-perfect-swing
#'   https://x.com/tangotiger/status/1790474439785648493
#' 
recreate_squared_up <- function(data) {

  data_enhanced <- data |>
    dplyr::mutate(
      plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
      plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
      plate_speed = 0.6818182 * sqrt(
        (ax * plate_time + bx)^2 + (ay * plate_time + by)^2 + (az * plate_time + bz)^2
      ),
      squared_up = ifelse(
        test = description == "hit_into_play" & !is.na(launch_speed),
        yes = (launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) > 0.8,
        no = FALSE
      )
    ) |>
    dplyr::select(dplyr::all_of(colnames(data)), squared_up) # drop intermediate columns

  return(data_enhanced)
}
