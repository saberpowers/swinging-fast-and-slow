#' Log-odds
#'  
#' Calculate the log-odds of a probability
#' 
#' @param prob a scalar or numeric vector between 0 and 1
#' 
#' @returns a scalar or numeric vector
#' 
log_odds <- function(prob) {
  return(log(prob / (1 - prob)))
}
