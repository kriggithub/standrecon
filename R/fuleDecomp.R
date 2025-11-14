#' Calculates Fule's decomposition rates
#'
#' @param rate Previous decay rate.
#' @param percentiles Decay percentiles to be calculated.
#'
#' @returns
#' @export
#'
#' @examples
fuleDecomp <- function(rate, percentiles){
  (log(percentiles) - log(1))/(log(1 + rate))
}
