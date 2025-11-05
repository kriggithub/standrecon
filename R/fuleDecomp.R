#' Calculates Fule's decomposition rates
#'
#' @param rate
#' @param percentiles
#'
#' @returns
#' @export
#'
#' @examples
fuleDecomp <- function(rate, percentiles){
  (log(percentiles) - log(1))/(log(1 + rate))
}
