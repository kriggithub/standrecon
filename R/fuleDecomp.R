fuleDecomp <- function(rate, percentiles){
  (log(percentiles) - log(1))/(log(1 + rate))
}
