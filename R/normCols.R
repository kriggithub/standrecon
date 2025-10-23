normCols <- function(x) {
  if (is.character(x)) {
    x
  } else {
    deparse(substitute(x))
  }
}
