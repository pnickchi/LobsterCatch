#' This function calculates the variance to mean ratio (also known as dispersion index).
#' @param x is a numeric vector.
#' @return Returns the dispersion index.

dispersion <- function(x) {
  var(x) / mean(x)
}
