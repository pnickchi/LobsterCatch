#' This function generates either a Poisson or a negative binomial distribution of lobsters on the sea bed
#' @param n is the number of observations (lobsters) to be generated
#' @param lambda is the mean density to be used
#' @param D is the dispersion index to be used
#' @import stats
#' @return a vector of integers that is used as initial distribution of lobsters on the seabed.
rpoisD<-function (n, lambda, D=1){
  if (D==1){
    rpois(n, lambda)
  }  else {
    sz = lambda^2/(D*lambda-lambda)
    rnbinom(n, size=sz, mu=lambda)
  }
}
