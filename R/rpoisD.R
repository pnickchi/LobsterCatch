#' This function generates either a Poisson or a negative binomial distribution of lobsters
#' @param n is the number of lobsters to be generated
#' @param lambda is the mean density of lobsters
#' @param D is the dispersion index to be used
#' @import
#' @return a vector of integers that is used as initial distribution of lobsters in the simulated arena
rpoisD<-function (n, lambda, D=1){
  if (D==1){
    rpois(n, lambda)
  }  else {
    sz = lambda^2/(D*lambda-lambda)
    rnbinom(n, size=sz, mu=lambda)
  }
}
