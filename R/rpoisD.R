#' This function generates  a Poisson or a negative binomial distribution for lobsters in the arena
#' @param n is the number of lobsters to be generated
#' @param lambda is the mean density of lobsters
#' @param D is the dispersion index to be used. Default value is 1
#' @import stats
#' @return A vector of integers that is used as initial distribution of lobsters
rpoisD<-function (n, lambda, D=1){
  if (D==1){
    rpois(n, lambda)
  }  else {
    sz = lambda^2/(D*lambda-lambda)
    rnbinom(n, size=sz, mu=lambda)
  }
}
