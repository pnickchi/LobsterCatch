#' This function finds the closest trap to a lobster and calculates its distance and returns the distance and trap number.
#' @param Lobster location of lobster in the grid in x and y coordinates.
#' @param Trap location of trap in x and y coordinates.
#' @return Returns distance to closest trap and the trap number.
#' @export
distanceToClosestTrap <- function(Lobster, Trap){
  ds = unlist(apply(Trap,1,distanceToTrapCalculator,Lobster))
  dmin = which.min(ds)
  return( c(distance = ds[dmin], trapId = dmin) )
}
