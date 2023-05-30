#' The function finds the closest trap to a lobster and calculates the distance.
#' @param Lobster location of lobster in the arena
#' @param Trap location of trap in the arena
#' @return Returns distance to closest trap and saves the trap number in case of multiple traps.
distanceToClosestTrap <- function(Lobster, Trap){
  ds = unlist(apply(Trap,1,distanceToTrapCalculator,Lobster))
  dmin = which.min(ds)
  return( c(distance = ds[dmin], trapId = dmin) )
}
