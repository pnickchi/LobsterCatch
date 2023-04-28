#' This function calculates the Euclidean distance between Trap(s) and each individual lobster. The function is internally called
#' in `distanceToClosestTrap` function.
#' @param Lobster location of lobster in the grid in x and y coordinates.
#' @param Trap location of trap in the grid in x and y coordinates.
#' @return Returns the distance to trap.
#' @export
distanceToTrapCalculator<- function(Lobster,Trap){
  xLobster = Lobster[1]
  yLobster = Lobster[2]
  xTrap = Trap[1]
  yTrap = Trap[2]
  distanceToTrap<- sqrt((xLobster - xTrap)^2 + (yLobster -yTrap)^2)
  return(distanceToTrap)
}
