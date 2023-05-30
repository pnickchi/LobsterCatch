#' This function models movement of lobsters toward the trap.The distance of lobsters to trap determines the magnitude of those moves.
#' As lobster gets closer to the trap, the magnitude of its directional move becomes larger and the random move becomes smaller.
#' @param Lobster location of lobster in the grid in x and y coordinates.
#' @param dStep Distance that each lobster moves during one time step.
#' @param minDistoTrap Distance from the trap.
#' @param Trap location of trap in the arena.
#' @param radiusOfInfluence  Radius of influence for the baited trap.
#' @param currentZoI   Radius of influence in each time step given the bait shrinkage.
#' @return Returns the new coordinates of each lobster in the arena after each directional move.
#'
directionalMove <- function(Lobster, dStep, minDistoTrap, Trap, radiusOfInfluence, currentZoI){

  # Get x and y coordinate of lobster
  xLobster = Lobster[1]
  yLobster = Lobster[2]

  # Get x and y coordinate of trap
  xtrap = Trap[1]
  ytrap = Trap[2]

  thetaT =  atan2(ytrap-yLobster,xtrap-xLobster)*180/pi
  b = 1 + 0.9 * (minDistoTrap - currentZoI) / radiusOfInfluence
  thetaR = -180:180
  P = 1/(180^b) * abs(thetaR) ^ b
  Prtheta_R = (1-P) / sum(1-P)
  theta_r = sample(thetaR,size = 1, prob = Prtheta_R)
  theta   <- thetaT + theta_r
  xNew   <- dStep * cos(theta * pi / 180) + xLobster
  yNew   <- dStep * sin(theta * pi / 180) + yLobster

  return( list(EASTING = xNew, NORTHING = yNew) )

}
