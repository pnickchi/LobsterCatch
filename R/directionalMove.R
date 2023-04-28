#' This function controls the movements of lobsters toward the trap. There are both directional and random move components considered in
#' this function. The proximity of a lobster to a trap controls the magnitude of each components, i.e closer a lobster gets to the trap
#' the smaller the random component of movement. The directional move depends on radius of influence of a trap and current zone of
#' influence.
#' @param Lobster location of lobster in the grid in x and y coordinates.
#' @param dStep defines the distance that each lobster moves in each time step.
#' @param minDistoTrap defines the minimum distance to the trap.
#' @param Trap location of trap in the grid in x and y coordinates.
#' @param radiusOfInfluence trap's radius of influence.
#' @param currentZoI  initial zone of influence
#' @return Returns the new coordinates of each lobster in the grid after a directional move is applied.
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
