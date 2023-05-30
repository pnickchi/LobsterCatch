#' The function randomly selects an angle (0:360) and moves the lobster. This function is called when a lobster
#' is outside the area of influence.
#' @param Lobster location of lobster in x and y coordinates
#' @param  dStep  is how much a lobster moves in each time step
#' @return Returns the new coordinates of each lobster
randomMove<- function(Lobster, dStep){

  xLobster = Lobster[1]
  yLobster = Lobster[2]

  randomAngle<- runif(n=1, min = 0, max=360)
  xNew<- dStep * cos(randomAngle * pi / 180) + xLobster
  yNew<- dStep * sin(randomAngle * pi / 180) + yLobster

  return( list(EASTING = xNew, NORTHING = yNew) )
}
