#' This function determines if lobster gets into a trap and is caught.
#' @param loc1 is the location of lobster at the start of each time step
#' @param loc2 is the location of lobster at the end of each time step
#' @param Trap is the location of trap
#' @param howClose sets an area around the location of trap within which catch occurs
#' @return Returns a vector that contain lobster path and whether its trapped
trapInPath = function(loc1, loc2, Trap, howClose){
  x = seq(loc1[1],loc2[1],length.out = 10)
  y = seq(loc1[2],loc2[2],length.out = 10)
  path = data.frame(EASTING=x, NORTHING=y)
  ds = unlist(apply(path,1,distanceToTrapCalculator,Trap = Trap))
  if(any(ds<howClose)) {
    i= min(which(ds<howClose))
    path = c(path[i,1],path[i,2])
    trapped = 1
  } else {
    path = loc2
    trapped = 0
  }
  return(c(path, trapped))
}
