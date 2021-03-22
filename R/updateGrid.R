#' This function updates the coordinate of each lobster at each timestep,
#' @param Lobster location of lobster in x and y coordinates
#' @param Trap location of trap in x and y coordinates
#' @param trapCatch number of lobsters caught
#' @param lobSize size of caught lobster
#' @param radiusOfInfluence is the initial radius of influence (Referred as I0 in the paper)
#' @param dStep is how much a lobster moves in each time step
#' @param currentZoI is the bait's area of influence at each timestep
#' @param howClose sets the distance from the trap within which catch occurs
#' @param q0 is the initial probability of entry into an empty trap(i.e. 0.5)
#' @param qmin is the asymptotic minimum probability of entry (i.e. 0)
#' @param  saturationThreshold is the number of lobsters in a trap at which the probability of
#' another lobster entering the trap drops to qmin
#' @param trapSaturation is a logical parameter
#' @param lengthBased is a logical parameter
#' @param lobLengthThreshold is a length threshold (i.e. CL in centimeters) beyond which there is no chance of catching another lobster
#' @return a list of new coordinates, number of catch and their sizes
#' @export
updateGrid    = function(Lobster, Trap, trapCatch, lobSize, radiusOfInfluence, currentZoI, dStep, howClose, q0, qmin, saturationThreshold, trapSaturation, lengthBased, lobLengthThreshold){

  numberOfLobsters <- nrow(Lobster)
  if(numberOfLobsters>0) {

    xNew            <- vector(mode = 'numeric', length = numberOfLobsters)
    yNew            <- vector(mode = 'numeric', length = numberOfLobsters)
    trappedLobster  <- vector(mode = 'numeric', length = numberOfLobsters)

    for( lobsterIndex in 1:numberOfLobsters ){
      xOld    <- Lobster[lobsterIndex,1]
      yOld    <- Lobster[lobsterIndex,2]
      trapped <- Lobster[lobsterIndex,3]

      if(trapped == 1){
        xNew[lobsterIndex] = xOld
        yNew[lobsterIndex] = yOld
        trappedLobster[lobsterIndex] = trapped
        next()
      }

      # Check with Adam about trap interaction, not always the closest trap catches the lobster, right?
      # Why Trap[,c(1,2)] and not just Trap
      minTrap = distanceToClosestTrap(Lobster = c(xOld,yOld), Trap = Trap[,c(1,2)] )

      if( minTrap[1] > radiusOfInfluence){

        temp                         <- randomMove(Lobster = c(xOld, yOld) , dStep)
        xNew[lobsterIndex]           <- temp$EASTING
        yNew[lobsterIndex]           <- temp$NORTHING
        trappedLobster[lobsterIndex] <- 0
      }else{
        temp               <- directionalMove(Lobster = c(xOld,yOld), dStep = dStep, minDistoTrap = minTrap[1], Trap = c(Trap[minTrap[2],1],Trap[minTrap[2],2]), radiusOfInfluence = radiusOfInfluence, currentZoI = currentZoI)
        xNew[lobsterIndex] <- temp$EASTING
        yNew[lobsterIndex] <- temp$NORTHING
        trapPathCheck      <- trapInPath(loc1 = c(xOld,yOld), loc2 = c(xNew[lobsterIndex],yNew[lobsterIndex]), Trap = Trap[minTrap[2],], howClose = howClose)

        if( trapPathCheck[3] == 1){

          if(trapSaturation == TRUE){
            pC = catchability(q0 = q0, qmin = qmin, saturationThreshold = saturationThreshold, Ct = trapCatch[minTrap[2]], lengthBased = lengthBased, lobLengthThreshold = lobLengthThreshold , lobSize = lobSize[minTrap[2]])
            caughtStatus = rbinom(n = 1, size = 1, prob = pC)
          }else{
            pC = q0
            caughtStatus = rbinom(n = 1, size = 1, prob = pC)
          }

          if( caughtStatus == 1 ){
            trapCatch[minTrap[2]]        <- trapCatch[minTrap[2]] + 1
            xNew[lobsterIndex]           <- Trap[minTrap[2],1]
            yNew[lobsterIndex]           <- Trap[minTrap[2],2]
            trappedLobster[lobsterIndex] <- 1
            lobSize[minTrap[2]]          <- paste0(lobSize[minTrap[2]], '-', Lobster[lobsterIndex,4])
          }

        }



      }
    }

    updatedGrid <- data.frame(EASTING = xNew, NORTHING = yNew, trapped = trappedLobster, lobLength = Lobster$lobLength)
    return(list(updatedGrid, trapCatch, lobSize))

  }
}