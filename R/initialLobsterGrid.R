#' This function simulates an arena, with lobsters in it based on the provided density, size and sex ratio
#' @param nrowgrids is a numeric value which defines number of rows of the arena
#' @param ncolgrids is a numeric value which defines number of columns of the arena
#' @param unitarea is the unit area used for estimating density of lobsters.
#' @param initlambda is the density of lobsters at the beginning of simulation
#' @param initD is the dispersion index of lobsters on seabed at the beginning of the simulation
#' @param losbterSizeFile is a csv file that contains the frequency of lobsters class size
#' @param losbterSexDist is a list that contains the sex ratio of lobsters
#' @return Returns  x and y coordinates of simulated lobsters at the beginning

initialLobsterGrid = function(nrowgrids, ncolgrids, unitarea, initlambda, initD, lobsterSizeFile, lobsterSexDist){

  ngrids <- nrowgrids * ncolgrids

  stop.condition = FALSE # this is to take care of instances when no lobster is simulated(due to low density)

  while(stop.condition == FALSE){
    #temp <- rpoisD(n = unitarea,lambda = initlambda, D = initD)
    #temp <- rep(temp,20)
    #initialLobster  <- c(temp, rep(0, ngrids - unitarea) )
    #initialLobster  <- sample(x = initialLobster)
    #rate <- (initlambda * unitarea) / ngrids
    #initialLobster  <- rpoisD(n = unitarea,lambda = rate, D = initD)

    #initialLobster <- as.vector( replicate(n = ngrids/unitarea, rpoisD(n = unitarea,lambda = initlambda, D = initD)) )
    initialLobster <- rpoisD(n = ngrids/unitarea,lambda = initlambda, D = initD)
    initialLobster <- c(initialLobster, rep(0,ngrids - length(initialLobster)))
    initialLobster <- sample(x = initialLobster)
    LobsterStart   <- data.frame(EASTING = rep(1:ncolgrids,times=nrowgrids),
                                  NORTHING = rep(1:nrowgrids,each=ncolgrids),
                                  Lobs = initialLobster)
    LobsterStart <- subset(LobsterStart,Lobs>0)
    tt <- unlist( apply(X = LobsterStart, MARGIN = 1, FUN = replicateCoordinates) )
    tt <- matrix(tt, ncol = 2, byrow = TRUE)
    colnames(tt)<- c("EASTING","NORTHING")
    initialxyCoordinate  = as.data.frame(tt)

    if( nrow(initialxyCoordinate) > 0 ){
      stop.condition = TRUE
    }

  }

  initialxyCoordinate$trapped <- 0
  initialxyCoordinate$lobLength <- NA
  initialxyCoordinate$lobSex    <- NA

  if( lobsterSizeFile != '' ){
    lobsterSizeFreq <- read.csv(file = lobsterSizeFile, header = TRUE, stringsAsFactors = FALSE)
    lobsterSizeFreq$prob <- lobsterSizeFreq$freq / sum(lobsterSizeFreq$freq )
    labels   <- lobsterSizeFreq$bins
    lobProb <- lobsterSizeFreq$prob
    initialxyCoordinate$lobLength <- rep(NA, nrow(initialxyCoordinate) )
    initialxyCoordinate$lobLength<-sample(x = labels, size = sum(initialLobster), replace = TRUE, prob = lobProb)
  }

  if( length(lobsterSexDist) > 0 ){

    u  <- lobsterSexDist$lobsterMatThreshold
    x  <- lobsterSexDist$labels
    p1 <- lobsterSexDist$prob1

    indx1 <- which( initialxyCoordinate$lobLength >= u)
    initialxyCoordinate[indx1, 'lobSex'] <- sample(x = x, size = length(indx1), replace = TRUE, prob = p1)

    p2 <- lobsterSexDist$prob2
    initialxyCoordinate[-indx1, 'lobSex'] <- sample(x = x, size = nrow(initialxyCoordinate) - length(indx1), replace = TRUE, prob = p2)
  }


  return(initialxyCoordinate)

}


