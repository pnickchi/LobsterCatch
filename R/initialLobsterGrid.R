#' This function simulates an arena (or grid) with lobsters in it based on the provided density, size and sex distribution.
#' @param nrowgrids is a numeric value which defines the number of rows of the arena.
#' @param ncolgrids is a numeric value which defines the number of columns of the arena.
#' @param unitarea is the unit area used for estimating density of lobsters.
#' @param initlambda is the density of lobsters at the beginning of simulation.
#' @param initD is the dispersion index of lobsters on seabed at the beginning of the simulation.
#' @param lobsterSizeFile is a csv file that contains the frequency of lobsters size clases.
#' @param lobsterSexDist is a list that contains the sex ratio of lobsters. Possible values are M=male, F=female, MM=mature male, BF=berried female)
#' @return Returns x and y coordinates of simulated lobsters at the beginning.
#' @import utils
initialLobsterGrid = function(nrowgrids, ncolgrids, unitarea, initlambda, initD, lobsterSizeFile, lobsterSexDist){

  ngrids <- nrowgrids * ncolgrids

  stop.condition = FALSE

  while(stop.condition == FALSE){

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
