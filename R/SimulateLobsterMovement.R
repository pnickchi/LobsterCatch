#' A function to run the simulation based on defined parameters
#' @param p is a list which contains all input variables
#' @return Returns a list
#' @seealso to-do-list
#' @export
#' @examples
#' initlambda          <- 0.1
#' howClose            <- 0.5
#' shrinkage           <- 0.993
#' dStep               <- 5
#' nrowgrids           <- 10
#' ncolgrids           <- 10
#' unitarea            <- 10
#' initD               <- 1
#' currentZoI           <- 15
#' radiusOfInfluence   <- 15
#' saturationThreshold <- 5
#' Trap                <- data.frame(x = c(5), y = c(5))
#' ntraps              <- 1
#' lobLengthThreshold   <- 115
#' q0                   <- 0.5
#' realizations         <- 50
#' tSteps               <- 50
#' sexBased             <- TRUE
#' lengthBased          <- FALSE
#' trapSaturation       <- TRUE
#' qmin                 <- 0
#' lobsterSizeFile      <- ''
#' lobsterSexDist       <- list(labels = c('M','F','MM','BF'),
#' prob1 = c(0.55,0.35,0.05,0.05),
#' prob2 = c(0.5,0.50,0,0),
#' lobsterMatThreshold = 100)
#'
#' param <- list( nrowgrids=nrowgrids,
#'               ncolgrids=ncolgrids,
#'               unitarea=unitarea,
#'               initlambda=initlambda,
#'               initD=initD,
#'               shrinkage=shrinkage,
#'               currentZoI=currentZoI,
#'               radiusOfInfluence=radiusOfInfluence,
#'               Trap=Trap,
#'               ntraps=ntraps,
#'               saturationThreshold=saturationThreshold,
#'               howClose=howClose,
#'               dStep=dStep,
#'               lengthBased=lengthBased,
#'               lobsterSizeFile=lobsterSizeFile,
#'               lobLengthThreshold=lobLengthThreshold,
#'               trapSaturation=trapSaturation,
#'               q0=q0,
#'               qmin=qmin,
#'               realizations=realizations,
#'               tSteps=tSteps,
#'               sexBased=sexBased,
#'               lobsterSexDist=lobsterSexDist)
#' sim <- SimulateLobsterMovement(p = param)

SimulateLobsterMovement = function(p){

  nrowgrids           <- p$nrowgrids
  ncolgrids           <- p$ncolgrids
  unitarea            <- p$unitarea
  initlambda          <- p$initlambda
  initD               <- p$initD
  ntraps              <- nrow(p$Trap)
  lobLengthThreshold  <- p$lobLengthThreshold
  currentZoI          <- p$currentZoI
  shrinkage           <- p$shrinkage
  dStep               <- p$dStep
  tSteps              <- p$tSteps
  howClose            <- p$howClose
  q0                  <- p$q0
  qmin                <- p$qmin
  saturationThreshold <- p$saturationThreshold
  trapSaturation      <- p$trapSaturation
  lengthBased         <- p$lengthBased
  lobLengthThreshold  <- p$lobLengthThreshold
  Trap                <- p$Trap
  radiusOfInfluence   <- p$radiusOfInfluence
  lobsterSexDist      <- p$lobsterSexDist
  lobsterSizeFile     <- p$lobsterSizeFile
  sexBased            <- p$sexBased

  with(p, {

  if( (p$lengthBased == TRUE) & (p$lobsterSizeFile == '') ){
      print('Please provide a csv file for lobster size distribution.')
      lobsterSizeFile   <- file.choose()
      p$lobsterSizeFile <- lobsterSizeFile
  }

  CatchSimulationOutput = list()
  for(k in 1:p$realizations){

    start            <- Sys.time()
    outputs          <- list()
    outputs$traps    <- rep(0, times = ntraps)
    outputs$lobsters <- data.frame(EASTING = 0, NORTHING = 0, trapped=0, T = 0, I = 0, lobLength = 0)


    coordinatesOverTime      <- list()
    coordinatesOverTime[[1]] <- initialLobsterGrid(nrowgrids, ncolgrids, unitarea, initlambda, initD, lobsterSizeFile, lobsterSexDist)


    trapCatch           <- list()
    lobSize             <- list()
    lobSex              <- list()
    trapCatch[[1]]      <- rep(0, length=ntraps)
    lobSize[[1]]        <- rep('',length=ntraps)
    lobSex[[1]]         <- rep('',length=ntraps)


    for(t in 2:tSteps){

      if(t>2){currentZoI<- currentZoI * shrinkage}

      tempUpdateGrid = updateGrid(Lobster = coordinatesOverTime[[t-1]],
                                  Trap = Trap,
                                  trapCatch = trapCatch[[t-1]],
                                  lobSize = lobSize[[t-1]],
                                  lobSex  = lobSex[[t-1]],
                                  radiusOfInfluence = radiusOfInfluence,
                                  currentZoI = currentZoI,
                                  dStep = dStep,
                                  howClose = howClose,
                                  q0 = q0,
                                  qmin = qmin,
                                  saturationThreshold = saturationThreshold,
                                  trapSaturation = trapSaturation,
                                  lengthBased = lengthBased,
                                  lobLengthThreshold = lobLengthThreshold,
                                  sexBased = sexBased)
      # Adam: DO you think, this would help?
      #the following line can be moved out of
      #the loop so that we simply just Just keep the last iteration? To make things faster
      #and less memory demanding?
      coordinatesOverTime[[t]] <- tempUpdateGrid[[1]]
      trapCatch[[t]]           <- tempUpdateGrid[[2]]
      lobSize[[t]]             <- tempUpdateGrid[[3]]
      lobSex[[t]]              <- tempUpdateGrid[[4]]
    }


    outmove   = do.call(rbind, coordinatesOverTime)
    outmove$TimeStep = rep(0:(tSteps-1), each = nrow(coordinatesOverTime[[1]]) )
    outmove$LobIndx = rep(1:nrow(coordinatesOverTime[[1]]), times=tSteps)

    outtraps   = as.data.frame(do.call(rbind, trapCatch))
    outlobsize = as.data.frame(do.call(rbind, lobSize)  )
    outlobsex  = as.data.frame(do.call(rbind, lobSex)   )
    colnames(outtraps)   = paste0( 'Trap', 1:ncol(outtraps) )
    colnames(outlobsize) = paste0( 'Trap', 1:ncol(outtraps) )
    colnames(outlobsex)  = paste0( 'Trap', 1:ncol(outtraps) )

    outputs$traps    = outtraps
    outputs$lobsters = outmove
    outputs$lobSize  = outlobsize
    outputs$lobSex   = outlobsex

    CatchSimulationOutput[[k]] = outputs
    #print(paste('Timing', Sys.time()-start, 'for iteration #',k,sep=" "))
  }
  return(CatchSimulationOutput)
  })

}
