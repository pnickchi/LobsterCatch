#' Function to run the simulation based on defined parameters
#' @param p is a list of all input variables
#' @return Returns a list
#' @seealso Examples of the input parameters and more details can be found here: https://github.com/pnickchi/lobstercatch/blob/main/Examplecode.R
#' @examples
#' p = list()
#' p$nrowgrids = 100
#' p$ncolgrids = 100
#' p$ngrids = p$nrowgrids * p$ncolgrids
#' p$unitarea = 5
#' p$initlambda = 0.5
#' p$dStep = 1
#' p$howClose = 1
#' p$initD = 2
#' p$shrinkage = 0.993
#' p$currentZoI = 15
#' p$radiusOfInfluence = 15
#' p$q0 = 0.5
#' p$qmin = 0
#' p$Trap = data.frame( x = c(50), y = c(50) )
#' p$ntraps = nrow(p$Trap)
#' p$saturationThreshold = 5
#' p$lengthBased = FALSE
#' p$lobsterSizeFile =
#' 'https://raw.githubusercontent.com/vpourfaraj/lobsterCatch/main/inst/extdata/LobsterSizeFreqs.csv'
#' p$lobLengthThreshold = 115
#' p$trapSaturation = FALSE
#' p$sexBased = FALSE
#' p$lobsterSexDist = list(labels = c('M','F','MM','BF'),
#'                         prob1 = c(0.55,0.35,0.05,0.05),
#'                         prob2 = c(0.5,0.50,0,0),
#'                        lobsterMatThreshold = 100)
#' p$realizations = 2
#' p$tSteps = 5
#' Simrun = SimulateLobsterMovement(p)
#' @export
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
      print('Upload a csv file for lobster size distribution.')
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

  }
  return(CatchSimulationOutput)
  })

}
