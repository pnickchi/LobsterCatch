#This file was written by Payman to atomize the simulation process for combination of factors. Make sure to change parameters
# on line 42-73 accordingly

# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(usethis)
library(devtools)

load_all()
document()
#check()
#build()

# Define function
GetdfComplete = function(x, extp){

  temp           <- bind_cols(x)

  density             <- rep.int(p$initlambda,          p$realizations)
  dstepmov            <- rep.int(p$dStep,               p$realizations)
  saturationThreshold <- rep.int(p$saturationThreshold, p$realizations)
  baitShrinkage       <- rep.int(p$shrinkage,           p$realizations)
  howClose            <- rep.int(p$howClose,            p$realizations)
  trapSaturation      <- rep.int(p$trapSaturation,      p$realizations)

  res <- bind_cols(temp, density, dstepmov, saturationThreshold, baitShrinkage, howClose, trapSaturation)

  clNames <- c(
    paste0('TimeToMax_Trap',    1:ncol(x$TimeToMax)),
    paste0('MaxCatch_Trap',     1:ncol(x$MaxCatch)),
    paste0('LegalCatchWt_Trap', 1:ncol(x$LegalCatchWt)),
    paste0('TotalCatchWt_Trap', 1:ncol(x$TotalCatchWt))
  )
  colnames(res) <- c(clNames,'densitylambda','dstepmov','saturationThreshold','baitShrinkage', 'howClose', 'trapSaturation')

  return(res)

}

initlambda          <- c(0.1, 0.5, 1, 1.6)
dStep               <- 1:10

nrowgrids           <- rep(200, length(initlambda) * length(dStep)) #dimesntions of arena
ncolgrids           <- rep(200, length(initlambda) * length(dStep))
unitarea            <- rep(100, length(initlambda) * length(dStep))
initD               <- rep(3, length(initlambda) * length(dStep))  #inial dispersion index of lobsters in the arena
shrinkage           <- rep(0.993, length(initlambda) * length(dStep))
currentZoI          <- rep(15, length(initlambda) * length(dStep)) # The Zone of influence that gets updated in each timestep
radiusOfInfluence   <- rep(15, length(initlambda) * length(dStep)) # the radious of bait influence
saturationThreshold <- rep(5, length(initlambda) * length(dStep))
howClose            <- rep(0.5, length(initlambda) * length(dStep)) #determines at what distance a lobster considered trapped
Trap                <- rep(list(data.frame( x = c(100), y = c(100))),  length(initlambda) * length(dStep))
ntraps              <- unlist( lapply(X = Trap, nrow) )
lobLengthThreshold  <- rep(115, length(initlambda) * length(dStep))
q0                  <- rep(0.5, length(initlambda) * length(dStep))
realizations        <- rep(50, length(initlambda) * length(dStep)) # Number of simulation
tSteps              <- rep(50, length(initlambda) * length(dStep)) #Soak time
sexBased            <- rep(TRUE, length(initlambda) * length(dStep))
lengthBased         <- rep(TRUE, length(initlambda) * length(dStep))
trapSaturation      <- rep(FALSE, length(initlambda) * length(dStep))
qmin                <- rep(0.5, length(initlambda) * length(dStep)) #this hase to be set to 0 when trap saturation is TRUE


lobsterSizeFile     <- 'https://raw.githubusercontent.com/vpourfaraj/lobsterCatch/main/inst/extdata/LobsterSizeFreqs.csv'
lobsterSexDist      <- list(labels = c('M','F','MM','BF'),
                            prob1 = c(0.55,0.35,0.05,0.05),
                            prob2 = c(0.5,0.50,0,0),
                            lobsterMatThreshold = 100)

initlambda          <- rep(c(0.1, 0.5, 1, 1.6), 4)
dStep               <- C(1,1,1,1,2,2,2,2,5,5,5,5,10,10,10,10)

param <- list( nrowgrids=nrowgrids,
               ncolgrids=ncolgrids,
               unitarea=unitarea,
               initlambda=initlambda,
               initD=initD,
               shrinkage=shrinkage,
               currentZoI=currentZoI,
               radiusOfInfluence=radiusOfInfluence,
               Trap=Trap,
               ntraps=ntraps,
               saturationThreshold=saturationThreshold,
               howClose=howClose,
               dStep=dStep,
               lengthBased=lengthBased,
               lobsterSizeFile=lobsterSizeFile,
               lobLengthThreshold=lobLengthThreshold,
               trapSaturation=trapSaturation,
               q0=q0,
               qmin=qmin,
               realizations=realizations,
               tSteps=tSteps,
               sexBased=sexBased,
               lobsterSexDist=lobsterSexDist)

nsettings <- length(param$nrowgrids)



# Loop over list and initialize a parameter list and execute the simulations

Simrun  <- list()
Results <- list()
resultdfcomplete <- list()
for(i in 1:nsettings){

  p <- list()
  p$nrowgrids            <- param$nrowgrids[i]
  p$ncolgrids            <- param$ncolgrids[i]
  p$ngrids               <- p$nrowgrids[i] * p$ncolgrids[i]
  p$unitarea             <- param$unitarea[i]
  p$initlambda           <- param$initlambda[i]
  p$initD                <- param$initD[i]
  p$shrinkage            <- param$shrinkage[i]
  p$currentZoI           <- param$currentZoI[i]
  p$radiusOfInfluence    <- param$radiusOfInfluence[i]
  p$Trap                 <- as.data.frame(param$Trap[i])
  p$ntraps               <- param$ntraps[i]
  p$saturationThreshold  <- param$saturationThreshold[i]
  p$howClose             <- param$howClose[i]
  p$dStep                <- param$dStep[i]
  p$lengthBased          <- param$lengthBased[i]
  p$lobsterSizeFile      <- param$lobsterSizeFile
  p$lobLengthThreshold   <- param$lobLengthThreshold[i]
  p$trapSaturation       <- param$trapSaturation[i]
  p$q0                   <- param$q0[i]
  p$qmin                 <- param$qmin[i]
  p$realizations         <- param$realizations[i]
  p$tSteps               <- param$tSteps[i]
  p$sexBased             <- param$sexBased[i]
  p$lobsterSexDist       <- param$lobsterSexDist

  print( paste0('Running simulation for parameter setting = ', i) )

  Simrun[[i]]            <- SimulateLobsterMovement(p)
  Results[[i]]           <- GetSimOutput(Simrun[[i]])
  resultdfcomplete[[i]]  <- GetdfComplete(x = Results[[i]], extp = p)


  saveRDS(object = resultdfcomplete[[i]], file = paste0('results_for_debug/Set_', i, '_resultdfcomplete', '.rds'))
}
