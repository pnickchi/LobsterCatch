# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(usethis)
library(devtools)

load_all()
document()


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

# Defining values for variable
initlambda          <- c(0.1, 0.5, 1, 1.6) #lobster density to start the simulation
howClose            <- c(0.5) #The distance from trap withing which a lobster considered trapped
shrinkage           <- c(0.993) #The rate of bait shrinkage
dStep               <- c(6,7,10) #Distance each lobster moves in a timestep

nsettings <- length(initlambda) * length(howClose) * length(shrinkage) * length(dStep)


# Variables with fixed values: can be also changed if needed
nrowgrids           <- rep(200, nsettings)
ncolgrids           <- rep(200, nsettings)
unitarea            <- rep(100, nsettings)
initD               <- rep(3,   nsettings)
currentZoI          <- rep(15,  nsettings)
radiusOfInfluence   <- rep(15,  nsettings)
saturationThreshold <- rep(5,   nsettings)
Trap                <- rep(list( data.frame(x = c(100), y = c(100)) ), nsettings)
ntraps              <- unlist( lapply(X = Trap, nrow) )
lobLengthThreshold  <- rep(115, nsettings)
q0                  <- rep(0.5, nsettings)
realizations        <- rep(50,  nsettings)#number of simulations
tSteps              <- rep(50, nsettings)
sexBased            <- rep(TRUE, nsettings)
lengthBased         <- rep(TRUE, nsettings)
trapSaturation      <- rep(FALSE, nsettings)
qmin                <- rep(0.5, nsettings) #need to change to 0 when trap saturation is TRUE


lobsterSizeFile     <- 'https://raw.githubusercontent.com/vpourfaraj/lobsterCatch/main/inst/extdata/LobsterSizeFreqs.csv'
lobsterSexDist      <- list(labels = c('M','F','MM','BF'),
                            prob1 = c(0.55,0.35,0.05,0.05),
                            prob2 = c(0.5,0.50,0,0),
                            lobsterMatThreshold = 100)


temp <- expand.grid(initlambda, howClose, shrinkage, dStep)
initlambda <- temp$Var1
howClose   <- temp$Var2
shrinkage  <- temp$Var3
dStep      <- temp$Var4


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





# Loop over list, initialize a parameter list and execute the simulations

Simrun  <- list()
Results <- list()
resultdfcomplete <- list()
for(i in 1:nsettings){

  p <- list()
  p$nrowgrids            <- param$nrowgrids[i]
  p$ncolgrids            <- param$ncolgrids[i]
  p$ngrids               <- param$nrowgrids[i] * param$ncolgrids[i]
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

  print( paste0('Run the simulation for parameter setting = ', i) )

  Simrun[[i]]            <- SimulateLobsterMovement(p)
  Results[[i]]           <- GetSimOutput(Simrun[[i]])
  resultdfcomplete[[i]]  <- GetdfComplete(x = Results[[i]], extp = p)


  saveRDS(object = resultdfcomplete[[i]], file = paste0('Setn_', i, '_localdepletion', '.rds'))
}
