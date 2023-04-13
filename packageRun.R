library(tidyverse)
library(ggplot2)

#initialize a parameter file to pass info into the code and then put all into a function
p = list()

p$nrowgrids = 200
p$ncolgrids = 200
p$ngrids = p$nrowgrids * p$ncolgrids
p$unitarea = 100

p$initlambda = 0.5 # Initial density of lobster per unit area
p$dStep = 1
p$howClose = 0.01 #initial used value was 0.5

p$initD = 3  #Initial Dispersion of lobster (initlambda and initD go into rpoissonD to randomly allocate lobster across the grid space)
p$shrinkage = 0.993 #initial shrinkage is 0.993

p$currentZoI = 15
p$radiusOfInfluence = 15

p$q0 = 0.5
p$qmin = 0 # set to 0 for initial param and to 0.5 when no there is no trap saturation (local depletion)
p$Trap = data.frame( x = c(100), y = c(100) ) # A single trap in the middle of arena
p$ntraps = nrow(p$Trap)
p$saturationThreshold = 5

p$lengthBased = TRUE
p$lobsterSizeFile <- 'https://raw.githubusercontent.com/vpourfaraj/lobsterCatch/main/inst/extdata/LobsterSizeFreqs.csv'
p$lobLengthThreshold = 115
p$trapSaturation = TRUE
p$sexBased <- TRUE
# The following lines creates a sex distribution
p$lobsterSexDist <- list(labels = c('M','F','MM','BF'), #male, female, mature male, berried female
                         prob1 = c(0.55,0.35,0.05,0.05), #their prob in population
                         prob2 = c(0.5,0.50,0,0), # prob of small males and females that are under lobsterMatThreshold
                         lobsterMatThreshold = 100)  # The average size of mature lobsters

p$realizations = 3 #number of iterations/simulations
p$tSteps = 50       #timesteps per iteration (5 was used before Feb 8th, 2023)


# p$lobsterSexDist <- ''  # in case of p$sexBased = FALSE

Simrun <- SimulateLobsterMovement(p)

Results  <- GetSimOutput(Simrun)

#unlisting the result to add parameters columns
resultsdf<- data.frame(unlist(Results, FALSE, TRUE))

#Converting to long format ( wasn't able to use pivot_longer!)
timetomax <- c(resultsdf$TimeToMax.Trap1, resultsdf$TimeToMax.Trap2, resultsdf$TimeToMax.Trap3)
maxcatchno  <- c(resultsdf$MaxCatch.Trap1, resultsdf$MaxCatch.Trap2, resultsdf$MaxCatch.Trap3)
legalcatchwt  <- c(resultsdf$LegalCatchWt.Trap1, resultsdf$LegalCatchWt.Trap2, resultsdf$LegalCatchWt.Trap3)
totalcatchwt  <- c(resultsdf$TotalCatchWt.Trap1, resultsdf$TotalCatchWt.Trap2, resultsdf$TotalCatchWt.Trap3)
#taking the params used for naming purpose
densitylambda<- rep.int(p$initlambda, p$realizations)
dstepmov<- rep.int(p$dStep,p$realizations)
saturationThreshold<- rep.int(p$saturationThreshold, p$realizations)
baitShrinkage<- rep.int(p$shrinkage, p$realizations)
resultdfcomplete <- data.frame(timetomax = timetomax,
                               maxcatchno = maxcatchno,
                               legalcatchwt = legalcatchwt,
                               totalcatchwt = totalcatchwt,
                               densitylambda = densitylambda,
                               dstepmov= dstepmov,
                               saturationThreshold=saturationThreshold,
                               baitShrinkage= baitShrinkage)
mean(resultdfcomplete[,2])

#export the result as RDS
dstepvalue<- resultdfcomplete$dstepmov[2]
densityvalue<- resultdfcomplete$densitylambda[2]
Saturationvalue<- resultdfcomplete$saturationThreshold[2]
shrinkagefactor<- resultdfcomplete$baitShrinkage[2]
saveRDS(resultdfcomplete, sprintf('dstep%s_density%s_saturation%s_shrinkage%s.rds',dstepvalue, densityvalue, Saturationvalue, shrinkagefactor))

