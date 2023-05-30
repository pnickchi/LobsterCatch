library(LobsterCatch)

# Most of the values were taken from Addison and Bell (1997): https://doi.org/10.1071/MF97169
#Create a list of parameters
p = list()

p$nrowgrids = 200
p$ncolgrids = 200
p$ngrids = p$nrowgrids * p$ncolgrids
p$unitarea = 100

p$initlambda = 0.5 # Initial density of lobster per unit area
p$dStep = 1
p$howClose = 0.5

p$initD = 3  #Initial Dispersion of lobster
p$shrinkage = 0.993 #initial shrinkage

p$currentZoI = 15
p$radiusOfInfluence = 15

p$q0 = 0.5
p$qmin = 0 # set to 0 for initial param and to 0.5 when no there is no trap saturation
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

p$realizations = 100 #number of iterations/simulations
p$tSteps = 50       #timesteps per iteration



Simrun <- SimulateLobsterMovement(p)

Results  <- GetSimOutput(Simrun)

