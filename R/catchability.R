#' This function calculates the probability of entry into the trap, also known as catchability. It includes the parameters described
#' in Addison and Bell (1997), and can incorporate the length of the catch while calculating the catchability.
#' @param q0 is the initial probability of entry into an empty trap (i.e. 0.5). It must be a number between 0 and 1.
#' @param qmin is the asymptotic minimum probability of entry (i.e. 0).
#' @param saturationThreshold represents the maximum number of lobsters that a trap can hold before the likelihood of another
#' lobster entering the trap decreases to qmin.
#' @param Ct is the number of caught lobster
#' @param lengthBased is a binary value (TRUE/FALSE) which determines whether the length of the lobster in simulation matters or not.
#' @param lobLengthThreshold is a length threshold (i.e. CL in centimeters) beyond which there is no chance of catching another lobster
#' @param lobSize is a size frequency dataset (maybe add some details here?). The default value is NA.
#' @param sexBased is a binary value (TRUE/FALSE) which determines whether the sex of lobster needs too be considered.
#' @param lobSex is the sex of the lobster.
#' @references Julian T. Addison and Michael C. Bell (1997), Simulation modelling of capture processes in trap fisheries for clawed
#' lobsters, Marine Freshwater Research, 48(8), 1035-1044, https://www.publish.csiro.au/MF/MF97169
#' @return Returns the probability of entry to trap.

catchability <- function(q0, qmin, saturationThreshold, Ct, lengthBased, lobLengthThreshold, lobSize = NA, sexBased, lobSex){

  # Check if the probability of entry into the trap does not depend on length and sex of lobster.
  if( (lengthBased == FALSE) & (sexBased == FALSE) ){

    # r is the instantaneous rate of change in qo with respect to Ct (number of lobsters that are caught in the trap).
    r  <- (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
    q  <- ( (q0 - qmin) / exp(r*Ct) ) + qmin
    return(q)
  }

  # Check if the probability of entry into the trap does not depend on length but depends on the sex of lobster.
  if( (lengthBased == FALSE) & (sexBased == TRUE) ){

    temp2 <- unlist( strsplit( lobSex, split = '-' ) )
    temp2 <- temp2[2:length(temp2)]

    if( any(temp2 %in% 'BF') ){
      q = 0
      return(q)
    }else{
      r = (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
      q = (q0-qmin) / exp(r*Ct) + qmin
      return(q)
    }
  }

  # Check if the probability of entry into the trap depends on length but does not depend on the sex of lobster.
  if( (lengthBased == TRUE) & (sexBased == FALSE) ){

    temp <- unlist( strsplit( lobSize, split = '-' ) )
    temp <- temp[2:length(temp)]
    temp <- as.numeric(temp)

    if( any(temp > lobLengthThreshold, na.rm = TRUE) ){
      q = 0
      return(q)
    }else{
      r = (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
      q = (q0-qmin) / exp(r*Ct) + qmin
      return(q)
    }


  }

  # Check if the catchability depends on both length and sex of lobster.
  if( (lengthBased == TRUE) & (sexBased == TRUE) ){

    temp <- unlist( strsplit( lobSize, split = '-' ) )
    temp <- temp[2:length(temp)]
    temp <- as.numeric(temp)

    temp2 <- unlist( strsplit( lobSex, split = '-' ) )
    temp2 <- temp2[2:length(temp2)]


    if( (any(temp > lobLengthThreshold, na.rm = TRUE) | ( any(temp2 %in% 'BF') ) ) ){
      q = 0
      return(q)
    }else{
      r = (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
      q = (q0-qmin) / exp(r*Ct) + qmin
      return(q)
    }

  }

}
