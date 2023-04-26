#' This function calculates the probability of entry to trap (aka catchability). In addition to parameters in Addison & Bell
#' 1997, length of catch can be added and included in calculation of catchability.
#' @param q0 is the initial probability of entry into an empty trap(i.e. 0.5).
#' @param qmin is the asymptotic minimum probability of entry (i.e. 0).
#' @param  saturationThreshold is the number of lobsters in a trap at which the probability of
#' another lobster entering the trap drops to qmin
#' @param Ct is the number of caught lobster
#' @param r is the instantaneous rate of change in qo with respect to Ct
#' @param lengthBased is a logical parameter(TRUE/FALSE) which determines if length of catch needs too be considered
#' @param lobLengthThreshold is a length threshold (i.e. CL in centimeters) beyond which there is no chance of catching another lobster
#' @param lobSize is a size frequency dataset which is set to NA by default
#' @param sexBased is a logical parameter(TRUE/FALSE) which determines if sex of catch needs too be considered
#' @param lobSex is the sex of the lobster
#' @return Returns the probability of entry to trap
#' @export

catchability <- function(q0, qmin, saturationThreshold, Ct, lengthBased, lobLengthThreshold,
                         lobSize = NA, sexBased, lobSex){


  if( (lengthBased == FALSE) & (sexBased == FALSE) ){
    r  <- (log(0.01) - log(q0 - qmin))/(-saturationThreshold)
    q  <- ( (q0 - qmin) / exp(r*Ct) ) + qmin
    return(q)
  }

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
