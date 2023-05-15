#' This function replicates the coordinates where there are multiple lobsters
#' @param d is a data frame containing  x and y coordinates of lobsters and number of lobsters at each coordinate
#' @return Returns a data frame
replicateCoordinates <- function(d){ rep(d[1:2], d[3]) }
