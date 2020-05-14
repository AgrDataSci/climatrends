#' Seasonal crop sensitive indices
#' 
#' Compute seasonal crop sensitive indices. These indices are intended to 
#'  capture the changes in temperature extremes during a whole season, 
#'  but can also be applied to specific phenological stages.
#' 
#' @inheritParams temperature




#' Crop duration index
#' 
#' Calculated using the mean growing season temperature (or the mean daily
#'  maximum growing season temperature). If Tmean > threshold then 
#'  CDI = Tmean - threshold, otherwise CDI = 0
#' 
#' @param x maximum day temperature
#' @param y optional, minimum temperature
#' @return the HTS index
#' @examples 
#' # cdi with mean daily maximum temperature
#' x <- modis[3, ,1]
#' .cdi(x, threshold = 36)
#' 
#' # cdi with mean temperature
#' x <- modis[1,,1]
#' y <- modis[1,,2]
#' .cdi(x,y, threshold = 32)
#' 
#' @noRd
.cdi <- function(x, y = NULL, threshold = NULL){
  
  # if y is provided than take the mean of x and y
  if (isFALSE(is.null(y))) {
    x <- (x + y) / 2
  }
  
  # take the mean
  tmean <- mean(x, na.rm = TRUE)
  
  # the mean minus the threshold
  tmean <- tmean - threshold
  
  # take the max between tmean and 0
  r <- max(tmean, 0)
  
  return(r)
  
}


.lsf <- function(x, y = NULL, threshold = -2){
  
}
