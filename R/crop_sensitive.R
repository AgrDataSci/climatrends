#' Crop sensitive indices
#' 
#' Compute crop sensitive indices
#' 
#' @inheritParams temperature
#' 
#' @source 
#' 
#' Challinor et al. (2016). Nature Climate Change, 6, 954-958(2016)
#' \cr\url{https://doi.org/10.1038/nclimate3061}
#' 
#' @export
crop_sensitive <- function(object, ...){
  return(NA)
}

#' High temperature stress
#' 
#' Calculated using daily mean temperature (or the daily max temperature) 
#'  and given as the percentage number of days a certain threshold is exceeded 
#' 
#' @param x maximum day temperature
#' @param y optional, minimum temperature
#' @return the HTS index
#' @examples 
#' # hts with day max temperature 
#' x <- modis[3, ,1]
#' .hts(x, threshold = 36)
#' 
#' # hts with mean daily temperature
#' x <- modis[1,,1]
#' y <- modis[1,,2]
#' .hts(x,y, threshold = 32)
#' 
#' # lethal temperatures
#' .hts(x,y, threshold = 43)
#' 
#' @noRd
.hts <- function(x, y = NULL, threshold = NULL){
  
  # if y is provided than take the mean of x and y
  if (isFALSE(is.null(y))) {
    x <- (x + y) / 2
  }
  
  # length of x
  l <- length(x)
  
  # number of values above the threshold
  i <- sum(x > threshold, na.rm = TRUE)
  
  # fraction of x above the threshold
  r <- i / l
  
  return(r)
  
}

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
