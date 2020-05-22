#' Crop sensitive indices
#' 
#' Compute crop sensitive indices. These indices are designed to capture 
#'  the changes in temperature extremes during key phenological stages 
#'  (e.g. anthesis), but can also be applied to other phenological stages.
#' 
#' @family temperature functions
#' @inheritParams temperature
#' @details 
#' 
#' The function uses pre-defined threshold to compute the indices. For hts_mean (32,
#'  35, 38 Celsius), for hts_max (36, 39, 42 Celsius), for hse (31 Celsius), for 
#'  cdi_mean (22, 23, 24 Celsius), for cdi_max (27, 28, 29 Celsius) and for 
#'  lethal (43, 46, 49 Celsius). 
#' 
#' Additional arguments:
#' 
#' The thresholds can be adjusted using the arguments \code{hts_mean.threshold}, 
#'  \code{hts_max.threshold}, \code{hse.threshold}, \code{cdi_mean.threshold}, 
#'  \code{cdi_max.threshold} and \code{lethal.threshold} which are a numeric (or
#'  vector of numeric)
#' 
#' \code{last.day}: an object (optional to \var{span}) of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD) for the last day of the time series. For \code{data.frame}, \code{array} 
#'  and \code{sf} methods
#'  
#' \code{span}: an integer (optional to \var{last.day}) or a vector with 
#'  integers (optional if \var{last.day} is given) for the length of 
#'  the time series to be captured. For \code{data.frame}, \code{array} 
#'  and \code{sf} methods
#' 
#' @return A dataframe with crop sensitive indices with n colunms depending on the 
#'  number of thresholds passed to each index:
#' \item{hts_mean}{high temperature stress using daily MEAN temperature, 
#'  and given as percentage number of days a certain threshold is exceeded}
#' \item{hts_max}{high temperature stress using daily MAX temperature,
#'  and given as percentage number of days a certain threshold is exceeded}
#' \item{hse}{heat stress event, and given as percentage number of days a 
#'  a certain threshold is exceeded for at least two consecutive days}
#' \item{hse_ms}{heat stress event, and given the maximum number of days 
#'  a certain threshold is exceeded for at least two consecutive days}
#' \item{cdi_mean}{crop duration index using daily MEAN temperature, 
#'  and given as max(Tmean - threshold, 0)}
#' \item{cdi_max}{crop duration index using daily MAX temperature, 
#'  and given as max(Tmax - threshold, 0)}
#' \item{lethal}{lethal temperatures, defined as percentage of days during the 
#'  timeseries where daily MEAN temperature exceeds a given threshold}
#' @references
#' Challinor et al. (2016). Nature Climate Change 6(10):6954-958
#' \cr\url{https://doi.org/10.1038/nclimate3061}
#' 
#' Trnka et al. (2014). Nature Climate Change 4(7):637–43.
#' \cr\url{https://doi.org/10.1038/nclimate2242}
#' 
#' @examples 
#' # the default method
#' set.seed(78)
#' tmax <- runif(50, 37, 47)
#' set.seed(79)
#' tmin <- runif(50, 31, 34)
#' 
#' crop_sensitive(tmax, tmin)
#' 
#' ###############################################
#' 
#' # the array method
#' data("temp_dat", package = "climatrends")
#' 
#' # use the default thresholds
#' crop_sensitive(temp_dat,
#'                day.one = "2013-10-27",
#'                last.day = "2013-11-04")
#' 
#' # or change the thresholds based on the crop physiology
#' crop_sensitive(temp_dat,
#'                day.one = "2013-10-27",
#'                last.day = "2013-11-04",
#'                hts_mean.threshold = c(24),
#'                hts_max.threshold = c(31, 33))
#' @export
crop_sensitive <- function(object, ...){
  UseMethod("crop_sensitive")
}

#' @rdname crop_sensitive
#' @method crop_sensitive default
#' @export
crop_sensitive.default <- function(object, tmin, ...) {
  
  dots <- list(...)
  hts_mean <- dots[["hts_mean.threshold"]]
  hts_max <- dots[["hts_max.threshold"]]
  hse <- dots[["hse.threshold"]]
  cdi_mean <- dots[["cdi_mean.threshold"]]
  cdi_max <- dots[["cdi_max.threshold"]]
  lethal <- dots[["lethal.threshold"]]
  
  temp <- data.frame(id = 1, 
                     tmax = object, 
                     tmin = tmin,
                     stringsAsFactors = FALSE)
  
  result <- .crop_sensitive(temp = temp, 
                            hts_mean.threshold = hts_mean,
                            hts_max.threshold = hts_max,
                            hse.threshold = hse,
                            cdi_mean.threshold = cdi_mean,
                            cdi_max.threshold = cdi_max,
                            lethal.threshold = lethal)
  
  return(result)
  
}

#' @rdname crop_sensitive
#' @method crop_sensitive data.frame
#' @export
crop_sensitive.data.frame <- function(object, day.one, ...) {
  
  dots <- list(...)
  pars <- dots[["pars"]]
  hts_mean <- dots[["hts_mean.threshold"]]
  hts_max <- dots[["hts_max.threshold"]]
  hse <- dots[["hse.threshold"]]
  cdi_mean <- dots[["cdi_mean.threshold"]]
  cdi_max <- dots[["cdi_max.threshold"]]
  lethal <- dots[["lethal.threshold"]]
  
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In crop_sensitive.default(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  temp <- cbind(dat[[1]], tmin = dat[[2]]$value)
  
  names(temp)[names(temp)=="value"] <- "tmax"
  
  result <- .crop_sensitive(temp = temp, 
                      hts_mean.threshold = hts_mean,
                      hts_max.threshold = hts_max,
                      hse.threshold = hse,
                      cdi_mean.threshold = cdi_mean,
                      cdi_max.threshold = cdi_max,
                      lethal.threshold = lethal)
  
  return(result)
  
}

#' @rdname crop_sensitive
#' @method crop_sensitive array
#' @export
crop_sensitive.array <- function(object, day.one, ...){
  
  dots <- list(...)
  hts_mean <- dots[["hts_mean.threshold"]]
  hts_max <- dots[["hts_max.threshold"]]
  hse <- dots[["hse.threshold"]]
  cdi_mean <- dots[["cdi_mean.threshold"]]
  cdi_max <- dots[["cdi_max.threshold"]]
  lethal <- dots[["lethal.threshold"]]
  
  # coerce to data.frame
  day.one <- as.vector(t(day.one))
  
  ts <- get_timeseries(object, day.one, ...)
  
  temp <- cbind(ts[[1]], tmin = ts[[2]]$value)
  
  names(temp)[names(temp)=="value"] <- "tmax"
  
  result <- .crop_sensitive(temp = temp, 
                      hts_mean.threshold = hts_mean,
                      hts_max.threshold = hts_max,
                      hse.threshold = hse,
                      cdi_mean.threshold = cdi_mean,
                      cdi_max.threshold = cdi_max,
                      lethal.threshold = lethal)
  
  return(result)
  
}

#' @rdname crop_sensitive
#' @method crop_sensitive sf
#' @export
crop_sensitive.sf <- function(object, day.one, ..., as.sf = TRUE){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  hts_mean <- dots[["hts_mean.threshold"]]
  hts_max <- dots[["hts_max.threshold"]]
  hse <- dots[["hse.threshold"]]
  cdi_mean <- dots[["cdi_mean.threshold"]]
  cdi_max <- dots[["cdi_max.threshold"]]
  lethal <- dots[["lethal.threshold"]]
  
  
  day.one <- as.vector(t(day.one))
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  temp <- cbind(dat[[1]], tmin = dat[[2]]$value)
  
  names(temp)[names(temp)=="value"] <- "tmax"
  
  result <- .crop_sensitive(temp = temp, 
                      hts_mean.threshold = hts_mean,
                      hts_max.threshold = hts_max,
                      hse.threshold = hse,
                      cdi_mean.threshold = cdi_mean,
                      cdi_max.threshold = cdi_max,
                      lethal.threshold = lethal)
  
  if (isTRUE(as.sf)) {
    
    result <- cbind(object, result)
    
  }
  
  return(result)
  
}

#' Crop sensitive
#' 
#' This is the main function, the others are handling methods
#' 
#' @param temp data.frame with following values id, tmax, tmin and date
#' @examples 
#' data(innlandet, package = "climatrends")
#' 
#' .crop_sensitive(innlandet)
#' @noRd 
.crop_sensitive <- function(temp, 
                            hts_mean.threshold = NULL,
                            hts_max.threshold = NULL,
                            hse.threshold = NULL,
                            cdi_mean.threshold = NULL,
                            cdi_max.threshold = NULL,
                            lethal.threshold = NULL) {
  
  if (is.null(hts_mean.threshold)) {
    hts_mean.threshold <- c(32, 35, 38)
  }
  
  if (is.null(hts_max.threshold)) {
    hts_max.threshold <- c(36, 39, 42)
  }
  
  if (is.null(hse.threshold)) {
    hse.threshold <- c(31)
  }
  
  if (is.null(cdi_mean.threshold)) {
    cdi_mean.threshold <- c(22, 23, 24)
  }
  
  if (is.null(cdi_max.threshold)) {
    cdi_max.threshold <- c(27, 28, 29)
  }
  
  if (is.null(lethal.threshold)) {
    lethal.threshold <- c(43, 46, 49)
  }
  
  
  X <- split(temp, temp$id)
  
  names_r <- c(paste0("hts_mean_", hts_mean.threshold),
               paste0("hts_max_", hts_max.threshold),
               paste0("hse_", hse.threshold),
               paste0("hse_ms_", hse.threshold),
               paste0("cdi_mean_", cdi_mean.threshold),
               paste0("cdi_max_", cdi_max.threshold),
               paste0("lethal_", lethal.threshold))
  
  X <- lapply(X, function(y){
    
    max <- y$tmax
    min <- y$tmin
    
    c(
      sapply(hts_mean.threshold, function(x){
        .hts(max, min, threshold = x)
      }),
      
      sapply(hts_max.threshold, function(x){
        .hts(max, NULL, threshold = x)
      }),
      
      sapply(hse.threshold, function(x){
        .hse2(max, threshold = x)
      }),
      
      sapply(hse.threshold, function(x){
        .hse(max, threshold = x)
      }),
      
      sapply(cdi_mean.threshold, function(x){
        .cdi(max, NULL, threshold = x)
      }),
      
      sapply(cdi_max.threshold, function(x){
        .cdi(max, min, threshold = x)
      }),
      
      sapply(lethal.threshold, function(x){
        .hts(max, min, threshold = x)
      })
    )
    
  })
  
  X <- do.call("rbind", X)
  
  X <- as.data.frame(X)
  
  names(X) <- names_r
  
  class(X) <- union("clima_df", class(X))
  
  return(X)
  
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


#' Heat stress event
#' 
#' When the Tmax is above threshold for at least two days
#' 
#' @param x a numeric vector
#' @return the maximum number of days when Tmax > threshold
#' @examples 
#' set.seed(871)
#' x <- rnorm(10, 32)
#' 
#' .hse(x)
#' 
#' @noRd
.hse <- function(x, threshold = 31){
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  # days above threshold should be returned as 0s
  a <- as.integer(x < threshold)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(a)$values
  
  keep <- keep == 0
  
  a <- rle(a)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  if (length(a) == 0) {
    a <- 0L
  }
  # if there is values, take the maximum sequecy
  if (length(a) != 0) {
    a <- max(a, na.rm = TRUE)
    a <- as.integer(a)
  }
  
  # if 1 then set to 0
  if (a == 1) {
    a <- 0
  }
  
  return(a)
  
}

#' Heat stress event proportion
#' 
#' When the Tmax is above threshold for at least two days
#' 
#' @param x a numeric vector
#' @return the percentage of days when Tmax > threshold for at least two days 
#' @examples 
#' set.seed(871)
#' x <- runif(10, 33, 35)
#' 
#' .hse2(x)
#' 
#' @noRd
.hse2 <- function(x, threshold = 31){
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  # days above threshold should be returned as 0s
  a <- as.integer(x < threshold)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(a)$values
  
  keep <- keep == 0
  
  a <- rle(a)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  if (length(a) == 0) {
    a <- 0L
  }
  
  # if there is values, take the sum of values >= 2
  if (length(a) != 0) {
    
    a[a < 2] <- 0
    
    a <- sum(a)
    
    l <- length(x)
    
    a <- a / l
    
  }
  
  return(a)
  
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
#' .cdi(x, threshold = 32)
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
