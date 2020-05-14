#' Crop sensitive indices during anthesis
#' 
#' Compute crop sensitive indices. These indices are intended to capture 
#'  the changes in temperature extremes during anthesis, but can also be 
#'  applied to other phenological stages.
#' 
#' @family temperature functions
#' @inheritParams temperature
#' @details 
#' 
#' The function uses pre-defined threshold to compute the indices. For hts_mean (32,
#'  35, 38 Celsius), for hts_max (36, 39, 42 Celsius) and for hse (31 Celsius). 
#'  See Additional arguments 
#' 
#' Additional arguments:
#' 
#' The thresholds can be adjusted using the arguments \code{hts_mean.threshold}, 
#'  \code{hts_max.threshold} and \code{hse.threshold}, which are a numeric (or
#'  vector of numeric)
#' 
#' \code{last.day}: an object (optional to \var{span}) of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD) for the last day of the time series
#'  
#' \code{span}: an integer (optional to \var{last.day}) or a vector with 
#'  integers (optional if \var{last.day} is given) for the length of 
#'  the time series to be captured
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
#' @source 
#' Challinor et al. (2016). Nature Climate Change 6(10):6954-958
#' \cr\url{https://doi.org/10.1038/nclimate3061}
#' 
#' Trnka et al. (2014). Nature Climate Change 4(7):637–43.
#' \cr\url{https://doi.org/10.1038/nclimate2242}
#' 
#' @examples 
#' data("modis", package = "climatrends")
#'  
#' anthesis(modis, 
#'          day.one = "2013-10-27", 
#'          last.day = "2013-11-04", 
#'          hse.threshold = c(33.5))
#' 
#' @export
anthesis <- function(object, ...){
  UseMethod("anthesis")
}

#' @rdname anthesis
#' @method anthesis default
#' @export
anthesis.default <- function(object, day.one, ...) {
  
  dots <- list(...)
  pars <- dots[["pars"]]
  hts_mean <- dots[["hts_mean.threshold"]]
  hts_max <- dots[["hts_max.threshold"]]
  hse <- dots[["hse.threshold"]]
  
  
  # coerce inputs to data.frame
  object <- as.data.frame(object)
  
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In anthesis.default(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  temp <- cbind(dat[[1]], min = dat[[2]]$value)
  
  result <- .anthesis(temp = temp, 
                      hts_mean.threshold = hts_mean,
                      hts_max.threshold = hts_max,
                      hse.threshold = hse)
  
  return(result)
  
}


#' @rdname anthesis
#' @method anthesis array
#' @export
anthesis.array <- function(object, day.one, ...){
  
  
  if (dim(object)[[2]] == 2) {
    UseMethod("anthesis", "default")
  }
  
  dots <- list(...)
  hts_mean <- dots[["hts_mean.threshold"]]
  hts_max <- dots[["hts_max.threshold"]]
  hse <- dots[["hse.threshold"]]
  
  # coerce to data.frame
  day.one <- as.vector(t(day.one))
  
  ts <- get_timeseries(object, day.one, ...)
  
  temp <- cbind(ts[[1]], min = ts[[2]]$value)
  
  result <- .anthesis(temp = temp,
                      hts_mean.threshold = hts_mean,
                      hts_max.threshold = hts_max,
                      hse.threshold = hse)
  
  return(result)
  
}

#' @rdname anthesis
#' @method anthesis clima_ls
#' @export
anthesis.clima_ls <- function(object, ...) {
  
  dots <- list(...)
  hts_mean <- dots[["hts_mean.threshold"]]
  hts_max <- dots[["hts_max.threshold"]]
  hse <- dots[["hse.threshold"]]
  
  temp <- cbind(object[[1]], min = object[[2]]$value)
  
  result <- .anthesis(temp = temp,
                      hts_mean.threshold = hts_mean,
                      hts_max.threshold = hts_max,
                      hse.threshold = hse)
  
  return(result)
  
}

#' @rdname anthesis
#' @method anthesis sf
#' @export
anthesis.sf <- function(object, day.one, ..., as.sf = TRUE){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  hts_mean <- dots[["hts_mean.threshold"]]
  hts_max <- dots[["hts_max.threshold"]]
  hse <- dots[["hse.threshold"]]
  
  
  day.one <- as.vector(t(day.one))
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  temp <- cbind(dat[[1]], min = dat[[2]]$value)
  
  result <- .anthesis(temp = temp, 
                      hts_mean.threshold = hts_mean,
                      hts_max.threshold = hts_max,
                      hse.threshold = hse)
  
  if (isTRUE(as.sf)) {
    
    result <- suppressWarnings(sf::st_bind_cols(object, result))
    
  }
  
  return(result)
  
}

# general function
.anthesis <- function(temp, 
                      hts_mean.threshold = NULL,
                      hts_max.threshold = NULL,
                      hse.threshold = NULL) {
  
  if (is.null(hts_mean.threshold)) {
    hts_mean.threshold <- c(32, 35, 38)
  }
  
  if (is.null(hts_max.threshold)) {
    hts_max.threshold <- c(36, 39, 42)
  }
  
  if (is.null(hse.threshold)) {
    hse.threshold <- c(31)
  }
  
  
  X <- split(temp, temp$id)
  
  names_r <- c(paste0("hts_mean_", hts_mean.threshold),
               paste0("hts_max_", hts_max.threshold),
               paste0("hse_", hse.threshold),
               paste0("hse_ms_", hse.threshold))
  
  X <- lapply(X, function(y){
    
    max <- y$value
    min <- y$min
    
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
