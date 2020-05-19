#' Growing degree-days
#' 
#' This a heuristic tool in phenology that measures heat accumulation and 
#' is used to predict plant and animal development rates. Growing degree-days
#' are calculated by taking the integral of warmth above a base temperature.
#' 
#' @family temperature functions
#' @family GDD functions
#' @inheritParams temperature
#' @param tbase an integer for the minimum temperature for growth
#' @return 
#'  The number of days to reach the accumulated \var{degree.days} or the daily degree-days 
#'   as defined with the argument \var{return.as}
#' @details 
#' Additional arguments:
#' 
#' \code{tbase_max} optional, the maximum tbase temperature, 
#'  required if \code{equation = "c"}   
#' 
#' \code{return.as} character (one of, the default, \code{"acc"} or \code{"daily"},
#'  \code{"ndays"}) to select if the function returns the accumulated gdd, or the 
#'  daily values of gdd across the series of the number of days required to reach a
#'  certain number of \code{degree.days}
#'  
#' \code{degree.days} an integer for the accumulated degree-days required by the 
#'  organism. Optional if \var{return.as = "daily"} or \var{return.as = "acc"} 
#' 
#' \code{equation} character to specify the equation to be used, one of \code{"default"},
#' \code{"a"}, \code{"b"} or \code{"c"}. See Equations below 
#' 
#' \code{dates}: a character (or Date or numeric) vector for the dates of tmax and tmin
#'  in the \code{default} method
#' 
#' \code{last.day}: an object (optional to \var{span}) of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD) for the last day of the time series. For \code{data.frame}, 
#'  \code{array} and \code{sf} methods
#'  
#' \code{span}: an integer (optional to \var{last.day}) or a vector with 
#'  integers (optional if \var{last.day} is given) for the length of 
#'  the time series to be captured. For \code{data.frame}, \code{array} 
#'  and \code{sf} methods
#' 
#' S3 Methods:
#' 
#' The \code{array} method assumes that \var{object} contains climate data available 
#'  in your R section; this requires an array with two dimensions, 1st dimension 
#'  contains the day temperature and 2nd dimension the night temperature, 
#'  see help("temp_dat", package = "climatrends") for an example on input structure.
#' 
#' The \code{data.frame} and \code{sf} methods assumes that the climate data
#'  will e fetched from a remote (cloud) source that be adjusted using the argument 
#'  \var{data.from}
#'
#'  
#' Equations: 
#'  
#'  \code{"default"}: GDD = ((tmax + tmin) / 2) - tbase
#'  
#'  \code{"a"}: GDD = max(GDD, 0)
#'  
#'  \code{"b"}: adjust tmin = tbase if tmin < tbase,
#'   adjust tmax = tbase if tmax < tbase
#'  
#'  \code{"c"}: adjust tmin = tbase if tmin < tbase, 
#'   adjust tmax = tbase_max if tmax < tbase_max
#' 
#' @references 
#' Prentice I. C., et al. (1992) Journal of Biogeography, 19(2), 117. 
#' \cr\url{https://doi.org/10.2307/2845499}
#' 
#' Baskerville, G., & Emin, P. (1969). Ecology, 50(3), 514-517. 
#' \cr\url{https://doi.org/10.2307/1933912}
#' 
#' @examples
#' data("innlandet", package = "climatrends")
#' 
#' # use the default equation, but in this case it returns negative values
#' GDD(innlandet$tmax, innlandet$tmin, tbase = 2)
#' 
#' # set the equation "b", which is a better option for this case
#' # tmin = tbase if tmin < tbase 
#' # tmax = tbase if tmax < tbase
#' GDD(innlandet$tmax, innlandet$tmin, tbase = 2, equation = "b")
#' 
#' 
#' #####################################################
#' 
#' # return as the number of days required to reach a certain accumulated GDD
#' # use equation "c", which adjusts tmax base on a tbase_max
#' data("temp_dat", package = "climatrends")
#' 
#' GDD(temp_dat, 
#'     day.one = "2013-10-27", 
#'     degree.days = 90, 
#'     return.as = "ndays", 
#'     tbase_max = 32,
#'     equation = "c")
#' 
#' \donttest{
#' #####################################################
#' 
#' # use the S3 method for data.frame to fetch data from nasapower
#' 
#' library("nasapower")
#' 
#' lonlat <- data.frame(lon = c(-73.3, -74.5),
#'                      lat = c(-6.1, - 6.2))
#' 
#' GDD(lonlat, 
#'     day.one = "2015-05-01", 
#'     last.day = "2015-09-30",
#'     equation = "c",
#'     tbase_max = 35)
#' 
#' }
#' @export
GDD <- function(object, ..., tbase = 10)
{
  UseMethod("GDD")
}

#' @rdname GDD
#' @method GDD default
#' @export
GDD.default <- function(object, tmin, ..., tbase = 10) {
  
  dots <- list(...)
  equation <- dots[["equation"]]
  dates <- dots[["dates"]]
  return.as <- dots[["return.as"]]
  degree.days <- dots[["degree.days"]]
  tbase_max <- dots[["tbase_max"]]
  
  if (is.null(return.as)) {
    return.as <- "acc" 
  }
  
  if (is.null(equation)) {
    equation <- "default"
  }
  
  if (!is.null(dates)) {
    dates <- .coerce2Date(dates)
  }
  
  setnulldate <- FALSE
  if (is.null(dates)) {
    dates <- .coerce2Date(1:length(object))
    setnulldate <- TRUE
  }
  
  temp <- data.frame(id = 1, 
                     date = dates,
                     tmax = object, 
                     tmin = tmin, 
                     stringsAsFactors = FALSE)
  
  result <- .gdd(temp, 
                 tbase = tbase, 
                 tbase_max = tbase_max,
                 degree.days = degree.days, 
                 equation = equation, 
                 return.as = return.as)
  
  if (isTRUE(setnulldate)) {
    result <- data.frame(gdd = result[, "gdd"], stringsAsFactors = FALSE)
    class(result) <- union("clima_df", class(result))
  }
  
  return(result)
  
}
  
#' @rdname GDD
#' @method GDD data.frame
#' @export
GDD.data.frame <- function(object, day.one, ..., tbase = 10){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  equation <- dots[["equation"]]
  return.as <- dots[["return.as"]]
  degree.days <- dots[["degree.days"]]
  tbase_max <- dots[["tbase_max"]]
  
  if (is.null(return.as)) {
    return.as <- "acc" 
  }
  
  if (is.null(equation)) {
    equation <- "default"
  }
  
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In GDD.default(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  temp <- cbind(dat[[1]], tmin = dat[[2]]$value)
  
  names(temp)[names(temp)=="value"] <- "tmax"
  
  result <- .gdd(temp, 
                 tbase = tbase, 
                 tbase_max = tbase_max,
                 degree.days = degree.days, 
                 equation = equation, 
                 return.as = return.as)
  
  return(result)
}

#' @rdname GDD
#' @method GDD array
#' @export
GDD.array <- function(object, day.one, ..., tbase = 10){

  dots <- list(...)
  span <- dots[["span"]]
  last.day <- dots[["last.day"]]
  equation <- dots[["equation"]]
  return.as <- dots[["return.as"]]
  degree.days <- dots[["degree.days"]]
  tbase_max <- dots[["tbase_max"]]
  
  if (is.null(return.as)) {
    return.as <- "acc" 
  }
  
  if (is.null(equation)) {
    equation <- "default"
  }
  
  day.one <- as.vector(t(day.one))
  
  # check if day.one is a 'Date' else try to coerce to Date
  if (isFALSE(.is_Date(day.one))) {
    day.one <- .coerce2Date(day.one)
  }
  
  if (all(is.null(span), is.null(last.day))) {
    do <- as.character(max(day.one))
    do <- match(do, dimnames(object[,,1])[[2]])
    span <- dim(object)[[2]] - do
  }
  
  ts <- get_timeseries(object, day.one, span = span, last.day = last.day)
  
  temp <- cbind(ts[[1]], tmin = ts[[2]]$value)
  
  names(temp)[names(temp)=="value"] <- "tmax"
  
  result <- .gdd(temp, 
                 tbase = tbase, 
                 tbase_max = tbase_max,
                 degree.days = degree.days, 
                 equation = equation, 
                 return.as = return.as)
  
  return(result)
}

#' @rdname GDD
#' @method GDD sf
#' @export
GDD.sf <- function(object, day.one, ..., tbase = 10, as.sf = TRUE){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  equation <- dots[["equation"]]
  return.as <- dots[["return.as"]]
  degree.days <- dots[["degree.days"]]
  tbase_max <- dots[["tbase_max"]]
  
  if (is.null(return.as)) {
    return.as <- "acc" 
  }
  
  if (is.null(equation)) {
    equation <- "default"
  }
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  temp <- cbind(dat[[1]], tmin = dat[[2]]$value)
  
  names(temp)[names(temp)=="value"] <- "tmax"
  
  result <- .gdd(temp, 
                 tbase = tbase, 
                 tbase_max = tbase_max,
                 degree.days = degree.days, 
                 equation = equation, 
                 return.as = return.as)
  
  if (isTRUE(as.sf)) {
    result <- suppressWarnings(sf::st_bind_cols(object, result))
  }
  
  return(result)
}

#' Growing degree-days
#' 
#' This is the main function, the others are handling methods
#' 
#' @param temp data.frame with following values id, tmax, tmin and date
#' @param tbase the tbase
#' @param degree.days number of accumulated gdd required if return.as = "ndays"
#' @param equation the equation to adjust gdd calculation
#' @param return.as how the values will be returned
#' @examples 
#' data(innlandet, package = "climatrends")
#' 
#' .gdd(innlandet, tbase = 3, 
#'      equation = "b")
#' 
#' .gdd(innlandet, tbase = 3, 
#'      equation = "b", return.as = "daily")
#' 
#' .gdd(innlandet, tbase = 3, degree.days = 100,
#'      equation = "b", return.as = "ndays")
#' 
#' @noRd
.gdd <- function(temp, tbase, tbase_max = NULL, degree.days = NULL, 
                 equation = "default", return.as = "acc"){
  
  if (all(return.as == "ndays", is.null(degree.days))) {
    stop("argument degree.days is missing with no default \n")
  }
  
  temp <- split(temp, temp$id)
  
  suppressWarnings(
  Y <- lapply(temp, function(x){
    
    if (isTRUE(equation == "default")) {
      
      y <- .gdd_eq_default(x$tmax, x$tmin, tbase)
    
    }
    
    if (isTRUE(equation == "a")) {
      
      y <- .gdd_eq_a(x$tmax, x$tmin, tbase)
      
    }
    
    if (isTRUE(equation == "b")) {
      
      y <- .gdd_eq_b(x$tmax, x$tmin, tbase)
      
    }
    
    if (isTRUE(equation == "c")) {
      
      y <- .gdd_eq_c(x$tmax, x$tmin, tbase, tbase_max)
      
    }
    
    # sum temperature values until reach the defined degree days
    if (isTRUE(return.as == "ndays")) {
      y[is.na(y)] <- 0
      y <- cumsum(y)
      y <- sum(y <= degree.days)
      
      return(y)
      
    }
    
    if (isTRUE(return.as == "daily")) {
      
      y <- data.frame(id = as.integer(x$id),
                      date = x$date,
                      gdd = y,
                      stringsAsFactors = FALSE)
      
      return(y)
      
    }
    
    if(isTRUE(return.as == "acc")) {
      
      y <- data.frame(id = as.integer(x$id),
                      date = x$date,
                      gdd = cumsum(y),
                      stringsAsFactors = FALSE)
      
      return(y)
      
      
    }
  
  })
  )
  
  result <- do.call("rbind", Y)
  
  if (isTRUE(return.as == "ndays")) {
    
    result <- data.frame(gdd = result, stringsAsFactors = FALSE)
  
  }
  
  rownames(result) <- seq_along(result[,1])
  
  class(result) <- union("clima_df", class(result))
  
  return(result)
  
}

# not modified
# take the temperature data as it is and compute the gdd
.gdd_eq_default <- function(tmax, tmin, tbase) {
  
  g <- ((tmax + tmin) / 2) - tbase
  
  return(g)
}

# modify for max(gdd, 0) if gdd < 0
.gdd_eq_a <- function(tmax, tmin, tbase) {
  
  g <- ((tmax + tmin) / 2) - tbase
  
  g[g < 0] <- 0
  
  return(g)
  
}

# modify for tmin and tmax if 
# tmin < tbase
# tmax < tbase
.gdd_eq_b <- function(tmax, tmin, tbase){
  
  tmin_adj <- tmin
  tmax_adj <- tmax
  
  tmin_adj[tmin_adj < tbase] <- tbase
  
  tmax_adj[tmax_adj < tbase] <- tbase
  
  g <- ((tmax_adj + tmin_adj) / 2) - tbase
  
  return(g)
}

# modify for tmin and tmax if 
# tmax > tbase_max
# tmin < tbase
.gdd_eq_c <- function(tmax, tmin, tbase, tbase_max = 40){
  
  tmin_adj <- tmin
  tmax_adj <- tmax

  tmax_adj[tmax_adj > tbase_max] <- tbase_max
  
  tmin_adj[tmin_adj > tbase] <- tbase
  
  g <- ((tmax_adj + tmin_adj) / 2) - tbase

  return(g)
  
}

