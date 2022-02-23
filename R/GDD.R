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
#' \code{equation} character to specify the equation to be used, one of \code{"default"},
#' \code{"a"}, \code{"b"} or \code{"c"}. See Equations below 
#' 
#' \code{tbase_max} optional, the maximum tbase temperature, 
#'  required if \code{equation = "c"}   
#' 
#' \code{return.as} character (one of, the default, \code{"acc"} or \code{"daily"},
#'  \code{"ndays"}) to select if the function returns the accumulated gdd, or the 
#'  daily values of gdd across the series, or the number of days required to reach a
#'  certain number of \code{degree.days}
#'  
#' \code{degree.days} an integer for the accumulated degree-days required by the 
#'  organism. Optional if \var{return.as = "daily"} or \var{return.as = "acc"} 
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
#'  \code{"a"}: adjust tmean = tbase if tmeam < tbase
#'  
#'  \code{"b"}: adjust tmin = tbase if tmin < tbase,
#'   adjust tmax = tbase if tmax < tbase
#'  
#'  \code{"c"}: adjust tmin = tbase if tmin < tbase, 
#'   adjust tmax = tbase_max if tmax < tbase_max
#' 
#' @references 
#' Prentice I. C., et al. (1992) Journal of Biogeography, 19(2), 117.
#' 
#' Baskerville, G., & Emin, P. (1969). Ecology, 50(3), 514-517. 
#' \doi{https://doi.org/10.2307/1933912}
#' 
#' @examples
#' data("innlandet", package = "climatrends")
#' 
#' # use the default equation
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
  return.as <- dots[["return.as"]]
  degree.days <- dots[["degree.days"]]
  tbase_max <- dots[["tbase_max"]]
  
  if (is.null(return.as)) {
    return.as <- "acc" 
  }
  
  if (is.null(equation)) {
    equation <- "default"
  }
  
  result <- gdd(tmax = object,
                tmin = tmin,
                tbase = tbase, 
                tbase_max = tbase_max,
                degree.days = degree.days, 
                equation = equation, 
                return.as = return.as)
  
  result <- data.frame(gdd = result, stringsAsFactors = FALSE)
  
  class(result) <- union("clima_df", class(result))

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
  
  temp <- split(temp, temp$id)
  
  result <- lapply(temp, function(x){
    
    gd <- gdd(x$tmax,
               x$tmin,
               tbase = tbase, 
               tbase_max = tbase_max,
               degree.days = degree.days, 
               equation = equation, 
               return.as = return.as)
    
    r <- data.frame(gdd = gd, 
                    stringsAsFactors = FALSE)
    
    if (isFALSE(return.as=="ndays")) {
      i <- rep(x$id[1], length(gd))
      r <- cbind(r, dates = x$date, id = i)
      r <- r[,c("id","dates","gdd")]
    }
    
    return(r)
  })
  
  result <- do.call("rbind", result)
  
  class(result) <- union("clima_df", class(result))
  
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
  
  temp <- split(temp, temp$id)
  
  result <- lapply(temp, function(x){
    
    gd <- gdd(x$tmax,
               x$tmin,
               tbase = tbase, 
               tbase_max = tbase_max,
               degree.days = degree.days, 
               equation = equation, 
               return.as = return.as)
    
    r <- data.frame(gdd = gd, 
                    stringsAsFactors = FALSE)
    
    if (isFALSE(return.as=="ndays")) {
      i <- rep(x$id[1], length(gd))
      r <- cbind(r, dates = x$date, id = i)
      r <- r[,c("id","dates","gdd")]
    }
    
    return(r)
  })
  
  result <- do.call("rbind", result)
  
  class(result) <- union("clima_df", class(result))
  
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
  
  temp <- split(temp, temp$id)
  
  result <- lapply(temp, function(x){
    
    gd <- gdd(x$tmax,
               x$tmin,
               tbase = tbase, 
               tbase_max = tbase_max,
               degree.days = degree.days, 
               equation = equation, 
               return.as = return.as)
    
    r <- data.frame(gdd = gd, 
                    stringsAsFactors = FALSE)
    
    if (isFALSE(return.as=="ndays")) {
      i <- rep(x$id[1], length(gd))
      r <- cbind(r, dates = x$date, id = i)
      r <- r[,c("id","dates","gdd")]
    }
    
    return(r)
    
  })
  
  result <- do.call("rbind", result)
  
  class(result) <- union("clima_df", class(result))
  
  if (all(as.sf, return.as == "ndays")) {
    result <- cbind(object, result)
  }
  
  return(result)
}

#' Growing degree-days
#' 
#' This is the main function, the others are handling methods
#' 
#' @param tmax a numeric vector with the maximum temperature
#' @param tmin a numeric vector with the minimum temperature
#' @param tbase the minimum temperature for growth
#' @param tbase_max the maximum temperature for growth
#' @param degree.days number of accumulated gdd required if return.as = "ndays"
#' @param equation the equation to adjust gdd calculation
#' @param return.as how the values will be returned
#' @examples 
#' data(innlandet, package = "climatrends")
#' 
#' gdd(innlandet$tmax, innlandet$tmin, tbase = 3, 
#'      equation = "b")
#' 
#' gdd(innlandet$tmax, innlandet$tmin, tbase = 3, 
#'      equation = "b", return.as = "daily")
#' 
#' gdd(innlandet$tmax, innlandet$tmin, tbase = 3, degree.days = 100,
#'      equation = "b", return.as = "ndays")
#' 
#' @noRd
gdd <- function(tmax, tmin, tbase, tbase_max = 45, 
                 degree.days = NULL, equation = "default", return.as = "acc"){
  
  eq <- c("default", "a", "b", "c")
  ret <- c("acc", "daily", "ndays")
  
  if (isFALSE(equation %in% eq)) {
    stop("No visible method to compute GDD with equation = '", equation, 
         "', please provide one of the options: ", 
         paste("'", eq,"'" , collapse = ", ", sep = ""), "\n")
  }
  
  if (isFALSE(return.as %in% ret)) {
    stop("No visible method to return GDD as '", return.as, 
         "', please provide one of the options: ", 
         paste("'", ret,"'" , collapse = ", ", sep = ""))
  }
  
  if (all(return.as == "ndays", is.null(degree.days))) {
    stop("Please provide a value for the argument 'degree.days' \n")
  }
  
  if (isTRUE(equation == "default")) {
    g <- ((tmax + tmin) / 2) - tbase
  }
  
  if (isTRUE(equation == "a")) {
    g <- ((tmax + tmin) / 2) - tbase
    g[g < 0] <- 0
  }
    
  if (isTRUE(equation == "b")) {
    tmin[tmin < tbase] <- tbase
    tmax[tmax < tbase] <- tbase
    g <- ((tmax + tmin) / 2) - tbase
  }
  
  if (isTRUE(equation == "c")) {
    tmax[tmax > tbase_max] <- tbase_max
    tmin[tmin < tbase] <- tbase
    g <- ((tmax + tmin) / 2) - tbase
  }
    
  g[g < 0] <- 0
    
  # sum temperature values until reach the defined degree days
  if (isTRUE(return.as == "ndays")) {
    g[is.na(g)] <- 0
    g <- cumsum(g)
    g <- sum(g <= degree.days)
    return(g)
  }
    
 if (isTRUE(return.as == "daily")) {
    return(g)
  }
    
 if(isTRUE(return.as == "acc")) {
    g <- cumsum(g)
    return(g)
 }

}
