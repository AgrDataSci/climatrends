#' Reference evapotranspiration
#' 
#' Reference evapotranspiration using the Blaney-Criddle method. This is general  
#' theoretical method used when no measured data on pan evaporation 
#' is available locally. 
#' 
#' @inheritParams temperature
#' @param lat a vector for the latitude (in Decimal degrees), used to compute 
#'  mean daily percentage of annual daytime hours based on the latitude and month.
#'  This is extracted automatically in the \code{sf} method. See details
#' @param Kc a numeric value for the crop factor for water requirement
#' @param p optional if \var{lat} is given, a numeric for the mean daily percentage 
#'  of annual daytime hours (p = 0.27 by default)
#' @param month an integer for the reference month of daylight percentage
#' @return The evapotranspiration in mm/day
#' @details 
#' When \var{lat} is provided, it is combined with the month provided in 
#'  \var{day.one} to call for the system data \code{daylight} to find
#'  the correct value for \var{p} which represents the daily percentage
#'  of daytime hours in the given month and latitude. Otherwise \var{p} is set 
#'  to 0.27 as default.
#'  
#' The \code{array} method assumes that \var{object} contains climate data available 
#'  in your R section; this requires an array with two dimensions, 1st dimension 
#'  contains the day temperature and 2nd dimension the night temperature, 
#'  see help("temp_dat", package = "climatrends") for an example on input structure.
#' 
#' The \code{data.frame} method and the \code{sf} method assumes that the climate data
#'  will be fetched from a remote (cloud) source that be adjusted using the argument 
#'  \var{data.from}.
#' 
#' Additional arguments:
#' 
#' \code{last.day}: an object (optional to \var{span}) of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD) for the last day of the time series
#'  
#' \code{span}: an integer (optional to \var{last.day}) or a vector with 
#'  integers (optional if \var{last.day} is given) for the length of 
#'  the time series to be captured
#' 
#' \code{data.from}: character for the source of climate data. Current remote data 
#'  is: 'nasapower'
#' 
#' \code{pars}: character vector for the temperature data to be fetched. If 
#'  \code{data.from} is 'nasapower'. The temperature can be adjusted to 2 m, the default,
#'  c("T2M_MAX", "T2M_MIN") or 10 m c("T10M_MAX", "T10M_MIN") 
#' 
#' \code{days.before}: optional, an integer for the number of days before 
#'  \var{day.one} to be included in the timespan.
#' 
#' @family temperature functions
#' @references
#' Brouwer C. & Heibloem M. (1986). Irrigation water management: 
#' Irrigation water needs. Food and Agriculture Organization of The 
#' United Nations, Rome, Italy. 
#' \url{https://www.fao.org/3/S2022E/s2022e00.htm}
#' 
#' @examples
#' # the default method
#' set.seed(78)
#' tmax <- runif(50, 37, 47)
#' set.seed(79)
#' tmin <- runif(50, 31, 34)
#' 
#' ETo(tmax, tmin, lat = 22, month = 10)
#' 
#' ###############################################
#' 
#' # the array method
#' data("temp_dat", package = "climatrends")
#' 
#' ETo(temp_dat, 
#'     day.one = "2013-10-28",
#'     span = 10,
#'     Kc = 0.92)
#'     
#' @export
ETo <- function(object, ..., Kc = 1){
  
  UseMethod("ETo")

}

#' @rdname ETo
#' @method ETo default
#' @export
ETo.default <- function(object, tmin, ..., Kc = 1, lat = NULL, month = NULL){
  
  dots <- list(...)
  p <- dots[["p"]]
  
  # get p if lat is provided
  if (all(!is.null(lat), !is.null(month))) {
    m <- as.integer(month)
    p <- .p_daytime(lat = lat, month = m)
  }
  
  if (is.null(p)) {
    p <- 0.27
  }
  
  temp <- data.frame(id = 1, 
                     tmax = object, 
                     tmin = tmin,
                     stringsAsFactors = FALSE)
  
  result <- .eto(temp, Kc, p)
  
  return(result)
  
}

#' @rdname ETo
#' @method ETo data.frame
#' @export
ETo.data.frame <- function(object, day.one, ..., Kc = 1){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  # coerce inputs to data.frame
  object <- as.data.frame(object)
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In ETo.default(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.vector(t(day.one))
  
  # get the latitude
  lat <- object[, 2]
  
  # check if day.one is a 'Date' else try to coerce to Date
  if (!.is_Date(day.one)) {
    
    day.one <- .coerce2Date(day.one)
    
  }
  
  # get p using lat and day.one
  m <- as.integer(format(day.one, "%m"))
  
  p <- .p_daytime(lat = lat, month = m)
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  temp <- cbind(dat[[pars[[1]]]], tmin = dat[[pars[[2]]]]$value)
  
  names(temp)[names(temp) == "value"] <- "tmax"
  
  result <- .eto(temp, Kc, p)
  
  return(result)
}


#' @rdname ETo
#' @method ETo array
#' @export
ETo.array <- function(object, day.one, ..., Kc = 1, lat = NULL, p = 0.27){
  
  dots <- list(...)
  span <- dots[["span"]]
  last.day <- dots[["last.day"]]
  
  # coerce to vector
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

  # get p if lat is provided
  if (!is.null(lat)) {
    m <- as.integer(format(day.one, "%m"))
    p <- .p_daytime(lat = lat, month = m)
  }
  
  dat <- get_timeseries(object, day.one, span, last.day)
  
  temp <- cbind(dat[[1]], tmin = dat[[2]]$value)
  
  names(temp)[names(temp) == "value"] <- "tmax"
  
  result <- .eto(temp, Kc, p)
  
  return(result)
  
}


#' @rdname ETo
#' @method ETo sf
#' @export
ETo.sf <- function(object, day.one, ..., Kc = 1, as.sf = TRUE){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  # get lat
  lat <- .lonlat_from_sf(object)
  lat <- lat[, 2]
  
  day.one <- as.vector(t(day.one))
  
  # check if day.one is a 'Date' else try to coerce to Date
  if (!.is_Date(day.one)) {
    
    day.one <- .coerce2Date(day.one)
    
  }
  
  # get p
  m <- as.integer(format(day.one, "%m"))
  p <- .p_daytime(lat = lat, month = m)
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  temp <- cbind(dat[[pars[[1]]]], tmin = dat[[pars[[2]]]]$value)
  
  names(temp)[names(temp) == "value"] <- "tmax"
  
  result <- .eto(temp, Kc, p)
  
  if (isTRUE(as.sf)) {
    result <- cbind(object, result)
  }
  
  return(result)
  
}

#' Evapotranspiration
#' 
#' This is the main function, the others are handling methods
#' 
#' @param temp data.frame with following values id, tmax, tmin and date
#' @param Kc the crop factor
#' @param p percentage of daylight
#' @examples 
#' data(innlandet, package = "climatrends")
#' 
#' .eto(innlandet, 0.8, 0.1)
#' @noRd 
.eto <- function(temp, Kc = 1, p = 0.27) {
  
  temp <- split(temp, temp$id)
  
  Tmean <-lapply(temp, function(x){
    (mean(x$tmax, na.rm = TRUE) + mean(x$tmin, na.rm = TRUE)) / 2
  })
  
  Tmean <- do.call("rbind", Tmean)
  
  # evapotranspiration
  eto <- p * (0.46 * Tmean + 8) * Kc
  
  result <- data.frame(ETo = eto, stringsAsFactors = FALSE)
  
  class(result) <- union("clima_df", class(result))

  return(result)
  
}


# Compute daylight percentage of daytime
# get p if lat is provided
.p_daytime <- function(lat, month){
  lat <- as.data.frame(lat)[, 1]
  
  l <- .round5(lat, 5)
  
  p <- daylight[cbind(match(l , daylight[, "y"]), match(month, names(daylight)))]
  
  return(p)

}
