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
#' @return The evapotranspiration in mm/day
#' @details 
#' When \var{lat} is provided, it is combined with the month provided in 
#'  \var{day.one} to call for the system data \code{daylight} to find
#'  the correct value for \var{p} which represents the daily percentage
#'  of daytime hours in the given month and latitude. Otherwise \var{p} is set 
#'  to 0.27 as default.
#'  
#' The \code{array} method assumes that \var{object} contains climate data provided 
#'  from a local source; this requires an array with two dimensions, 1st dimension 
#'  contains the day temperature and 2nd dimension the night temperature, 
#'  see help("modis", "climatrends") for an example on input structure.
#' 
#' The \code{default} method and the \code{sf} method assumes that the climate data
#'  will e fetched from a remote (cloud) source that be adjusted using the argument 
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
#' \url{http://www.fao.org/3/S2022E/s2022e00.htm}
#' 
#' @examples
#' # Using local data
#' data("modis", package = "climatrends")
#' 
#' ETo(modis, 
#'     day.one = "2013-10-28",
#'     span = 10,
#'     Kc = 0.92)
#' 
#' \donttest{
#' ######################################
#' 
#' # Using remote data
#' # random geographic locations around bbox(11, 12, 55, 58)
#' set.seed(826128)
#' lonlat <- data.frame(lon = runif(2, 11, 12),
#'                      lat = runif(2, 55, 58))
#' 
#' # the evapotranspiration in the 30 days
#' ETo(lonlat,
#'     day.one = "2018-05-15",
#'     last.day = "2018-06-15",
#'     Kc = 0.5)
#' 
#' #######################################
#' 
#' # Objects of class 'sf'
#' data("lonlatsf", package = "climatrends")
#' 
#' ETo(lonlatsf,
#'     day.one = "2015-04-22",
#'     last.day = "2015-06-22",
#'     pars = c("T10M_MAX", "T10M_MIN"))
#' }
#' @importFrom sf st_bind_cols
#' @export
ETo <- function(object, day.one, Kc = 1, ...){
  
  UseMethod("ETo")

}

#' @rdname ETo
#' @method ETo default
#' @export
ETo.default <- function(object, day.one, Kc = 1, ...){
  
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
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  result <- .eto(day, night, Kc, p)
  
  return(result)
}


#' @rdname ETo
#' @method ETo array
#' @export
ETo.array <- function(object, day.one, Kc = 1, lat = NULL, p = 0.27, ...){
  
  if(dim(object)[[2]] == 2) {
    UseMethod("ETo", "default")
  }
  
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
  
  ts <- get_timeseries(object, day.one, span, last.day)
  
  result <- .eto(ts[[1]], ts[[2]], Kc, p)
  
  return(result)
  
}


#' @rdname ETo
#' @method ETo sf
#' @export
ETo.sf <- function(object, day.one, Kc = 1, as.sf = TRUE, ...){
  
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
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  result <- .eto(day, night, Kc, p)
  
  if (isTRUE(as.sf)) {
    result <- suppressWarnings(sf::st_bind_cols(object, result))
  }
  
  return(result)
  
}

# Compute evapotranspiration
.eto <- function(day, night, Kc, p) {
  
  temp <- cbind(day, value2 = night$value)
  
  temp <- split(temp, temp$id)
  
  Tmean <-lapply(temp, function(x){
    (mean(x$value, na.rm = TRUE) + mean(x$value2, na.rm = TRUE)) / 2
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
