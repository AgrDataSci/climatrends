#' Evapotranspiration
#' 
#' Evapotranspiration using the Blaney-Criddle method. This is general  
#' theoretical method used when no measured data on pan evaporation 
#' is available locally. 
#' 
#' @inheritParams temperature
#' @param lat a vector for the latitude (in Decimal degrees), used to compute 
#'  mean daily percentage of annual daytime hours based on the latitude and month.
#'  This is extracted automatically in the \code{sf} method. See details
#' @param Kc a numeric value for the crop factor for water requirement
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
#' \code{last.day}: optional to \var{span}, an object of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD)  for the last day of the time series
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
#' # random dates around 2018-05-15 and 2018-05-20
#' set.seed(826128)
#' dates <- as.integer(runif(2, 17666, 17670))
#' 
#' # the evapotranspiration in the first 50 days after day.one
#' ETo(lonlat,
#'     day.one = dates,
#'     span = 50,
#'     lat = lonlat["lat"])
#'     
#' #######################################
#'  
#' # Objects of class 'sf'
#' data("lonlatsf", package = "climatrends")
#' 
#' ETo(lonlatsf,
#'     day.one = "2015-09-22",
#'     last.day = "2015-10-15",
#'     pars = c("T10M_MAX", "T10M_MIN"))
#' 
#' }
#' 
#' @importFrom sf st_bind_cols
#' @export
ETo <- function(object, day.one, span = NULL, Kc = 1, ...){
  
  UseMethod("ETo")

}

#' @rdname ETo
#' @method ETo default
#' @export
ETo.default <- function(object, day.one, span = NULL, Kc = 1, lat = NULL, ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  # coerce inputs to data.frame
  object <- as.data.frame(object)
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In ETo.default(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.vector(t(day.one))
  
  # check if day.one is a 'Date' else try to coerce to Date
  if (!.is_Date(day.one)) {
    
    day.one <- .coerce2Date(day.one)
    
  }
  
  # get p if lat is provided
  if (!is.null(lat)) {
    m <- as.integer(format(day.one, "%m"))
    p <- .p_daytime(lat = lat, month = m)
  } 
  
  if (is.null(lat)) {
    p <- 0.27
  }
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, span, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  result <- .eto(day, night, Kc, p)
  
  return(result)
}


#' @rdname ETo
#' @method ETo array
#' @export
ETo.array <- function(object, day.one, span = NULL, Kc = 1, lat = NULL, ...){
  
  if(dim(object)[[2]] == 2) {
    UseMethod("ETo", "default")
  }
  
  day.one <- as.vector(t(day.one))
  
  # check if day.one is a 'Date' else try to coerce to Date
  if (!.is_Date(day.one)) {
    
    day.one <- .coerce2Date(day.one)
    
  }

  # get p if lat is provided
  if (!is.null(lat)) {
    m <- as.integer(format(day.one, "%m"))
    p <- .p_daytime(lat = lat, month = m)
  } 
  
  if (is.null(lat)) {
    p <- 0.27
  }
  
  day <- get_timeseries(object[, , 1], day.one, span)[[1]]
  
  night <- get_timeseries(object[, , 2], day.one, span)[[1]]
  
  result <- .eto(day, night, Kc, p)
  
  return(result)
  
}


#' @rdname ETo
#' @method ETo sf
#' @export
ETo.sf <- function(object, day.one, span = NULL, Kc = 1, as.sf = TRUE, ...){
  
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
  
  dat <- get_timeseries(object, day.one, span, pars = pars, ...)
  
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
