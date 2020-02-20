#' Evapotranspiration
#' 
#' Compute evapotranspiration using the Blaney-Criddle method. A 
#' theoretical method used when no measured data on pan evaporation 
#' are available locally. 
#' 
#' @inheritParams temperature
#' @param lat a vector for the latitude (in Decimal degrees) 
#'  used to compute mean daily percentage of annual daytime hours based 
#'  on the latitude and month. See details
#' @param Kc a numeric value for the crop factor for water requirement
#' @param p optional, a numeric value (from 0 to 1) used if lat is not 
#' given, representing the mean daily percentage of annual daytime hours 
#' for different latitudes
#' @return The evapotranspiration in mm/day
#' @details 
#' When \var{lat} is used, it is combined with the month provided in 
#'  \var{day.one} to call for the system data \code{daylight} to find
#'  the correct value for \var{p} which represents the daily percentage
#'  of daytime hours in the given month and latitude.
#' @family climatology functions
#' @references
#' Brouwer C. & Heibloem M. (1986). Irrigation water management: 
#' Irrigation water needs. Food and Agriculture Organization of The 
#' United Nations, Rome, Italy. 
#' \url{http://www.fao.org/3/S2022E/s2022e00.htm}
#' 
#' @examples
#' # Using local sources
#' data("modis", package = "climatrends")
#' 
#' day <- as.Date("2013-10-28", format = "%Y-%m-%d")
#' 
#' ETo(modis, 
#'     day.one = day,
#'     span = 10,
#'     Kc = 0.92)
#'     
#' \donttest{
#' # Using remote sources 
#' library("nasapower")
#' 
#' # random geographic locations around bbox(11, 12, 55, 58)
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(2, 11, 12),
#'                      lat = runif(2, 55, 58))
#' 
#' # random dates around 2018-05-15 and 2018-05-20
#' set.seed(321)
#' dates <- as.integer(runif(2, 17666, 17670))
#' dates <- as.Date(dates, origin = "1970-01-01")
#' 
#' # the evapotranspiration in the first 50 days after day.one
#' ETo(lonlat,
#'     day.one = dates,
#'     span = 50,
#'     lat = lonlat[["lat"]])
#' }
#' 
#' @importFrom tibble tibble
#' @export
ETo <- function(object, day.one = NULL, span = 150, 
                lat = NULL, Kc = 1, p = NULL){
  
  # remove vector from a tibble object
  if (.is_tibble(day.one)) {
    day.one <- day.one[[1]]
  }
  
  # remove vector from a tibble object
  if (.is_tibble(lat)) {
    lat <- lat[[1]]
  }
  
  # get p if lat is provided
  if (!is.null(lat)) {
    l <- .round5(lat, 5)
    m <- as.integer(format(day.one, "%m"))
    p <- daylight[cbind(match(l , daylight[, "y"]), match(m , names(daylight)))]
  } 
  
  if (is.null(p)) {
    p <- 0.27
  }
  
  # get timespan for the day temperature
  if (dim(object)[2] == 2) {
    day <- .get_timeseries(object, day.one, span, pars = "T2M_MAX")
  } else {
    day <- .get_timeseries(object[, , 1], day.one, span)
  }
  
  # get timespan for the night temperature
  if (dim(object)[2] == 2) {
    night <- .get_timeseries(object, day.one, span, pars = "T2M_MIN")
  } else {
    night <- .get_timeseries(object[, , 2], day.one, span)
  }
  
  # calculate Tmean
  Tmean <- (rowMeans(day, na.rm = TRUE) +  rowMeans(night, na.rm = TRUE)) / 2
  
  # evapotranspiration
  eto <- p * (0.46 * Tmean + 8) * Kc
  
  result <- tibble::tibble(ETo = eto)
  
  return(result)
  
}


