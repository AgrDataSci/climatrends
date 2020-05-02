#' Growing degree-days
#' 
#' Compute number of days required to reach Growing degree-days.
#' GDD is a heuristic tool in phenology that measures heat 
#' accumulation and is used to predict plant and animal development 
#' rates. Growing degree-days are calculated by taking the 
#' integral of warmth above a base temperature.
#' 
#' @inheritParams temperature
#' @param degree.days an integer for the degree-days required by the 
#'  organism (look for the physiology of the focal organism)
#' @param base an integer for the base temperature
#' @return The number of days which were required to reach the growing degree-days.
#' @family temperature functions
#' @details 
#' The \code{array} method assumes that \var{object} contains climate data provided 
#'  from a local source; this requires an array with two dimensions, 1st dimension 
#'  contains the day temperature and 2nd dimension the night temperature, 
#'  see help("modis", "climatrends") for an example on input structure.
#' 
#' The default method and the sf method assumes that the climate data will be fetched 
#'  from an remote (cloud) \var{source}.
#'
#' Additional arguments:
#' 
#' \code{last.day}: optional to \var{span}, an object of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD)  for the last day of the time series
#' 
#' \code{source}: character for the source of remote data. Current remote \var{source} 
#'  is: 'nasapower'
#' 
#' \code{pars}: character vector for the temperature data to be fetched. If 
#'  \code{source} is 'nasapower'. The temperature can be adjusted to 2 m, the default,
#'  c("T2M_MAX", "T2M_MIN") or 10 m c("T10M_MAX", "T10M_MIN") 
#' 
#' @references 
#' Prentice I. C., et al. (1992) Journal of Biogeography, 19(2), 117. 
#' \cr\url{https://doi.org/10.2307/2845499}
#' 
#' @examples
#' # Using local sources
#' data("modis", package = "climatrends")
#' 
#' GDD(modis,
#'     day.one = "2013-10-28",
#'     degree.days = 100,
#'     base = 5)
#' 
#' ######################################
#' \donttest{
#' # Using remote sources
#' set.seed(123)
#' # random geographic locations around bbox(11, 12, 55, 58)
#' lonlat <- data.frame(lon = runif(3, 11, 12),
#'                      lat = runif(3, 55, 58))
#' 
#' set.seed(321)
#' # random dates around 2018-05-15 and 2018-05-20
#' dates <- as.integer(runif(3, 17666, 17670))
#' 
#' # Calculate the days required for the plants in these plots to reach the
#' # maturity. Here the plant species requires ~1300 degree days for it.
#' GDD(lonlat,
#'     day.one = dates,
#'     degree.days = 1300,
#'     span = 150,
#'     base = 5)
#' 
#' ######################################
#' 
#' # Objects of class 'sf'
#' data("lonlatsf", package = "climatrends")
#' 
#' dates <- as.Date(16150, origin = "1970-01-01")
#' 
#' GDD(lonlatsf,
#'     day.one = "2014-03-21",
#'     last.day = "2014-06-30",
#'     degree.days = 200,
#'     base = 5)
#' 
#'}
#' @export
GDD <- function(object, day.one, degree.days,
                base = 10, ...)
{
  UseMethod("GDD")
}

#' @rdname GDD
#' @method GDD default
#' @export
GDD.default <- function(object, day.one, degree.days,
                        base = 10, span = NULL, ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  # coerce inputs to data.frame
  object <- as.data.frame(object)
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In GDD.default(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, span, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  result <- .gdd(day, night, base, degree.days)
  
  return(result)
}


#' @rdname GDD
#' @method GDD array
#' @export
GDD.array <- function(object, day.one, degree.days,
                      base = 10, ...){
  
  if(dim(object)[[2]] == 2) {
    UseMethod("GDD", "default")
  }
  
  dots <- list(...)
  span <- dots[["span"]]
  last.day <- dots[["last.day"]]
  
  # coerce to vector
  day.one <- as.vector(t(day.one))
  
  if (all(is.null(span), is.null(last.day))) {
    
    if (isFALSE(.is_Date(day.one))){
      day.one <- .coerce2Date(day.one)
    }
    do <- as.character(max(day.one))
    do <- match(do, dimnames(object[,,1])[[2]])
    span <- dim(object)[[2]] - do
  }
  
  
  
  day <- get_timeseries(object[, , 1], day.one, span, last.day)[[1]]
  
  night <- get_timeseries(object[, , 2], day.one, span, last.day)[[1]]
  
  result <- .gdd(day, night, base, degree.days)
  
  return(result)
}


#' @rdname GDD
#' @method GDD sf
#' @export
GDD.sf <- function(object, day.one, degree.days,
                   base = 10, span = NULL, as.sf = TRUE, ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, span, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  result <- .gdd(day, night, base, degree.days)
  
  if (isTRUE(as.sf)) {
    result <- suppressWarnings(sf::st_bind_cols(object, result))
  }
  
  return(result)
}


.gdd <- function(day, night, base, degree.days){
  # get the difference between day and night temperature
  Y <- (((day + night) / 2) - base)
  
  # sum temperature values until reach the defined degree days
  Y <- apply(Y, 1, function(x){
    
    for (d in seq_along(x)) {
      
      i <- d
      
      if (sum(x[1:d]) > degree.days) break}
    
    return(i)
  })
  
  result <- data.frame(GDD = Y, stringsAsFactors = FALSE)
  
  class(result) <- union("clima_df", class(result))
  
  return(result)
  
}