#' Time series climate data
#' 
#' Concatenate time series climate data
#' 
#' @param object a data.frame (or object that can be coerced to data.frame) with
#'  geographical coordinates (lonlat), or an object of class \code{sf} with 
#'  geographical coordinates (lonlat), or a named \code{matrix} with climate 
#'  data. See details.   
#' @param day.one a vector of class \code{Date} for the starting date to capture 
#'  the climate data
#' @param span an integer or a vector with integers for the duration of the 
#'  time series to be captured
#' @param as.sf logical, returns an object of class \code{[sf]{sf}}
#' @param source character, for the source of climate data. See details.
#' @param ... additional arguments passed to methods. See details.
#' @details 
#' The default method and the sf method assumes that the climate data will be retrieved 
#'  from an external \code{source}.
#'
#' The matrix method assumes that the climate data was previously handled and will be 
#'  inputted in the format of a named matrix. See help(modis) for examples.
#' 
#' Current remote \code{source} is: 'nasapower'
#' 
#' Additional arguments:
#' 
#' \code{pars}: character vector of solar, meteorological or climatology parameters 
#' to download. See help("parameters", "nasapower").
#' 
#' \code{days.before}: optional, an integer for the number of days before 
#'  \code{day.one} to be included in the timespan.
#' 
#' @return An object with time series climate data for the chosen period
#' @family GET functions
#' @examples 
#' # Using local sources
#' data("modis", package = "climatrends")
#' 
#' day <- as.Date("2013-10-28", format = "%Y-%m-%d")
#' 
#' get_timeseries(chirp, day, span = c(10,11))
#' 
#' 
#' ########################################################
#' \donttest{
#' # Fetch data from NASA POWER using 'sf' method
#' library("sf")
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(2, 11, 12),
#'                      lat = runif(2, 55, 58))
#' 
#' lonlat <- st_as_sf(lonlat, coords = c("lon","lat"), crs = 4326)
#' 
#' do <- as.Date(17667, origin = "1970-01-01")
#' 
#' g <- get_timeseries(lonlat, day.one = do, span = 10, pars = "PRECTOT")
#' 
#' g
#' 
#' g <- get_timeseries(lonlat, day.one = do, span = c(10, 13), 
#'                     pars = "T10M", as.sf = TRUE)
#' 
#' g
#' }
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom nasapower get_power
#' @importFrom sf st_centroid st_geometry_type st_as_sf
#' @importFrom tibble as_tibble
#' @export
get_timeseries <- function(object, day.one = NULL, span = 150, ...) {
  
  UseMethod("get_timeseries")
  
}

#' @rdname get_timeseries
#' @export
get_timeseries.default <- function(object, 
                                   day.one = NULL,
                                   span = 150,  
                                   source = "nasapower",
                                   ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  days.before <- dots[["days.before"]]
  if (is.null(days.before)) {
    days.before <- 0
  }
  
  sts <- .set_span_length(day.one, span, days.before)

  object <- as.data.frame(object)
  
  makecall <- paste0(".", source)
  
  args <- list(dates = sts$dates,
               lonlat = object,
               pars = pars)
  
  
  object <- do.call(makecall, args)

  
  r <- .setup_timeseries(object,
                         days = sts$b,
                         span = sts$span,
                         maxspan = sts$maxspan)
  
  return(r)
  
}

#' @rdname get_timeseries
#' @method get_timeseries sf
#' @export
get_timeseries.sf <- function(object, 
                              day.one = NULL,
                              span = 150,  
                              source = "nasapower", 
                              as.sf = FALSE, 
                              ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  days.before <- dots[["days.before"]]
  if (is.null(days.before)) {
    days.before <- 0
  }
  
  # check geometry type
  type <- c("POINT", "POLYGON")
  
  # check for supported types 
  supp_type <- c(all(grepl(type[[1]], sf::st_geometry_type(object))),
                 all(grepl(type[[2]], sf::st_geometry_type(object))))
  
  if (!any(supp_type)) {
    stop("The sf geometry type is not supported. ",
         "Please provide a sf object of geometry type ",
         "'POINT' or 'POLYGON'\n")
  }
  
  type <- type[which(supp_type)]
  
  nr <- dim(object)[[1]]
  
  # find the sf_column
  index <- attr(object, "sf_column")
  
  # get the sf column
  lonlat <- object[[index]]
  
  if (type == "POINT") {
    
    # unlist the sf_column
    lonlat <- unlist(object[[index]])
    
  }
  
  if (type == "POLYGON") {
    
    # set centroid to validade lonlat
    lonlat <- sf::st_centroid(lonlat)
    
    # unlist the sf_column
    lonlat <- unlist(lonlat)
    
  }
  
  lonlat <- matrix(lonlat,
                   nrow = nr,
                   ncol = 2, 
                   byrow = TRUE, 
                   dimnames = list(seq_len(nr), c("lon","lat")))
  
  object <- as.data.frame(lonlat)
  
  ll <- object
  
  sts <- .set_span_length(day.one, span, days.before)
  
  makecall <- paste0(".", source)
  
  args <- list(dates = sts$dates,
               lonlat = object,
               pars = pars)
  
  
  object <- do.call(makecall, args)
  
  
  r <- .setup_timeseries(object = object,
                         days = sts$b,
                         span = sts$span,
                         maxspan = sts$maxspan)
  
  if (as.sf) {
    
    r <- cbind(ll, r)
    
    r <- sf::st_as_sf(r, coords = c("lon","lat"), crs = 4326)
    
  } 
  
  return(r)
  
}


#' @rdname get_timeseries
#' @method get_timeseries matrix
#' @export
get_timeseries.matrix <- function(object, 
                                  day.one = NULL,
                                  span = 150, 
                                  ...){
  
  dots <- list(...)
  days.before <- dots[["days.before"]]
  if (is.null(days.before)) {
    days.before <- 0
  }
  
  sts <- .set_span_length(day.one, span, days.before)
  
  object <- as.data.frame(object)
  
  r <- .setup_timeseries(object = object,
                         days = sts$b,
                         span = sts$span,
                         maxspan = sts$maxspan)
  
  return(r)
}

#' Get data from NASAPOWER using nasapower::get_power()
#' 
#' @param dates character with first and final date in the format YYYY-MM-DD
#' @param lonlat data.frame with longitude and latitude, in that order
#' @param pars character vector of solar, meteorological or climatology parameters 
#' to download. See nasapower::get_power() for details.
#' @param ... addtional arguments passed to nasapower::get_power()
#' @examples 
#' set.seed(123)
#' ll <- data.frame(lon = runif(2, 11, 12),
#'                  lat = runif(2, 55, 58))
#' 
#' .nasapower(dates = c("2010-01-01", "2010-01-30"),
#'            lonlat = ll,
#'            pars = "PRECTOT")
#' @noRd
.nasapower <- function(dates, lonlat, pars, ...){
  
  # define geographic boundaries for lonlat
  lims <- with(lonlat, c(floor(min(lonlat[,1])), 
                         floor(min(lonlat[,2])),
                         ceiling(max(lonlat[,1])), 
                         ceiling(max(lonlat[,2]))))
  
  # get NASA POWER
  info <- nasapower::get_power(community = "AG",
                               lonlat = lims,
                               dates = dates,
                               temporal_average = "DAILY", 
                               pars = pars)
  
  class(info) <- class(info)[-1]
  
  # rename target fetched product
  nc <- dim(info)[[2]]
  names(info)[nc] <- "value"
  
  # split by YYYYMMDD to create a list of data frames
  info <- split(info, info$YYYYMMDD)
  
  # keep only coordinates and the variable fetched
  info <- lapply(info, function(x) {
    x[(!names(x) %in% c("YEAR", "MM", "DD", "DOY"))]
  })
  
  # put this information in its right lonlat as provided in the input
  xy2 <- info[[1]][,c("LON","LAT")]
  xy2 <- as.data.frame(xy2)
  
  n <- dim(lonlat)[[1]]
  
  # split lonlat into a list by its rows
  xy1 <- split(lonlat, seq_len(n))
  
  # get the index for lonlat in info
  nn <- lapply(xy1, function(n) {
    n <- as.vector(t(n))
    .nearest(xy1 = n, xy2 = xy2)
  })
  
  # unlist to get the vector
  nn <- unlist(nn)
  
  # force the vector to be in the right order, from 1 to n 
  nn <- nn[ sort(as.numeric(names(nn))) ]
  
  # retrieve the data from info using nn
  dat <- lapply(info, function(n) {
    n <- n[nn, "value"]
    n
  })
  
  # combine vectors in this list 
  dat <- do.call("cbind", dat)
  
  object <- as.data.frame(dat)
  
  names(object) <- as.character(names(info))
  
  return(object)
    
}

#' Set up span length and organise dates
#' 
#' @param day.one the first day
#' @param span the span
#' @param days.before the number of days before day.one
#' @examples 
#' .set_span_length(day.one = as.Date(c(17667,17789, 17665), origin = "1970-01-01"),
#'                  span = c(10, 15, 12))
#'
#' @noRd
.set_span_length <- function(day.one, span, days.before = 0){
  
  if (.is_tibble(day.one)) {
    day.one <- day.one[[1]]
  }

  # the timespan
  span <- as.vector(t(span))
  
  # the begin date
  b <- day.one - days.before
  
  # the end date
  e <- day.one + span
  
  # the refreshed timespan
  span <- as.integer(e - b)
  
  # the maximum timespan
  maxspan <- max(span)
  
  # the maximum end date
  maxend <- max(b) + max(span)
  
  # the first and last date to fetch
  dates <- c(min(b), maxend)
  
  list(maxend = maxend,
       dates = dates,
       b = b, 
       e = e,
       maxspan = maxspan,
       day.one = day.one,
       span = span)
  
}


#' Timeseries
#' 
#' @examples
#' set.seed(123)
#' ll <- data.frame(lon = runif(2, 11, 12),
#'                  lat = runif(2, 55, 58))
#' 
#' sts <- .set_span_length(day.one = as.Date(c(17667,17789), origin = "1970-01-01"),
#'                         span = c(10, 15))
#' 
#' object <- .nasapower(dates = sts$dates,
#'                      lonlat = ll,
#'                      pars = "PRECTOT")
#' 
#' .setup_timeseries(object,
#'                   days = sts$b,
#'                   span = sts$span,
#'                   maxspan = sts$maxspan)
#' 
#' @noRd
.setup_timeseries <- function(object, days, span, maxspan){
  
  n <- dim(object)[[1]]
  rownames(object) <- seq_len(n)
  date <- names(object)
  
  # find the index for specified dates within the start.dates provided
  date <- match(as.character(days), date)
  
  Y <- NULL
  
  for (i in 0:maxspan) {
    Y <- cbind(Y, object[cbind(seq_len(n), date + i)])
  }
  
  # if ts is variable then add NA's
  Y <- t(apply(cbind(span, Y), 1, function(x) {
    x1 <- x[1]
    
    x <- x[2:length(x)]
    
    x[(x1 + 1):length(x)] <- NA
    
    return(x)
    
  }))
  
  # make a tibble
  if (n != 1) {
    
    dimnames(Y)[[2]] <- paste0("day", 1:ncol(Y))
    
    Y <- tibble::as_tibble(Y[, seq_len(maxspan)])
    
  } else {
    
    Y <- tibble::as_tibble(t(Y[, seq_len(maxspan)]))
    
    names(Y) <- paste0("day", seq_len(maxspan))
  }
  
  return(Y)
}
