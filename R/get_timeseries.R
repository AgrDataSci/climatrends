#' Time series climate data
#' 
#' General functions and methods to concatenate climate data across a time series.
#' 
#' @param object a \code{data.frame} (or any othwer object that can be coerced to 
#'  data.frame) with geographical coordinates (lonlat), or an object of class 
#'  \code{sf} with geometry 'POINT' or 'POLYGON', or a named \code{matrix} with 
#'  climate data. See details.   
#' @param day.one a vector of class \code{Date} or any other object that can be 
#'  coerced to \code{Date} (e.g. integer, character YYYY-MM-DD) for the starting 
#'  day to capture the climate data
#' @param span an integer or a vector with integers (optional if \var{last.day} is 
#'  given) for the length of the time series to be captured
#' @param last.day optional to \var{span}, an object of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD)  for the last day of the time series
#' @param source character, for the source of climate data. See details.
#' @param ... additional arguments passed to methods. See details.
#' @details 
#' The \code{default} method and the \code{sf} method assumes that the climate 
#'  data will be fetched from an remote (cloud) \var{source}.
#'
#' The \code{matrix} method assumes that the climate data was previously handled 
#'  and will be inputted in the format of a named matrix. 
#'  See help("modis", "climatrends") for examples.
#' 
#' Available remote \var{source}s are: "nasapower"
#' 
#' Additional arguments:
#' 
#' \code{pars}: character vector of solar, meteorological or climatology parameters 
#' to download. See help("parameters", "nasapower") when \var{source} = "nasapower".
#' 
#' \code{days.before}: an integer for the number of days before \var{day.one} to be 
#'  included in the timespan.
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
#' 
#' data("lonlatsf", package = "climatrends")
#' 
#' do <- as.Date(17667, origin = "1970-01-01")
#' 
#' g <- get_timeseries(lonlatsf, 
#'                     day.one = do, 
#'                     span = 10,
#'                     pars = c("PRECTOT", "T2M", "T10M"))
#' 
#' g
#' }
#' @importFrom nasapower get_power
#' @importFrom sf st_centroid st_geometry_type st_as_sf
#' @importFrom stats dist hclust cutree
#' @export
get_timeseries <- function(object, day.one, span = NULL, last.day = NULL, ...) {
  
  UseMethod("get_timeseries")
  
}

#' @rdname get_timeseries
#' @export
get_timeseries.default <- function(object, day.one, span = NULL, last.day = NULL,
                                   source = "nasapower", ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  days.before <- dots[["days.before"]]
  if (is.null(days.before)) {
    days.before <- 0
  }
  
  sts <- .set_span_length(day.one, span, last.day, days.before)

  object <- as.data.frame(object)
  
  makecall <- paste0(".", source)
  
  args <- list(dates = sts$dates,
               lonlat = object,
               pars = pars)
  
  
  object <- do.call(makecall, args)

  r <- lapply(object, function(x){
    .setup_timeseries(x,
                      days = sts$b,
                      span = sts$span,
                      maxspan = sts$maxspan)
  })
  
  return(r)
  
}

#' @rdname get_timeseries
#' @method get_timeseries sf
#' @export
get_timeseries.sf <- function(object, day.one, span = NULL, last.day = NULL, 
                              source = "nasapower",
                              ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  days.before <- dots[["days.before"]]
  if (is.null(days.before)) {
    days.before <- 0
  }
  
  object <- .lonlat_from_sf(object)
  
  object <- as.data.frame(object)
  
  sts <- .set_span_length(day.one, span, last.day, days.before)
  
  makecall <- paste0(".", source)
  
  args <- list(dates = sts$dates,
               lonlat = object,
               pars = pars)
  
  
  object <- do.call(makecall, args)
  
  
  r <- lapply(object, function(x){
    .setup_timeseries(x,
                      days = sts$b,
                      span = sts$span,
                      maxspan = sts$maxspan)
  })
  
  return(r)
  
}


#' @rdname get_timeseries
#' @method get_timeseries matrix
#' @export
get_timeseries.matrix <- function(object, day.one, span = NULL, last.day = NULL, 
                                  ...){
  
  dots <- list(...)
  days.before <- dots[["days.before"]]
  if (is.null(days.before)) {
    days.before <- 0
  }
  
  dmo <- dim(object)[[2]]
  
  if (!is.null(span)) {
    mspan <- max(span)
  }else{
    mspan <- dmo
  }
  
  if (all(!is.null(span), (mspan > dmo))) {
    stop("subscript out of bounds,",
         "'span' is larger than the dim[2] of provided 'object' \n")
  }
  
  sts <- .set_span_length(day.one, span, last.day, days.before)
  
  object <- as.data.frame(object)
  
  r <- .setup_timeseries(object = object,
                         days = sts$b,
                         span = sts$span,
                         maxspan = sts$maxspan)
  
  r <- list(r)
  
  return(r)
}

#' Set up span length and organise dates
#' 
#' @param day.one the first day
#' @param span the span
#' @param days.before the number of days before day.one
#' @examples 
#' .set_span_length(day.one = "2013-10-27",
#'                  span = 15)
#'
#' @noRd
.set_span_length <- function(day.one, 
                             span = NULL, 
                             last.day = NULL, 
                             days.before = 0){
  
  day.one <- as.vector(t(day.one))
  
  if (all(is.null(span), is.null(last.day))) {
    stop("No visible timespan observed, 
         either argument 'span' or 'last.day' should be provided \n")
  }
  
  if (all(!is.null(span), !is.null(last.day))) {
    stop("No visible bound for confliting arguments, 
         please provide either 'span' or 'last.day'\n")
  }
  
  # check if day.one is a 'Date' else try to coerce to Date
  if (!.is_Date(day.one)) {
    
    day.one <- .coerce2Date(day.one)
  
  }
  
  # the timespan
  if (!is.null(span)) {
    
    span <- as.vector(t(span)) 
  
  }
  
  # or from last.day
  if (!is.null(last.day)) {
    
    if (length(last.day) > 1) {
      
      warning("argument 'last.day' has length > 1 and only the first element will be used")
      
    }
    
    if (length(day.one) > 1) {
      
      warning("argument 'day.one' has length > 1 and only the first element will be used")
      
    }
    
    if (!.is_Date(last.day)) {
      
      last.day <- .coerce2Date(last.day)
      
    }
    
    span <- as.integer(last.day[[1]] - day.one[[1]]) + 1
    
  }
  
  # the begin date
  b <- day.one - days.before
  
  # the end date
  e <- day.one + (span - 1)
  
  # the refreshed timespan
  span <- as.integer(e - b) + 1
  
  # the maximum timespan
  maxspan <- max(span)
  
  # the maximum end date
  maxend <- max(b) + max(span - 1)
  
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
#'  
#' sts <- climatrends:::.set_span_length(day.one = "2013-10-27",
#'                                       last.day = "2013-11-10")
#'  
#' object <- as.data.frame(chirp)
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
  
  # find the index for the specified dates within the start.dates provided
  date <- match(as.character(days), date)
  
  Y <- NULL
  
  for (i in 0:(maxspan-1)) {
    Y <- cbind(Y, object[cbind(seq_len(n), date + i)])
  }
  
  # if ts is variable then add NA's
  Y <- t(apply(cbind(span, Y), 1, function(x) {
    
    s <- x[1]
    
    x <- x[2:length(x)]
    
    if (length(x) > s) {
      x[(s + 1):length(x)] <- NA
    }
    
    return(x)
    
  }))
  
  # make a data.frame
  if (n != 1) {
    
    dimnames(Y)[[2]] <- paste0("day", 1:ncol(Y))
    
    Y <- as.data.frame(Y[, seq_len(maxspan)])
    
  } else {
    
    Y <- as.data.frame(t(Y[, seq_len(maxspan)]))
    
    names(Y) <- paste0("day", seq_len(maxspan))
  }
  
  return(Y)
}


#' Get data from NASAPOWER using nasapower::get_power()
#' 
#' @param dates character with first and final date in the format YYYY-MM-DD
#' @param lonlat data.frame with longitude and latitude, in that order
#' @param pars character vector of solar, meteorological or climatology parameters 
#' to download. See help("parameters", "nasapower") for details.
#' @examples 
#'  
#' lonlat <- data.frame(lon = c(-66.48, -83.08, -66.45, -66.4),
#'                      lat = c(-4.60, 9.85, -5.19, -0.15))
#' 
#' climatrends:::.nasapower(dates = c("2010-01-01", "2010-01-30"),
#'                          lonlat = lonlat,
#'                          pars = c("T2M_MAX","T2M_MIN"))
#' 
#' 
#'                      
#' @noRd
.nasapower <- function(dates, lonlat, pars, community = NULL, temporal_average = NULL){
  
  message("Getting climate data from NASA POWER \n")
  
  if (is.null(community)) {
    community <- "AG"
  }
  
  if (is.null(temporal_average)) {
    temporal_average <- "DAILY"
  }
  
  nr <- dim(lonlat)[[1]]
  
  # check if data from multiple regions is required
  h <- stats::dist(lonlat)
  h <- stats::hclust(h)
  regions <- stats::cutree(h, h = 5)
  
  nregions <- max(regions)
  
  if (nregions > 1) {
    message("Fetching data for ", nregions, " regions with 5 x 5 arc-degree \n")
  }
  
  sp <- as.Date(dates, format = "%Y-%m-%d")
  sp <- length(sp[1]:sp[2])
  npars <- length(pars)
  
  dat <- matrix(NA,
                nrow = nr,
                ncol = (sp * npars),
                dimnames = list(1:nr))

  
  for (i in seq_len(nregions)) {
    
    r_i <- which(regions == i)
    lonlat_i <- lonlat[r_i, ]
    
    # define geographic boundaries for lonlat
    lims <- with(lonlat, c(floor(min(lonlat_i[,1])), 
                           floor(min(lonlat_i[,2])),
                           ceiling(max(lonlat_i[,1])), 
                           ceiling(max(lonlat_i[,2]))))
    
    # get NASA POWER
    info <- nasapower::get_power(community = community,
                                 lonlat = lims,
                                 dates = dates,
                                 temporal_average = temporal_average, 
                                 pars = pars)
    
    info <- as.data.frame(info)
    
    # split by YYYYMMDD to create a list of data frames
    info <- split(info, info$YYYYMMDD)
    
    # keep only coordinates and the variable fetched
    info <- lapply(info, function(x) {
      x[(!names(x) %in% c("YEAR", "MM", "DD", "DOY"))]
    })
    
    # put this information in its right lonlat as provided in the input
    xy2 <- info[[1]][,c("LON","LAT")]
    
    n <- dim(lonlat_i)[[1]]
    
    # split lonlat into a list by its rows
    xy1 <- split(lonlat_i, seq_len(n))
    
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
    d <- lapply(info, function(n) {
      n <- n[nn, pars]
      n
    })
    
    namedays <- names(d)
    
    # combine vectors in this list
    d <- do.call("cbind", d)
    d <- as.matrix(d)
    
    namesdat <- dimnames(d)[[2]]
    
    dat[r_i, ] <- d
    
  }
  
  dimnames(dat)[[2]] <- namesdat
  
  dat <- as.data.frame(dat)
  
  result <- list()
  if (length(pars) > 1) {
    for(i in seq_along(pars)){
      index <- grepl(pars[[i]], names(dat))
      rs <- dat[, index]
      rs <- as.data.frame(rs)
      names(rs) <- namedays
      result[[i]] <- rs
    }
  }
 
  if (length(pars) == 1) {
    dat <- as.data.frame(dat)
    names(dat) <- namedays
    result[[1]] <- dat
  }
  
  names(result) <- pars
  
  return(result)
  
}

