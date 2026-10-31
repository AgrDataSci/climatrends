#' Time series climate data
#' 
#' General functions and methods to concatenate climate data across a time series
#' 
#' @param object a \code{data.frame} (or any other object that can be coerced to 
#'  data.frame) with geographical coordinates (lonlat), or an object of class 
#'  \code{sf} with geometry 'POINT' or 'POLYGON', or a named \code{matrix} with 
#'  climate data, or an array with two dimensions for max and min temperature.
#'  See details.   
#' @param day.one a vector of class \code{Date} or any other object that can be 
#'  coerced to \code{Date} (e.g. integer, character YYYY-MM-DD) for the starting 
#'  day to capture the climate data
#' @param span an integer or a vector with integers (optional if \var{last.day} is 
#'  given) for the length of the time series to be captured
#' @param last.day optional to \var{span}, an object of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD)  for the last day of the time series
#' @param data.from character, for the source of climate data. See details.
#' @param as.matrix logical, optional, to return a matrix or array instead of a data.frame 
#' @param ... additional arguments passed to methods. See details.
#' @details 
#' The \code{default} method and the \code{sf} method assumes that the climate 
#'  data will be fetched from an remote (cloud) \var{data.from}.
#'
#' The \code{matrix} method assumes that the climate data was previously handled 
#'  and will be inputted in the format of a named matrix. 
#'  See help("modis", "climatrends") for examples.
#' 
#' Available remote sources to pass \var{data.from}: "nasapower"
#' 
#' Additional arguments:
#' 
#' \code{pars}: character vector of solar, meteorological or climatology parameters 
#' to download. See help("parameters", "nasapower") when \var{data.from} = "nasapower".
#' 
#' \code{days.before}: an integer for the number of days before \var{day.one} to be 
#'  included in the timespan.
#' 
#' @return A list with class \code{clima_ls} with data.frame(s) with 
#'  the class \code{clima_df} 
#' @family GET functions
#' @examplesIf interactive()
#' # Using local sources
#' # an array with temperature data
#' data("temp_dat", package = "climatrends")
#' 
#' set.seed(9271)
#' span <- as.integer(runif(10, 6, 15))
#' 
#' get_timeseries(temp_dat, "2013-10-28", span = span)
#' 
#' # matrix with precipitation data
#' data("rain_dat", package = "climatrends")
#' 
#' get_timeseries(rain_dat, "2013-10-28", span = span)
#' 
#' \donttest{
#' # data can be returned as matrix
#' library("sf")
#' # Fetch data from NASA POWER using 'sf' method
#' data("lonlatsf", package = "climatrends")
#' 
#' g <- get_timeseries(object = lonlatsf,
#'                     day.one = "2018-05-16",
#'                     last.day = "2018-05-30",
#'                     pars = c("PRECTOTCORR", "T2M"),
#'                     as.matrix = TRUE)
#' }
#' @importFrom stats dist hclust cutree
#' @export
get_timeseries <- function(object, day.one, ...) {
  
  UseMethod("get_timeseries")
  
}

#' @rdname get_timeseries
#' @export
get_timeseries.default <- function(object, day.one, 
                                   span = NULL, 
                                   last.day = NULL, 
                                   as.matrix = FALSE,
                                   data.from = "nasapower", ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  days.before <- dots[["days.before"]]
  if (is.null(days.before)) {
    days.before <- 0
  }
  
  if (isTRUE("sf" %in% class(object))) {
    
    object <- .lonlat_from_sf(object)
    
  }
  
  object <- as.data.frame(object)
  
  sts <- .st_span(day.one, span, last.day, days.before)
  
  makecall <- paste0(".", data.from)
  
  args <- list(dates = sts$dates,
               lonlat = object,
               pars = pars)
  
  object <- do.call(makecall, args)
  
  if (isTRUE(as.matrix)) {
    
    object <- lapply(object, as.matrix)
    
    return(object)
    
  }
  
  r <- lapply(object, function(x){
    .st_ts(x,
           days = sts$begin,
           span = sts$span,
           maxspan = sts$maxspan)
  })
  
  class(r) <- union("clima_ls", class(r))
  
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
  
  sts <- .st_span(day.one, span, last.day, days.before)
  
  object <- as.data.frame(object)
  
  r <- .st_ts(object = object,
              days = sts$begin,
              span = sts$span,
              maxspan = sts$maxspan)
  
  r <- list(r)
  
  class(r) <- union("clima_ls", class(r))
  
  return(r)
}


#' @rdname get_timeseries
#' @method get_timeseries array
#' @export
get_timeseries.array <- function(object, day.one, span = NULL, last.day = NULL, 
                                 ...){
  
  
  dm1 <- get_timeseries(object[,,1], day.one, span = span, last.day = last.day, ...)
  
  dm2 <- get_timeseries(object[,,2], day.one, span = span, last.day = last.day, ...)
  
  r <- c(dm1, dm2)
  
  class(r) <- union("clima_ls", class(r))
  
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
.st_span <- function(day.one, 
                     span = NULL, 
                     last.day = NULL, 
                     days.before = 0){
  
  day.one <- as.vector(t(day.one))
  
  if (all(is.null(span), is.null(last.day))) {
    stop("No visible time span,", 
         " either argument 'span' or 'last.day' should be provided \n")
  }
  
  if (all(!is.null(span), !is.null(last.day))) {
    stop("No visible bound for confliting arguments,", 
         " please provide either 'span' or 'last.day'\n")
  }
  
  # check if day.one is a 'Date' else try to coerce to Date
  if (!.is_Date(day.one)) {
    
    day.one <- .coerce2Date(day.one)
    
  }
  
  # the time span
  if (!is.null(span)) {
    
    span <- as.vector(t(span)) 
    
  }
  
  # or from last.day
  if (!is.null(last.day)) {
    
    if (length(last.day) > 1) {
      
      warning("argument 'last.day' has length > 1",
              " and only the first element will be used")
      
    }
    
    if (length(day.one) > 1) {
      
      warning("argument 'day.one' has length > 1",
              " and only the first element will be used")
      
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
  maxend <- max(e)
  
  # the first and last date to fetch
  dates <- c(min(b), maxend)
  
  list(dates = dates,
       begin = b, 
       end = e,
       maxend = maxend,
       day.one = day.one,
       span = span,
       maxspan = maxspan)
}


#' Timeseries
#' 
#' @examples
#' sts <- .st_span(day.one = "2013-10-27",
#'                 last.day = "2013-11-10")
#' 
#' object <- as.data.frame(chirp)
#' 
#' .st_ts(object,
#'        days = sts$begin,
#'        span = sts$span,
#'        maxspan = sts$maxspan)
#' @noRd
.st_ts <- function(object, days, span, maxspan){
  
  n <- dim(object)[[1]]
  ids <- seq_len(n)
  rownames(object) <- ids
  date <- names(object)
  object[is.na(object)] <- -9999
  
  # do this to preserve the initial idea that the spans and first day 
  # can be variable (different seasons or weeks) as observed in the 
  # citizen science data. It should work fine in both citizen science 
  # and timeseries analysis
  if (length(span) < n) {
    span <- rep(span, length.out = n)
  }
  
  if (length(days) < n) {
    days <- rep(days, length.out = n)
  }
  
  # find the col index in object for the specified dates within 
  # the days provided
  date_i <- match(as.character(days), date)
  # and the index for the last day 
  date_f <- date_i + (span - 1)
  
  Y <- cbind(date_i, date_f, ids, object)
  # and them make the vectors for the timeseries in each point within object
  # this returns a list of data.frames that later are combined
  Y <- apply(Y, 1, function(x) {
    
    i <- t(x[1])
    
    f <- t(x[2])
    
    id <- t(x[3])
    
    x <- x[-c(1:3)]
    
    x <- x[c(i:f)]
    
    d <- names(x)
    
    x <- as.vector(t(x))
    
    id <- rep(id, length(x))
    
    data.frame(id = id, date = d, value = x, 
               stringsAsFactors = FALSE)
    
  })
  
  # put all together in a single data.frame
  Y <- do.call("rbind", Y)
  
  rownames(Y) <- 1:nrow(Y)
  
  Y[Y == -9999] <- NA
  
  Y$date <- .coerce2Date(Y$date)
  
  Y$id <- as.integer(Y$id)
  
  class(Y) <- union("clima_df", class(Y))
  
  return(Y)
  
}

#' Get data from NASAPOWER using nasapower::get_power()
#' 
#' Regional requests to NASA POWER accept one parameter at a time and a
#' limited time window, so the data are requested by region, by parameter
#' and in chunks of at most 365 days, then put back in the order of
#' \code{lonlat}
#' 
#' @param dates first and final date, Date or character YYYY-MM-DD
#' @param lonlat data.frame with longitude and latitude, in that order
#' @param pars character vector of solar, meteorological or climatology parameters 
#' to download. See help("parameters", "nasapower") for details.
#' @return a list with one data.frame per parameter, with one row per point
#'  in \code{lonlat} and one column per day, named YYYY-MM-DD
#' @examples 
#' lonlat = data.frame(lon = c(-66.48), lat = c(-4.60))
#' 
#' .nasapower(dates = c("2010-01-01", "2010-01-30"),
#'            lonlat = lonlat,
#'            pars = c("T2M_MAX","T2M_MIN"))
#' @importFrom nasapower get_power
#' @noRd
.nasapower = function(dates, 
                      lonlat, 
                      pars, 
                      community = NULL, 
                      temporal_average = NULL){
  
  message("Getting climate data from NASA POWER \n")
  
  if (is.null(community)) {
    community = "ag"
  }
  
  if (is.null(temporal_average)) {
    temporal_average = "daily"
  }
  
  if (is.null(pars) || length(pars) == 0) {
    stop("argument 'pars' is required to fetch data from NASA POWER")
  }
  
  lonlat = as.data.frame(lonlat)
  
  nr = dim(lonlat)[[1]]
  
  # group points in regions of about 3 arc-degrees
  if (isTRUE(nr > 1)) {
    h = stats::dist(lonlat)
    h = stats::hclust(h)
    regions = stats::cutree(h, h = 3)
  } else {
    regions = 1L
  }
  
  nregions = max(regions)
  
  if (isTRUE(nregions > 1)) {
    message("Fetching data for ", nregions, " regions with 5 x 5 arc-degree \n")
  }
  
  dates = as.Date(dates)
  alldays = seq(dates[1], dates[2], by = "day")
  daynames = as.character(alldays)
  nd = length(alldays)
  
  # chunks of at most 365 days
  chunk_start = seq(dates[1], dates[2], by = 365)
  chunk_end = pmin(chunk_start + 364, dates[2])
  
  result = lapply(pars, function(p) {
    matrix(NA_real_, nrow = nr, ncol = nd, dimnames = list(seq_len(nr), daynames))
  })
  names(result) = pars
  
  for (i in seq_len(nregions)) {
    
    r_i = which(regions == i)
    lonlat_i = lonlat[r_i, , drop = FALSE]
    
    # geographic boundaries for the region
    lims = c(floor(min(lonlat_i[, 1])),
             floor(min(lonlat_i[, 2])),
             ceiling(max(lonlat_i[, 1])) + 2,
             ceiling(max(lonlat_i[, 2])) + 2)
    
    for (p in pars) {
      
      info = lapply(seq_along(chunk_start), function(k) {
        args = list(community = community,
                    lonlat = lims,
                    pars = p,
                    dates = as.character(c(chunk_start[k], chunk_end[k])),
                    temporal_api = temporal_average)
        as.data.frame(do.call(nasapower::get_power, args))
      })
      
      info = do.call("rbind", info)
      
      if (is.null(info[[p]])) {
        stop("NASA POWER did not return the parameter '", p, "'")
      }
      
      day = info$YYYYMMDD
      if (!.is_Date(day)) {
        day = as.Date(as.character(day), format = "%Y%m%d")
      }
      
      value = as.numeric(info[[p]])
      # NASA POWER uses -999 for missing values
      value[value <= -999] = NA
      
      # grid cells returned and the nearest cell for each point
      cell = paste(info$LON, info$LAT)
      cells = unique(info[, c("LON", "LAT")])
      cellkey = paste(cells$LON, cells$LAT)
      
      nn = apply(lonlat_i, 1, function(xy) .nearest(xy1 = as.numeric(xy), xy2 = cells))
      
      m = matrix(NA_real_, nrow = nrow(cells), ncol = nd)
      m[cbind(match(cell, cellkey), match(as.character(day), daynames))] = value
      
      result[[p]][r_i, ] = m[nn, , drop = FALSE]
      
    }
    
  }
  
  result = lapply(result, as.data.frame)
  
  return(result)
  
}

