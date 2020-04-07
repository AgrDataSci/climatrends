#' Temperature indices
#'
#' Methods to compute temperature indices over a time period
#'
#' @param object a data.frame (or object that can be coerced to data.frame) 
#'  with geographical coordinates (lonlat), or an object of class \code{sf}
#'  with geometry 'POINT' or 'POLYGON', or an \code{array} with two dimensions 
#'  containing the temperature data, see details. 
#' @inheritParams get_timeseries
#' @param timeseries logical, \code{FALSE} for a single point time series
#'  observation or \code{TRUE} for a time series based on \var{intervals}
#' @param intervals integer (no lower than 5), for the days intervals when
#'  \var{timeseries} = \code{TRUE}
#' @param as.sf logical, to return an object of class 'sf'
#' @param ... additional arguments passed to methods. See details.
#' @details 
#' The \code{array} method assumes that \var{object} contains climate data provided 
#'  from a local source; this requires a array with two dimensions, 1st dimension 
#'  contains the day temperature and 2nd dimension the night temperature, 
#'  see help("modis", package = "climatrends") for an example on input structure.
#' 
#' The default method and the sf method assumes that the climate data will be fetched 
#'  from an remote (cloud) \var{source}.
#'
#' When \var{timeseries} = \code{TRUE}, an id is created, 
#'  which is the index for the rownames of the inputted \var{object}.
#' 
#' Additional arguments:
#' 
#' \code{source}: character for the source of remote data. Current remote \var{source} 
#'  is: 'nasapower'
#' 
#' \code{pars}: character vector for the temperature data to be fetched. If 
#'  \code{source} is 'nasapower', the temperature can be adjusted to 2 m, the default,
#'  c("T2M_MAX", "T2M_MIN") or 10 m c("T10M_MAX", "T10M_MIN") 
#' 
#' \code{days.before}: optional, an integer for the number of days before 
#'  \var{day.one} to be included in the timespan.
#' 
#' @return A dataframe with temperature indices:
#' \item{maxDT}{maximun day temperature (degree Celsius)}
#' \item{minDT}{minimum day temperature (degree Celsius)}
#' \item{maxNT}{maximun night temperature (degree Celsius)}
#' \item{minNT}{minimum night temperature (degree Celsius) }
#' \item{DTR}{diurnal temperature range (mean difference between DT 
#' and NT (degree Celsius)) }
#' \item{SU}{summer days, number of days with maximum temperature > 
#' 30 (degree Celsius)}
#' \item{TR}{tropical nights, number of nights with maximum 
#' temperature > 25 (degree Celsius)}
#' \item{CFD}{consecutive frosty days, number of days with temperature 
#' bellow 0 degree Celsius}
#' 
#' @family temperature functions
#' @references 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 
#' 110(D23), D23107. \cr\url{https://doi.org/10.1029/2005JD006119}
#' 
#' @examples
#' # Using local sources
#' data("modis", package = "climatrends")
#' 
#' day <- as.Date("2013-10-28", format = "%Y-%m-%d")
#' 
#' temperature(modis, 
#'             day.one = day,
#'             span = 12)
#' 
#' \donttest{
#' #####################################################
#'  
#' # Using remote sources of climate data
#' 
#' data("lonlatsf", package = "climatrends")
#' 
#' set.seed(123)
#' dates <- as.integer(runif(5, 17660, 17675))
#' dates <- as.Date(dates, origin = "1970-01-01")
#' 
#' # get temperature indices for 30 days after day.one
#' # return a data.frame
#' temp1 <- temperature(lonlatsf,
#'                     day.one = dates,
#'                     span = 30, 
#'                     as.sf = FALSE)
#' temp1
#' 
#' # return a sf object
#' temp2 <- temperature(lonlatsf,
#'                     day.one = dates,
#'                     span = 30)
#' temp2
#' 
#' # indices with intervals of 7 days and return a sf object 
#' temp3 <- temperature(lonlatsf,
#'                     day.one = dates,
#'                     span = 30,
#'                     timeseries = TRUE,
#'                     intervals = 7)
#' temp3
#' 
#' }
#' @export
temperature <- function(object, day.one, 
                        span, ...)
{
  
  UseMethod("temperature")
  
}


#' @rdname temperature
#' @method temperature default
#' @export
temperature.default <- function(object, day.one, 
                                span, timeseries = FALSE,
                                intervals = 5,
                                ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  # coerce inputs to data.frame
  object <- as.data.frame(object)
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. Only lonlat should be provided ",
         "in the default method \n.")
  }
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, span, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  indices <- .temperature_indices(day, night, timeseries, intervals, day.one, span)
  
  class(indices) <- union("clima_df", class(indices))
  
  return(indices)
}

#' @rdname temperature
#' @method temperature array
#' @export
temperature.array <- function(object, day.one, 
                              span, timeseries = FALSE,
                              intervals = 5, ...){
  
  
  if(dim(object)[[2]] == 2) {
    UseMethod("temperature", "default")
  }
  
  # coerce to data.frame
  day.one <- as.data.frame(day.one)[, 1]
  
  day <- get_timeseries(object[, , 1], day.one, span)[[1]]
  
  night <- get_timeseries(object[, , 2], day.one, span)[[1]]
  
  indices <- .temperature_indices(day, night, timeseries, intervals, day.one, span)
  
  class(indices) <- union("clima_df", class(indices))
  
  return(indices)
  
}

#' @rdname temperature
#' @method temperature sf
#' @export
temperature.sf <- function(object, day.one, 
                           span, timeseries = FALSE,
                           intervals = 5, as.sf = TRUE, ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, span, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  indices <- .temperature_indices(day, night, timeseries, intervals, day.one, span)
  
  if (all(as.sf, timeseries)) {
  
    xy <- .lonlat_from_sf(object)
    xy <- as.data.frame(xy)
    xy$id <- 1:dim(xy)[[1]]
    xy <- merge(xy, indices, by = "id")
    
    indices <- st_as_sf(xy, coords = c("lon", "lat"), crs = 4326)
      
  }
  
  if (isTRUE(as.sf) & isFALSE(timeseries)) {
    
    indices <- suppressWarnings(sf::st_bind_cols(object, indices))
  
  }
  
  if (isFALSE(as.sf)) {
    
    class(indices) <- union("clima_df", class(indices))
  
  }
  
  return(indices)
  
}

#' Compute the temperature indices
#' @param day a data.frame with the day temperature
#' @param night a data.frame with the night temperature
#' @inheritParams temperature
#' @noRd
.temperature_indices <- function(day, night, timeseries, intervals, day.one, span){
  
  index <- c("maxDT","minDT","maxNT","minNT","DTR","SU","TR","CFD")
  
  n <- nrow(day)
  
  if (isTRUE(timeseries)) {
    
    # it might happen that when bins are not well distributed across dates
    # in that case the last values are dropped
    # for example, divide the periods of 7 days in a time series of 53 days
    # in that case, the last four observations are dropped to fit in a vector of
    # length == 49 (the maximum integer from dividing days/intervals)
    # organise bins, ids and dates
    bins <- floor(ncol(day) / intervals)
    
    bins <- rep(1:bins, each = intervals, length.out = NA)
    
    # ids are the row names in r
    ids <- rownames(day)
    
    # dates are the first day for each bin
    dates <- NULL
    # for diffent day.one a loop is required to take the
    # sequence of days and the first day in each bin
    for (i in seq_along(day.one)) {
      
      d <- day.one[i]:(day.one[i] + (span - 1))
      
      d <- d[seq_along(bins)]
      
      d <- d[!duplicated(bins)]
      
      d <- rep(d, each = length(index))
      
      dates <- c(dates, d)
      
      
    }
    
    dates <- as.Date(dates, origin = "1970-01-01")
    
    # transpose and keep values until the end of bins
    day <- t(day)
    
    night <- t(night)
    
    # keep data within the lenght of bins
    day <- as.data.frame(day[seq_along(bins), ])
    
    night <- as.data.frame(night[seq_along(bins), ])
    
    # split by bins
    day <- split(day, bins)
    
    night <- split(night, bins)
    
    ind <- mapply(function(X, Y) {
      
      Z <- rbind(X, Y)
      
      i <- apply(Z, 2, function(z) {
        l <- length(z)
        
        XX <- z[1:(l / 2)]
        
        YY <- z[((l / 2) + 1):l]
        
        c(maxDT = .max_temperature(XX),
          minDT = .min_temperature(XX),
          maxNT = .max_temperature(YY),
          minNT = .min_temperature(YY),
          DTR   = .temperature_range(XX, YY),
          SU    = .summer_days(XX),
          TR    = .tropical_nights(YY),
          CFD   = .frosty_days(YY))
        
      })
      
      i <- data.frame(id    = rep(ids, each = length(index)),
                      index = rep(index, times = length(ids)),
                      value = as.vector(unlist(i)), 
                      stringsAsFactors = FALSE)
      
    }, X = day, Y = night, SIMPLIFY = FALSE)
    
    ind <- do.call("rbind", ind)
    
    ind$id <- as.integer(ind$id)
    
    ind$index <- as.character(ind$index)
    
    ind <- ind[order(ind$id), ]
    
    ind$date <- dates
    
    ind <- as.data.frame(ind, stringsAsFactors = FALSE)
    
    rownames(ind) <- seq_along(ind[, 1])
    
    ind <- ind[, c("id", "date", "index", "value")]
    
  } 
  
  
  if (isFALSE(timeseries)) {
    
    day <- split(day, 1:n)
    
    night <- split(night, 1:n)
    
    ind <- mapply(function(X, Y) {
      
      x <- as.vector(as.matrix(X))
      y <- as.vector(as.matrix(Y))
      
      x <- data.frame(maxDT = .max_temperature(x),
                      minDT = .min_temperature(x),
                      maxNT = .max_temperature(y),
                      minNT = .min_temperature(y),
                      DTR   = .temperature_range(x, y),
                      SU    = .summer_days(x),
                      TR    = .tropical_nights(y),
                      CFD   = .frosty_days(y))
      
    }, X = day, Y = night)
    
    ind <- matrix(unlist(ind), 
                  nrow = n, 
                  ncol = length(index), 
                  byrow = TRUE)
    
    dimnames(ind)[[2]] <- index
    
    ind <- as.data.frame(ind, stringsAsFactors = FALSE)
    
    }
  
  return(ind)
}

#' Maximum temperature
#' 
#' @param x a numeric vector
#' @return the maximum temperature 
#' @examples 
#' set.seed(123)
#' temp <- runif(10, 25, 34)
#' .max_temperature(day)
#' @noRd
.max_temperature <- function(x) {
  max(x, na.rm = TRUE)
}

#' Minimum temperature
#'  
#' @param x a numeric vector
#' @return the minimum temperature 
#' @examples 
#' set.seed(123)
#' temp <- runif(10, 25, 34)
#' .min_temperature(day)
#' @noRd
.min_temperature <- function(x) {
  min(x, na.rm = TRUE)
}

#' Diurnal temperature range
#' 
#' Compute mean mean difference between day and night
#' temperature (degree Celsius)
#' 
#' @param x a numeric vector
#' @param y a numeric vector
#' @return the diurnal temperature range 
#' @examples 
#' set.seed(123)
#' day <- runif(10, 25, 34)
#' 
#' set.seed(321)
#' night <- runif(10, 21, 30)
#' 
#' .temperature_range(day, night)
#' @noRd
.temperature_range <- function(x, y) {
  dtr <- x - y
  mean(dtr, na.rm = TRUE)
}

#' Summer days
#' 
#' Compute number of days with maximum temperature > 30 C
#' 
#' @param x a numeric vector
#' @return the summer days index
#' @examples 
#' x <- c(30, NA, 31, 32, 34)
#' .summer_days(x)
#' @noRd
.summer_days <- function(x) {
  x <- x > 30
  sum(x, na.rm = TRUE)
}

#' Tropical days
#' 
#' Compute number of nights with maximum temperature > 25 C
#' 
#' @param x a numeric vector
#' @return the summer days index
#' @examples 
#' x <- c(22, NA, 23, 26, 27, 21)
#' .tropical_nights(x)
#' @noRd
.tropical_nights <- function(x) {
  x <- x > 25
  sum(x, na.rm = TRUE)
}

#' Consecutive frosty days 
#' 
#' Compute number of consecutive days with temperature 
#' bellow 0 degree Celsius
#' 
#' @param x numeric vector
#' @return the CFD index
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' .frosty_days(r)
#' @noRd
.frosty_days <- function(x)
{
  # the function rle is applied
  # which looks for the sequencies of numbers
  # in this case, zeros (0)
  # take the maximum sequency
  # first all values <= 0 are converted to zero (0)
  fd <- ifelse(x <= 0, 0, x)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(fd)$values
  
  keep <- keep == 0
  
  fd <- rle(fd)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  # which means there is no frosty days
  if (length(fd) == 0) {
    fd <- 0
  }
  
  # if there is values, take the maximum sequency
  if (length(fd) != 0) {
    fd <- max(fd, na.rm = TRUE)
  }
  
  return(fd)
  
}
