#' Temperature indices
#'
#' Methods to compute temperature indices over a time series
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
#' \code{last.day}: optional to \var{span}, an object of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD)  for the last day of the time series
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
#' \item{WSDI}{maximum warm spell duration, consecutive days with 
#' temperature > 90th percentile}
#' \item{CSDI}{maximum cold spell duration, consecutive nights with 
#' temperature < 10th percentile}
#' \item{T10p}{the 10th percentile of night tempeture (degree Celsius)}
#' \item{T90p}{the 90th percentile of day tempeture (degree Celsius)}
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
#' temperature(modis,
#'             day.one = "2013-10-28",
#'             span = 12)
#' 
#' \donttest{
#' #####################################################
#' 
#' # Using remote sources of climate data
#' 
#' data("lonlatsf", package = "climatrends")
#' 
#' # some random dates provided as integers and coerced to Dates internally
#' set.seed(2718279)
#' dates <- as.integer(runif(5, 17660, 17675))
#' 
#' # get temperature indices for 30 days after day.one
#' # return a data.frame
#' temp1 <- temperature(lonlatsf,
#'                      day.one = dates,
#'                      span = 30,
#'                      as.sf = FALSE)
#' temp1
#' 
#' # get temperature indices from "2010-12-01" to "2011-01-31"
#' temp2 <- temperature(lonlatsf,
#'                      day.one = "2010-12-01",
#'                      last.day = "2011-01-31",
#'                      as.sf = FALSE)
#' temp2
#' 
#' # indices from "2010-12-01" to "2011-01-31" with intervals of 7 days
#' temp3 <- temperature(lonlatsf,
#'                      day.one = "2010-12-01",
#'                      last.day = "2011-01-31",
#'                      timeseries = TRUE,
#'                      intervals = 7,
#'                      as.sf = FALSE)
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
                                span = NULL, timeseries = FALSE,
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
  
  indices <- .temperature_indices(day, night, timeseries, intervals, day.one, span, ...)
  
  class(indices) <- union("clima_df", class(indices))
  
  return(indices)
}

#' @rdname temperature
#' @method temperature array
#' @export
temperature.array <- function(object, day.one, 
                              span = NULL, timeseries = FALSE,
                              intervals = 5, ...){
  
  
  if (dim(object)[[2]] == 2) {
    UseMethod("temperature", "default")
  }
  
  # coerce to data.frame
  day.one <- as.vector(t(day.one))
  
  day <- get_timeseries(object[, , 1], day.one, span, ...)[[1]]
  
  night <- get_timeseries(object[, , 2], day.one, span, ...)[[1]]
  
  indices <- .temperature_indices(day, night, timeseries, intervals, day.one, span, ...)
  
  return(indices)
  
}

#' @rdname temperature
#' @method temperature sf
#' @export
temperature.sf <- function(object, day.one, 
                           span = NULL, timeseries = FALSE,
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
  
  indices <- .temperature_indices(day, night, timeseries, intervals, day.one, span, ...)
  
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
  
  if (isFALSE(as.sf) & isTRUE(timeseries)) {
    
    xy <- .lonlat_from_sf(object)
    xy <- as.data.frame(xy)
    xy$id <- 1:dim(xy)[[1]]
    indices <- merge(xy, indices, by = "id", all.y = TRUE)
    
    class(indices) <- union("clima_df", class(indices))
  
  }
  
  return(indices)
  
}

#' Compute the temperature indices
#' @param day a data.frame with the day temperature
#' @param night a data.frame with the night temperature
#' @inheritParams temperature
#' @noRd
.temperature_indices <- function(day, night, timeseries, intervals, day.one, span = NULL, last.day = NULL){
  
  index <- c("maxDT", "minDT", "maxNT", "minNT",
             "DTR", "SU", "TR", "CFD",
             "WSDI", "CSDI", "T10p", "T90p")
  
  n <- nrow(day)
  
  if (isTRUE(timeseries)) {
    
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
      
      if (!.is_Date(last.day)) {
        
        last.day <- .coerce2Date(last.day)
        
      }
      
      span <- as.integer(last.day[[1]] - day.one[[1]])
      
    }
    
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
        
        # day
        XX <- z[1:(l / 2)]
        
        # night
        YY <- z[((l / 2) + 1):l]
        
        c(maxDT  = .max_temperature(XX),
          minDT  = .min_temperature(XX),
          maxNT  = .max_temperature(YY),
          minNT  = .min_temperature(YY),
          DTR    = .temperature_range(XX, YY),
          SU     = .summer_days(XX),
          TR     = .tropical_nights(YY),
          CFD    = .frosty_days(YY),
          WSDI = .max_wsdi(XX),
          CSDI = .max_csdi(YY),
          T10p  = .t10p(YY),
          T90p  = .t90p(XX))
        
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
                      CFD   = .frosty_days(y),
                      WSDI  = .max_wsdi(x),
                      CSDI  = .max_csdi(y),
                      T10p  = .t10p(y),
                      T90p  = .t90p(x))
      
    }, X = day, Y = night)
    
    ind <- matrix(unlist(ind), 
                  nrow = n, 
                  ncol = length(index), 
                  byrow = TRUE)
    
    dimnames(ind)[[2]] <- index
    
    ind <- as.data.frame(ind, stringsAsFactors = FALSE)
    
    integ <- c("DTR","SU","TR","CFD","WSDI","CSDI")
    
    ind[integ] <- lapply(ind[integ] , as.integer)
    
    }
  
  class(ind) <- union("clima_df", class(ind))
  
  return(ind)
}


#' Maximum warm spell duration
#' 
#' Maximum number of days with consecutive days
#'  when temp > 90th percentile
#' 
#' @param x a numeric vector
#' @param reorder logical, if a combination of day and night is provided
#'  reorder the vector
#' @return the maximum warm spell duration index
#' 
#' set.seed(871)
#' x <- rnorm(30, 34)
#' 
#' .max_wsdi(x)
#' @noRd
.max_wsdi <- function(x, reorder = FALSE) {
  
  if (isTRUE(reorder)) {
    
    nv <- length(x)
    
    n <- seq(from = 1, to = nv, by = 2)
    
    d <- seq(from = 2, to = nv, by = 2)
    
    names(x) <- c(n, d)
    
    x <- x[order(as.integer(names(x)))]
    
  }
  
  q90 <- stats::quantile(x, probs = 0.9, na.rm = TRUE)
  # days above q90 should be returned as 0s
  ws <- as.integer(x < q90)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(ws)$values
  
  keep <- keep == 0
  
  ws <- rle(ws)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  if (length(ws) == 0) {
    ws <- 0L
  }
  # if there is values, take the maximum sequecy
  if (length(ws) != 0) {
    ws <- max(ws, na.rm = TRUE)
    ws <- as.integer(ws)
  }
  
  return(ws)
  
}


#' Maximum cool spell duration
#' 
#' Maximum number of days with consecutive days
#'  when temp < 10th percentile
#' 
#' @param x a numeric vector
#' @param reorder logical, if a combination of day and night is provided
#'  reorder the vector
#' @return the maximum cool spell duration index
#' 
#' set.seed(871)
#' x <- runif(14, 6, 34)
#' 
#' .max_csdi(x)
#' 
#' @noRd
.max_csdi <- function(x, reorder = FALSE) {
  
  if (isTRUE(reorder)) {
    
    nv <- length(x)
    
    n <- seq(from = 1, to = nv, by = 2)
    
    d <- seq(from = 2, to = nv, by = 2)
    
    names(x) <- c(n, d)
    
    x <- x[order(as.integer(names(x)))]
    
  }
  
  q10 <- stats::quantile(x, probs = 0.1, na.rm = TRUE)
  # days below q10 should be returned as 0s
  cs <- as.integer(x > q10)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(cs)$values
  
  keep <- keep == 0
  
  cs <- rle(cs)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  if (length(cs) == 0) {
    cs <- 0L
  }
  # if there is values, take the maximum sequecy
  if (length(cs) != 0) {
    cs <- max(cs, na.rm = TRUE)
    cs <- as.integer(cs)
  }
  
  return(cs)
  
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
  x <- sum(x, na.rm = TRUE)
  x <- as.integer(x)
  return(x)
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
  x <- sum(x, na.rm = TRUE)
  x <- as.integer(x)
  return(x)
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
  # first all values x <= 0 are converted to zero (0)
  
  x <- x[!is.na(x)]
  
  fd <- ifelse(x <= 0, 0, 
               ifelse(x > 0, 2, x))
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(fd)$values
  
  keep <- keep == 0
  
  fd <- rle(fd)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  # which means there is no frosty days
  if (length(fd) == 0) {
    fd <- 0L
  }
  
  # if there is values, take the maximum sequency
  if (length(fd) != 0) {
    fd <- max(fd, na.rm = TRUE)
    fd <- as.integer(fd)
  }
  
  return(fd)
  
}

#' Day temp 90p
#' 
#' the value for the 90th percentile of temperature
#' 
#' #' @param x numeric vector
#' #' @examples 
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' .t90p(r)
#' @noRd
.t90p <- function(x){
  x <- stats::quantile(x, probs = 0.9, na.rm = TRUE)
  x <- as.numeric(x)
  return(x)
}

#' Temp 90p
#' 
#' the value for the 10th percentile of temperature
#' 
#' @param x numeric vector
#' @examples 
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' .t10p(r)
#' @noRd
.t10p <- function(x){
  x <- stats::quantile(x, probs = 0.1, na.rm = TRUE)
  x <- as.numeric(x)
  return(x)
}
