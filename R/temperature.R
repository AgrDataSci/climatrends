#' Temperature indices
#'
#' Methods to compute temperature indices over a time series
#'
#' @family temperature functions
#' @param object a data.frame (or any object that can be coerced to data.frame) 
#'  with geographical coordinates (lonlat), or an object of class \code{sf}
#'  with geometry 'POINT' or 'POLYGON', or an \code{array} with two dimensions 
#'  containing the temperature data or a \code{clima_ls} with maximum and minimum 
#'  temperature, in that order. See details 
#' @inheritParams get_timeseries
#' @param timeseries logical, \code{FALSE} for a single point time series
#'  observation or \code{TRUE} for a time series based on \var{intervals}
#' @param intervals integer (no lower than 5), for the days intervals when
#'  \var{timeseries} = \code{TRUE}
#' @param as.sf logical, to return an object of class 'sf'
#' @param ... additional arguments passed to methods. See details
#' @details 
#' The \code{array} method assumes that \var{object} contains climate data provided 
#'  from a local source; this requires an array with two dimensions, 1st dimension 
#'  contains the day temperature and 2nd dimension the night temperature, 
#'  see help("modis", package = "climatrends") for an example on input structure.
#' 
#' The \code{default} method and the \code{sf} method assumes that the climate data
#'  will e fetched from a remote (cloud) source that be adjusted using the argument 
#'  \var{data.from}.
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
#' \code{data.from}: character for the source of remote data. Current remote source 
#'  is: 'nasapower'
#' 
#' \code{pars}: character vector for the temperature data to be fetched. If 
#'  \code{data.from} is 'nasapower', the temperature can be adjusted to 2 m, the default,
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
temperature <- function(object, ...)
{
  
  UseMethod("temperature")
  
}


#' @rdname temperature
#' @method temperature default
#' @export
temperature.default <- function(object, day.one, 
                                span = NULL, timeseries = FALSE,
                                intervals = 5, ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  last.day <- dots[["last.day"]]
  
  # coerce inputs to data.frame
  object <- as.data.frame(object)
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In temperature.default(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.vector(t(day.one))
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, span, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  indices <- .temperature_indices(day, night, timeseries, intervals)
  
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
  
  dots <- list(...)
  last.day <- dots[["last.day"]]
  
  # coerce to data.frame
  day.one <- as.vector(t(day.one))
  
  ts <- get_timeseries(object, day.one, span = span, last.day = last.day, ...)
  
  indices <- .temperature_indices(ts[[1]], ts[[2]], timeseries, intervals)
  
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
  last.day <- dots[["last.day"]]
  
  day.one <- as.vector(t(day.one))
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, span, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  indices <- .temperature_indices(day, night, timeseries, intervals)
  
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

#' @rdname temperature
#' @method temperature clima_ls
#' @export
temperature.clima_ls <- function(object, 
                                 timeseries = FALSE,
                                 intervals = 5, ...){
  
  day <- object[[1]]
  night <- object[[2]]
  
  result <- .temperature_indices(day, night, timeseries, intervals)
  
  return(result)
  
}

#' Compute the temperature indices
#' @param day a data.frame with the day temperature
#' @param night a data.frame with the night temperature
#' @inheritParams temperature
#' @noRd
.temperature_indices <- function(day, night, timeseries, intervals){
  
  index <- c("maxDT", "minDT", "maxNT", "minNT",
             "DTR", "SU", "TR", "CFD",
             "WSDI", "CSDI", "T10p", "T90p")
  
  nr <- max(unique(day$id))
  
  if (isTRUE(timeseries)) {
    
    ids <- unique(day$id)
    
    day <- split(day, day$id)
    
    night <- split(night, night$id)
    
    # it might happen that when bins are not well distributed across dates
    # in that case the last values are dropped
    # for example, divide the periods of 7 days in a time series of 53 days
    # in that case, the last four observations are dropped to fit in a vector of
    # length == 49 (the maximum integer from dividing days/intervals)
    # organise bins, ids and dates
    day <- lapply(day, function(x){
      
      r <- dim(x)[[1]]
      
      bins <- floor(r / intervals)
      
      bins <- rep(1:bins, each = intervals, length.out = NA)
      
      x <- x[1:length(bins), ]
      
      x$id <- paste(x$id, bins, sep = "_")
      
      x 
      
    })
    
    day <- do.call("rbind", day)
    
    night <- lapply(night, function(x) {
      
      r <- dim(x)[[1]]
      
      bins <- floor(r / intervals)
      
      bins <- rep(1:bins, each = intervals, length.out = NA)
      
      x <- x[1:length(bins), ]
      
      x$id <- paste(x$id, bins, sep = "_")
      
      x 
      
    })
    
    night <- do.call("rbind", night)
    
    # split by bins
    day <- split(day, day$id)
    
    night <- split(night, night$id)
    
    ind <- mapply(function(X, Y) {
      
      id <- strsplit(Y$id[1], "_")[[1]][[1]]
      
      d <- Y$date[[1]]
      
      XX <- X$value
      
      YY <- Y$value

      i <- c(maxDT = .max_temperature(XX),
             minDT = .min_temperature(XX),
             maxNT = .max_temperature(YY),
             minNT = .min_temperature(YY),
             DTR   = .temperature_range(XX, YY),
             SU    = .summer_days(XX),
             TR    = .tropical_nights(YY),
             CFD   = .frosty_days(YY),
             WSDI  = .max_wsdi(XX),
             CSDI  = .max_csdi(YY),
             T10p  = .t10p(YY),
             T90p  = .t90p(XX))
      
      i <- data.frame(id    = id,
                      date  = as.character(d),
                      index = names(i),
                      value = as.vector(unlist(i)), 
                      stringsAsFactors = FALSE)
      
    }, X = day, Y = night, SIMPLIFY = FALSE)
    
    ind <- do.call("rbind", ind)
    
    ind$id <- as.integer(ind$id)
    
    ind$index <- as.character(ind$index)
    
    ind$date <- .coerce2Date(ind$date)
    
    ind <- ind[order(ind$id), ]
    
    ind <- as.data.frame(ind, stringsAsFactors = FALSE)
    
    rownames(ind) <- seq_along(ind[, 1])
    
    ind <- ind[, c("id", "date", "index", "value")]
    
  } 
  
  
  if (isFALSE(timeseries)) {
    
    day <- split(day, day$id)
    
    night <- split(night, night$id)
    
    ind <- mapply(function(X, Y) {
      
      x <- as.vector(as.matrix(X$value))
      y <- as.vector(as.matrix(Y$value))
      
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
                      T90p  = .t90p(x),
                      stringsAsFactors = FALSE)
      
    }, X = day, Y = night)
    
    ind <- matrix(unlist(ind), 
                  nrow = nr, 
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
#' @return the maximum warm spell duration index
#' @examples
#' set.seed(871)
#' x <- rnorm(30, 34)
#' 
#' .max_wsdi(x)
#' @noRd
.max_wsdi <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
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
#' @return the maximum cool spell duration index
#' @examples
#' set.seed(871)
#' x <- runif(14, 6, 34)
#' 
#' .max_csdi(x)
#' 
#' @noRd
.max_csdi <- function(x) {

  if (all(is.na(x))) {
    return(NA)
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
#' x <- runif(10, 25, 34)
#' .max_temperature(x)
#' @noRd
.max_temperature <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  max(x, na.rm = TRUE)
}

#' Minimum temperature
#'  
#' @param x a numeric vector
#' @return the minimum temperature 
#' @examples 
#' set.seed(123)
#' x <- runif(10, 25, 34)
#' .min_temperature(x)
#' @noRd
.min_temperature <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  
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
  
  if (all(is.na(c(x,y)))) {
    
    return(NA)
  
  }
  
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
  
  if (all(is.na(x))) {
    return(NA)
  }
  
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
  
  if (all(is.na(x))) {
    return(NA)
  }
  
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
.frosty_days <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  
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

#' Temp 90p
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
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  x <- stats::quantile(x, probs = 0.9, na.rm = TRUE)
  x <- as.numeric(x)
  return(x)
}

#' Temp 10p
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
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  x <- stats::quantile(x, probs = 0.1, na.rm = TRUE)
  x <- as.numeric(x)
  return(x)
}
