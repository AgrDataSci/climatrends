#' Rainfall indices
#'
#' Methods to compute rainfall indices over a time series
#'
#' @family precipitation functions
#' @inheritParams get_timeseries
#' @param object a numeric vector with precipitation data or a \code{data.frame}
#'  with geographical coordinates (lonlat), or an object of class \code{sf} 
#'  with geometry 'POINT' or 'POLYGON', or a named \code{matrix} with 
#'  precipitation data. See details.   
#' @param timeseries logical, \code{FALSE} for a single point time series
#'  observation or \code{TRUE} for a time series based on \var{intervals}
#' @param as.sf logical, to return an object of class 'sf'
#' @details 
#' #' Additional arguments:
#' 
#' \code{intervals}: an integer (no lower than 5), for the days intervals when
#'  \var{timeseries = TRUE}
#'  
#' \code{last.day}: optional to \var{span}, an object of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD)  for the last day of the time series
#'  
#' \code{dates}: a character (or Date or numeric) vector for the dates of tmax and tmin
#'  in the \code{default} method
#' 
#' \code{data.from}: character for the source of remote data. Current remote source 
#'  is: 'nasapower'
#' 
#' \code{pars}: character vector for the precipitation data to be fetched. If 
#'  \code{data.from} is 'nasapower', the default precipitation parameter is "PRECTOTCORR".
#' 
#' \code{days.before}: optional, an integer for the number of days before 
#'  \var{day.one} to be included in the timespan.
#'  
#' # S3 Methods 
#' 
#' The \code{matrix} method assumes that \var{object} contains climate data available in
#'  your R section; see help("rain_dat", package = "climatrends") for an example on input 
#'  structure.
#' 
#' The \code{data.frame} and the \code{sf} methods assumes that the climate data
#'  will e fetched from a remote (cloud) source that be adjusted using the argument 
#'  \var{data.from}.
#'
#' When \var{timeseries} = \code{TRUE}, an id is created, 
#'  which is the index for the rownames of the inputted \var{object}.
#'
#' @return A dataframe with rainfall indices:
#' \item{MLDS}{maximum length of consecutive dry day, rain < 1 mm (days)}
#' \item{MLWS}{maximum length of consecutive wet days, rain >= 1 mm (days)}
#' \item{R10mm}{number of heavy precipitation days 10 >= rain < 20 mm (days)}
#' \item{R20mm}{number of very heavy precipitation days rain >= 20 (days)}
#' \item{Rx1day}{maximum 1-day precipitation (mm)}
#' \item{Rx5day}{maximum 5-day precipitation (mm)}
#' \item{R95p}{total precipitation when rain > 95th percentile (mm)}
#' \item{R99p}{total precipitation when rain > 99th percentile (mm)}
#' \item{Rtotal}{total precipitation (mm) in wet days, rain >= 1 (mm)}
#' \item{SDII}{simple daily intensity index, total precipitation divided by the
#'  number of wet days (mm/days)}
#'  
#' @references 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 
#' 110(D23), D23107. \doi{https://doi.org/10.1029/2005JD006119}
#' 
#' @examples
#' # A vector with precipitation data
#' set.seed(987219)
#' rain <- runif(50, min = 0, max = 6)
#' 
#' rainfall(rain)
#' 
#' # Return as timeseries with intervals of 7 days
#' dates <- 17650:17699
#' rainfall(rain, dates = dates, timeseries = TRUE, intervals = 7)
#' 
#' ######################################################
#' 
#' # the matrix method
#' data("rain_dat", package = "climatrends")
#' 
#' rainfall(rain_dat,
#'          day.one = "2013-10-28",
#'          span = 12)
#' 
#' 
#' @importFrom stats quantile
#' @export
rainfall <- function(object, ...)
{
  UseMethod("rainfall")
}
#' @rdname rainfall
#' @method rainfall default
#' @export
rainfall.default <- function(object, ..., timeseries = FALSE){
  
  dots <- list(...)
  dates <- dots[["dates"]]
  intervals <- dots[["intervals"]]
  
  if (!is.null(dates)) {
    dates <- .coerce2Date(dates)
  }
  
  setnulldate <- FALSE
  if (is.null(dates)) {
    dates <- .coerce2Date(1:length(object))
    
    if (isTRUE(timeseries)) {
      setnulldate <- TRUE
    }
  }
  
  rain <- data.frame(id = 1, value = object, date = dates, stringsAsFactors = FALSE)
  
  indices <- .rainfall_indices(rain, timeseries, intervals)
  
  if (isTRUE(setnulldate)) {
    message("Intervals set with no visible dates, returning NAs \n")
    indices$date <- NA 
  }
  
  return(indices)
  
}

#' @rdname rainfall
#' @method rainfall data.frame
#' @export
rainfall.data.frame <- function(object, day.one, span = NULL, ...,
                                timeseries = FALSE){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  intervals <- dots[["intervals"]]
  
  # coerce inputs to data.frame
  object <- as.data.frame(object)
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In rainfall.data.frame(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- "PRECTOTCORR"
  }
  
  rain <- get_timeseries(object, day.one, span, pars = pars, ...)[[1]]
  
  indices <- .rainfall_indices(rain, timeseries, intervals)
  
  return(indices)
  
}

#' @rdname rainfall
#' @method rainfall matrix
#' @export
rainfall.matrix <- function(object, day.one, span = NULL, ...,
                            timeseries = FALSE){
  dots <- list(...)
  intervals <- dots[["intervals"]]
  days.before <- dots[["days.before"]]
  
  # coerce to data.frame
  day.one <- as.data.frame(day.one)[, 1]
  
  rain <- get_timeseries(object, day.one, span = span, 
                         days.before = days.before)[[1]]
  
  indices <- .rainfall_indices(rain, timeseries, intervals)
  
  return(indices)
  
}


#' @rdname rainfall
#' @method rainfall sf
#' @export
rainfall.sf <- function(object, day.one, span = NULL, ...,
                        timeseries = FALSE, as.sf = TRUE){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  intervals <- dots[["intervals"]]
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- "PRECTOT"
  }
  
  rain <- get_timeseries(object, day.one, span, pars = pars, ...)[[1]]
  
  indices <- .rainfall_indices(rain, timeseries, intervals)
  
  if (isTRUE(as.sf) & isFALSE(timeseries)) {
    
    indices <- cbind(object, indices)
    
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

#' Rainfall indices
#' 
#' This is the main function, the others are handling methods
#' 
#' @param rain data.frame with following values id, value and date
#' @param timeseries if indices are to be returned in timeseries 
#' @param intervals the intervals in the timeseries
#' @examples
#' r <- data.frame(id = 1, 
#'                 value = runif(21, 0, 4), 
#'                 date = 200:220, 
#'                 stringsAsFactors = FALSE)
#' 
#' .rainfall_indices(r)
#' 
#' @noRd 
.rainfall_indices <- function(rain, timeseries = FALSE, intervals = NULL){
  
  index <- c("MLDS","MLWS","R10mm","R20mm","Rx1day",
             "Rx5day","R95p","R99p","Rtotal","SDII")
  
  nr <- max(unique(rain$id))
  
  if (isTRUE(timeseries)) {
    
    ids <- unique(rain$id)
    
    rain <- split(rain, rain$id)
    
    # it might happen that when bins are not well distributed across dates
    # in that case the last values are dropped
    # for example, divide the periods of 7 days in a time series of 53 days
    # in that case, the last four observations are dropped to fit in a vector of
    # length == 49 (the maximum integer from dividing days/intervals)
    # organise bins, ids and dates
    rain <- lapply(rain, function(x){
      
      r <- dim(x)[[1]]
      
      bins <- floor(r / intervals)
      
      bins <- rep(1:bins, each = intervals, length.out = NA)
      
      x <- x[1:length(bins), ]
      
      x$id <- paste(x$id, bins, sep = "_")
      
      x 
      
    })
    
    rain <- do.call("rbind", rain)
    
    rain <- split(rain, rain$id)
    
    # calculate indices
    ind <- lapply(rain, function(x) {
      
      id <- strsplit(x$id[1], "_")[[1]][[1]]
      
      d <- x$date[[1]]
      
      y <- x$value
      
      i <- c(.dryspell(y),
             .wetspell(y),
             .r_ten_mm(y),
             .r_twenty_mm(y),
             .r_one_day(y),
             .r_five_day(y),
             .very_wet_days(y),
             .extreme_wet_days (y),
             .r_total(y),
             .sdii(y))
      
      i <- data.frame(id    = id,
                      date  = as.character(d),
                      index = index,
                      value = i, 
                      stringsAsFactors = FALSE)
      
    })
    
    ind <- do.call("rbind", ind)
    
    ind$id <- as.integer(ind$id)
    
    ind$index <- as.character(ind$index)
    
    ind$date <- .coerce2Date(ind$date)
    
    ind <- ind[order(ind$id), ]
    
    ind <- as.data.frame(ind, stringsAsFactors = FALSE)
    
    rownames(ind) <- seq_along(ind[, 1])
    
    ind <- ind[, c("id", "date", "index", "value")]
    
  } 
  
  # if no time series required then
  if (isFALSE(timeseries)) {
    
    # split r by rows
    r <- split(rain, rain$id)
    
    ind <- lapply(r, function(x) {
      
      x <- x$value
      
      x <- data.frame(.dryspell(x),
                      .wetspell(x),
                      .r_ten_mm(x),
                      .r_twenty_mm(x),
                      .r_one_day(x),
                      .r_five_day(x),
                      .very_wet_days(x),
                      .extreme_wet_days (x),
                      .r_total(x),
                      .sdii(x),
                      stringsAsFactors = FALSE)
      
    })
    
    
    ind <- do.call("rbind", ind)
    
    names(ind) <- index
   
    ind <- as.data.frame(ind, stringAsFactor = FALSE)
    
    integ <- c("MLDS","MLWS","R10mm","R20mm")
    
    ind[integ] <- lapply(ind[integ] , as.integer)
    
    rownames(ind) <- seq_along(ind[, 1])
    
  }
  
  class(ind) <- union("clima_df", class(ind))
  
  return(ind)
  
}

#' Maximum length of consecutive dry days
#' @param x numeric vector
#' @return the MLDS index, which is the maximum length of consecutive dry days
#' precipitation < 1 mm
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' climatrends:::.dryspell(r)
#' @noRd
.dryspell <- function(x){
  if (all(is.na(x))) {
    return(NA)
  }
  
  # the function rle is applied
  # which looks for the sequencies of numbers
  # in this case, zeros (0)
  # take the maximum sequency
  # first all values < 1 are converted to zero (0)
  ds <- x[!is.na(x)]
  
  ds <- ifelse(ds < 1, 0, ds)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(ds)$values
  
  keep <- keep == 0
  
  ds <- rle(ds)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  # which means there is no dry spell
  if (length(ds) == 0) {
    ds <- 0
  }
  
  # if there is values, take the maximum sequency
  if (length(ds) != 0) {
    ds <- max(ds, na.rm = TRUE)
  }
  
  return(ds)
  
}

#' Maximum length of consecutive wet days
#' @param x numeric vector
#' @return the MLWS index, which is the maximum length of consecutive wet days
#' precipitation > 1 mm
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:11)] <- 0.1
#' climatrends:::.wetspell(r)
#' @noRd
.wetspell <- function(x){
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  # the function rle is applied
  # which looks for the sequencies of zeros
  # take the maximum sequency
  # first all values >= 1 are converted to zero (0)
  # no precipitation (r < 1) is converted to two (2)
  ws <- x[!is.na(x)]
  
  ws <- ifelse(ws >= 1, 0, 2)
  
  # get the lengths of each sequency of zeros (0)
  keep <- rle(ws)$values
  
  keep <- keep == 0
  
  ws <- rle(ws)$lengths[keep]
  
  # if there is no value (empty()) then set as zero
  if (length(ws) == 0) {
    ws <- 0
  }
  # if there is values, take the maximum sequecy
  if (length(ws) != 0) {
    ws <- max(ws, na.rm = TRUE)
  }
  
  return(ws)
}

#' Heavy precipitation days (10 >= r < 20 mm)
#' @param x numeric vector
#' @return the R10mm index, which is number of heavy precipitation days (10 >= r
#'  < 20 mm)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 12)
#' r[c(1,4,9:11)] <- 0.1
#' climatrends:::.r_ten_mm(r)
#' @noRd
.r_ten_mm <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  rt <- sum(x >= 10 & x < 20, na.rm = TRUE)
  
  return(rt)
  
}

#' Very heavy precipitation days (r >= 20)
#' @param x numeric vector
#' @return the R20mm index, which is number of very heavy precipitation days (r
#'  >= 20)
#' @examples
#' set.seed(12)
#' r <- runif(20, 10, 23)
#' r[c(1,4,9:11)] <- 0.1
#' climatrends:::.r_twenty_mm(r)
#' @noRd
.r_twenty_mm <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  rtw <- sum(x >= 20, na.rm = TRUE)
  
  return(rtw)
  
}

#' Simple rainfall intensity index
#' @param x numeric vector
#' @return the SDII index, which is the simple daily intensity 
#' index total precipitation divided by the number of wet days (r >= 1.0mm)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#'
#' r[c(1,4,9:11)] <- 0.1
#'
#' .sdii(r)
#'
#' climatrends:::.sdii(rep(0.1, 9))
#' @noRd
.sdii <- function(x){
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  # total precipitation
  tp <- sum(x, na.rm = TRUE)
  
  # number of wet days
  wd <- length(x[x >= 1])
  
  #if both zero, then return 0
  if (wd == 0) {
    si <- 0L
  }else{
    si <- tp / wd
  }
  
  return(si)
  
}

#' Compute Rx5day rainfall index
#' @param x numeric vector
#' @return the Rx5day index, which is the maximun sum 
#' of rain in consecutive 5 days
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' climatrends:::.r_five_day(r)
#' @noRd
.r_five_day <- function(x){
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  # this look for the maximum sum of rain in
  # consecutive 5 days
  l <- length(x)
  
  if (l < 5) {
    
    return(0L)
  
  }
  
  r5day <- NULL
  
  for (i in 1:(l-4)){
    
    r5day <- cbind(r5day, sum(x[i:(i + 4)], na.rm = TRUE))
    
  }
  
  r5day <- max(r5day, na.rm = TRUE)
  
  return(r5day)
  
}

#' Maximum 1-day rainfall
#' @param x numeric vector
#' @return the Rx1day index, which is the 1-day rainfall
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' climatrends:::.r_one_day(r)
#' @noRd
.r_one_day <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  ro <- max(x, na.rm = TRUE)
  
  return(ro)
  
}

#' Total rainfall (mm) in wet days (r >= 1)
#' @param x numeric vector
#' @return the Rtotal index, which is sum of rainfall (mm) in wet days (r >= 1)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' climatrends:::.r_total(r)
#' @noRd
.r_total <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  rt <- x[x >= 1]
  
  rt <- sum(rt, na.rm = TRUE)
  
  return(rt)
  
}


#' Very wet days
#' @param x numeric vector
#' @return the R95p index, total PRCP when rain > 95th percentile
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' climatrends:::.very_wet_days(r)
#' @noRd
.very_wet_days <- function(x) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  q <- stats::quantile(x, probs = seq(0, 1, 0.05), na.rm = TRUE)
  q <- q["95%"]
  
  vwd <- x[x > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}

#' Extreme wet days
#' @param x numeric vector
#' @return the R99p index, total PRCP when rain > 99th percentile
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.extreme_wet_days(r)
#' @noRd
.extreme_wet_days <- function(x){
  
  if (all(is.na(x))) {
    return(NA)
  }
  
  q <- stats::quantile(x, probs = seq(0, 1, 0.01), na.rm = TRUE)
  q <- q["99%"]
  
  vwd <- x[x > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}
