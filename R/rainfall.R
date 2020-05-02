#' Rainfall indices
#'
#' Methods to compute rainfall indices over a time series
#'
#' @family precipitation functions
#' @inheritParams get_timeseries
#' @param timeseries logical, \code{FALSE} for a single point time series
#'  observation or \code{TRUE} for a time series based on \var{intervals}
#' @param intervals integer (no lower than 5), for the days intervals when
#'  \var{timeseries} = \code{TRUE}
#' @param as.sf logical, to return an object of class 'sf'
#' @details 
#' The \code{matrix} method assumes that \var{object} contains climate data provided 
#'  from a local source; see help("chirp", package = "climatrends") for an example on input 
#'  structure.
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
#' \code{pars}: character vector for the precipitation data to be fetched. If 
#'  \code{source} is 'nasapower', the default precipitation parameter is "PRECTOT".
#' 
#' \code{days.before}: optional, an integer for the number of days before 
#'  \var{day.one} to be included in the timespan.
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
#' @family climatology functions
#' @references 
#' Aguilar E., et al. (2005). Journal of Geophysical Research, 
#' 110(D23), D23107. \cr\url{https://doi.org/10.1029/2005JD006119}
#' 
#' @examples
#' # Using local sources
#' data("chirp", package = "climatrends")
#' 
#' rainfall(chirp,
#'          day.one = "2013-10-28",
#'          span = 12)
#' 
#' #####################################################
#' \donttest{
#' # Using remote sources of climate data
#' 
#' data("lonlatsf", package = "climatrends")
#' 
#' # some random dates provided as integers and coerced to Dates internally
#' set.seed(2718279)
#' dates <- as.integer(runif(5, 17660, 17675))
#' 
#' # get precipitation indices for 30 days after day.one
#' # return a data.frame
#' rain1 <- rainfall(lonlatsf,
#'                   day.one = dates,
#'                   span = 30,
#'                   as.sf = FALSE)
#' rain1
#' 
#' # get precipitation indices from "2010-12-01" to "2011-01-31"
#' rain2 <- rainfall(lonlatsf,
#'                   day.one = "2010-12-01",
#'                   last.day = "2011-01-31",
#'                   as.sf = FALSE)
#' rain2
#' 
#' # indices from "2010-12-01" to "2011-01-31" with intervals of 7 days
#' rain3 <- rainfall(lonlatsf,
#'                   day.one = "2010-12-01",
#'                   last.day = "2011-01-31",
#'                   timeseries = TRUE,
#'                   intervals = 7,
#'                   as.sf = FALSE)
#' rain3
#' 
#'}       
#'          
#' @importFrom stats quantile
#' @export
rainfall <- function(object, day.one, span, ...)
{
  UseMethod("rainfall")
}


#' @rdname rainfall
#' @method rainfall default
#' @export
rainfall.default <- function(object, day.one, span = NULL, 
                             timeseries = FALSE,
                             intervals = 5,
                             ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  # coerce inputs to data.frame
  object <- as.data.frame(object)
  if(dim(object)[[2]] != 2) {
    stop("Subscript out of bounds. In rainfall.default(),",
         " only lonlat should be provided. \n")
  }
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- "PRECTOT"
  }
  
  rain <- get_timeseries(object, day.one, span, pars = pars, ...)[[1]]
  
  indices <- .rainfall_indices(rain, timeseries, intervals, day.one, span, ...)
  
  return(indices)
  
}

#' @rdname rainfall
#' @method rainfall matrix
#' @export
rainfall.matrix <- function(object, day.one, span = NULL,
                            timeseries = FALSE,
                            intervals = 5, ...){
  
  if(dim(object)[[2]] == 2) {
    UseMethod("rainfall", "default")
  }
  
  # coerce to data.frame
  day.one <- as.data.frame(day.one)[, 1]
  
  rain <- get_timeseries(object, day.one, span, ...)[[1]]
  
  indices <- .rainfall_indices(rain, timeseries, intervals, day.one, span, ...)
  
  return(indices)
  
}


#' @rdname rainfall
#' @method rainfall sf
#' @export
rainfall.sf <- function(object, day.one, span = NULL, 
                        timeseries = FALSE,
                        intervals = 5, as.sf = TRUE, ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- "PRECTOT"
  }
  
  rain <- get_timeseries(object, day.one, span, pars = pars, ...)[[1]]
  
  indices <- .rainfall_indices(rain, timeseries, intervals, day.one, span, ...)
  
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


.rainfall_indices <- function(rain, timeseries, intervals, day.one, span = NULL, last.day = NULL){
  
  index <- c("MLDS","MLWS","R10mm","R20mm","Rx1day",
             "Rx5day","R95p","R99p","Rtotal","SDII")
  
  nr <- dim(rain)[[1]]
  
  nc <- dim(rain)[[2]]
  
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
    bins <- floor(nc / intervals)
    
    bins <- rep(1:bins, each = intervals, length.out = NA)
    
    # ids are the row names in rain
    ids <- rownames(rain)
    
    # dates are the first day for each bin
    dates <- NULL
    # for diffent day.one a loop is required to take the
    # sequence of days and the first day in each bin
    for (i in seq_along(day.one)) {
      
      d <- day.one[[i]]:(day.one[[i]] + (span - 1))
      
      d <- d[seq_along(bins)]
      
      d <- d[!duplicated(bins)]
      
      d <- rep(d, each = length(index))
      
      dates <- c(dates, d)
      
      
    }
    
    dates <- as.Date(dates, origin = "1970-01-01")
    
    # transpose and keep values until the end of bins
    rr <- t(rain)
    
    # keep data within the lenght of bins
    rr <- as.data.frame(rr[seq_along(bins), ])
    
    # split by ids
    rr <- split(rr, bins)
    
    # calculate indices
    ind <- lapply(rr, function(x) {
      
      x <- apply(x, 2, function(y) {
        
        c(.dryspell(y),
          .wetspell(y),
          .r_ten_mm(y),
          .r_twenty_mm(y),
          .r_one_day(y),
          .r_five_day(y),
          .very_wet_days(y),
          .extrem_wet_days(y),
          .r_total(y),
          .sdii(y))
        
      })
      
      x <- data.frame(id    = rep(ids, each = length(index)),
                      index = rep(index, nr), 
                      value = as.vector(x))  
      
    })
    
    ind <- do.call("rbind", ind)
    
    ind$id <- as.integer(ind$id)
    
    ind$index <- as.character(ind$index)
    
    ind <- ind[order(ind$id), ]
    
    ind$date <- dates
    
    ind <- ind[, c("id", "date", "index", "value")]
    
    
  } 
  
  # if no time series required then
  if (isFALSE(timeseries)) {
    
    # split r by rows
    r <- split(rain, seq_len(nr))
    
    ind <- lapply(r, function(x) {
      
      x <- as.vector(as.matrix(x))
      
      x <- data.frame(.dryspell(x),
                      .wetspell(x),
                      .r_ten_mm(x),
                      .r_twenty_mm(x),
                      .r_one_day(x),
                      .r_five_day(x),
                      .very_wet_days(x),
                      .extrem_wet_days(x),
                      .r_total(x),
                      .sdii(x))
      
    })
    
    
    ind <- do.call("rbind", ind)
    
    names(ind) <- index
    
  }
  
  ind <- as.data.frame(ind, stringAsFactor = FALSE)
  
  rownames(ind) <- seq_along(ind[, 1])
  
  class(ind) <- union("clima_df", class(ind))
  
  return(ind)
  
}

#' Maximum length of consecutive dry days
#' @param object numeric vector
#' @return the MLDS index, which is the maximum length of consecutive dry days
#' precipitation < 1 mm
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.dryspell(r)
#' @noRd
.dryspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of numbers
  # in this case, zeros (0)
  # take the maximum sequency
  # first all values < 1 are converted to zero (0)
  ds <- object[!is.na(object)]
  
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
#' @param object numeric vector
#' @return the MLWS index, which is the maximum length of consecutive wet days
#' precipitation > 1 mm
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.wetspell(r)
#' @noRd
.wetspell <- function(object)
{
  # the function rle is applied
  # which looks for the sequencies of zeros
  # take the maximum sequency
  # first all values >= 1 are converted to zero (0)
  # no precipitation (r < 1) is converted to two (2)
  ws <- object[!is.na(object)]
  
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
#' @param object numeric vector
#' @return the R10mm index, which is number of heavy precipitation days (10 >= r
#'  < 20 mm)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 12)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.r_ten_mm(r)
#' @noRd
.r_ten_mm <- function(object) {
  
  rt <- sum(object >= 10 & object < 20, na.rm = TRUE)
  
  return(rt)
  
}

#' Very heavy precipitation days (r >= 20)
#' @param object numeric vector
#' @return the R20mm index, which is number of very heavy precipitation days (r
#'  >= 20)
#' @examples
#' set.seed(12)
#' r <- runif(20, 10, 23)
#' r[c(1,4,9:11)] <- 0.1
#' chirps:::.r_twenty_mm(r)
#' @noRd
.r_twenty_mm <- function(object) {
  
  rtw <- sum(object >= 20, na.rm = TRUE)
  
  return(rtw)
  
}

#' Simple rainfall intensity index
#' @param object numeric vector
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
#' chirps:::.sdii(rep(0.1, 9))
#' @noRd
.sdii <- function(object) {
  
  # total precipitation
  tp <- sum(object, na.rm = TRUE)
  
  # number of wet days
  wd <- length(object[object >= 1])
  
  #if both zero, then return 0
  if (wd == 0) {
    si <- 0L
  }else{
    si <- tp / wd
  }
  
  return(si)
  
}

#' Compute Rx5day rainfall index
#' @param object numeric vector
#' @return the Rx5day index, which is the maximun sum 
#' of rain in consecutive 5 days
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_five_day(r)
#' @noRd
.r_five_day <- function(object)
{
  
  # this look for the maximum sum of rain in
  # consecutive 5 days
  l <- length(object)
  
  if (l < 5) {
    
    return(0L)
  
  }
  
  r5day <- NULL
  
  for (i in 1:(l-4)){
    
    r5day <- cbind(r5day, sum(object[i:(i + 4)], na.rm = TRUE))
    
  }
  
  r5day <- max(r5day, na.rm = TRUE)
  
  return(r5day)
  
}

#' Maximum 1-day rainfall
#' @param object numeric vector
#' @return the Rx1day index, which is the 1-day rainfall
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_one_day(r)
#' @noRd
.r_one_day <- function(object) {
  
  ro <- max(object, na.rm = TRUE)
  
  return(ro)
  
}

#' Total rainfall (mm) in wet days (r >= 1)
#' @param object numeric vector
#' @return the Rtotal index, which is sum of rainfall (mm) in wet days (r >= 1)
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.r_total(r)
#' @noRd
.r_total <- function(object) {
  
  rt <- object[object >= 1]
  
  rt <- sum(object, na.rm = TRUE)
  
  return(rt)
  
}


#' Very wet days
#' @param object numeric vector
#' @return the R95p index, annual total PRCP when rain > 95th percentile
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.very_wet_days(r)
#' @noRd
.very_wet_days <- function(object) {
  
  q <- stats::quantile(object, probs = seq(0, 1, 0.05), na.rm = TRUE)
  q <- q["95%"]
  
  vwd <- object[object > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}

#' Very wet days
#' @param object numeric vector
#' @return the R95p index, annual total PRCP when rain > 95th percentile
#' @examples
#' set.seed(12)
#' r <- runif(20, 0, 9)
#' r[c(1,4,9:12,17)] <- 0
#' chirps:::.extrem_wet_days(r)
#' @noRd
.extrem_wet_days <- function(object) {
  
  q <- stats::quantile(object, probs = seq(0, 1, 0.01), na.rm = TRUE)
  q <- q["99%"]
  
  vwd <- object[object > q]
  
  vwd <- sum(vwd, na.rm = TRUE)
  
  return(vwd)
  
}
