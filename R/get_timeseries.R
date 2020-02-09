#' Time series environmental data
#' 
#' Concatenate time series environmental data
#' 
#' @param object a numeric vector of geographic coordinates (lonlat) or
#' a matrix with environmental data from other sources. When lonlat is used,
#'  the function makes a call to \code{nasapower} to fetch and combine 
#'  enviromental data from NASA POWER 
#'  (\url{https://power.larc.nasa.gov/})
#' @param day.one a vector of class 'Date' for the starting date to capture the
#'  environmental data
#' @param span an integer or a vector with integers for the duration of the 
#'  time series to be captured
#' @param days.before optional, an integer for the number of days before 
#'  day.one to be included in the timespan
#' @param ... additional arguments passed to \code{\link[nasapower]{get_power}}
#' @return a data frame of time series environmental data for the chosen period
#' @family climatology functions
#' @examples 
#' set.seed(123)
#' object <- data.frame(lon = runif(1, 11, 12),
#'                      lat = runif(1, 55, 58))
#' 
#' day.one <- as.Date(17667, origin = "1970-01-01")
#' 
#' span <- 50
#' 
#' days.before <- 0
#' 
#' pars = "PRECTOT"
#' 
#' .get_timeseries(object, day.one, span, days.before, pars = "PRECTOT")
#' 
#' @importFrom nasapower get_power
#' @importFrom tibble as_tibble
#' @noRd
.get_timeseries <- function(object, day.one = NULL,
                            span = 150, days.before = 0, ...)
{
  
  if (is.null(day.one)) {
    stop("Argument 'day.one' is missing with no default \n")
  }
  
  if (is.matrix(object)) {
    object <- as.data.frame(object)
  }
  
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
  
  # look if nasapower is required 
  nasa_power <- dim(object)[[2]] == 2
  
  # then get data from NASA POWER using nasapower
  if (nasa_power) {
   
    # the first and last date to fetch
    dates <- c(min(b), maxend)
    
    # define geographic boundaries for lonlat
    lims <- with(object, c(floor(min(object[,1])), 
                           floor(min(object[,2])),
                           ceiling(max(object[,1])), 
                           ceiling(max(object[,2]))))
    
    # get NASA POWER
    info <- nasapower::get_power(community = "AG",
                                 lonlat = lims,
                                 dates = dates,
                                 temporal_average = "DAILY", 
                                 ...)
    
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
    
    n <- dim(object)[[1]]
    
    # split lonlat into a list by its rows
    xy1 <- split(object, seq_len(n))
    
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
    data <- lapply(info, function(n) {
      n <- n[nn, "value"]
      n
    })
    
    # combine vectors in this list 
    data <- do.call("cbind", data)
    
    object <- as.data.frame(data)
    
    names(object) <- as.character(names(info))
     
  }
  
  # # check if provided start.date exists within the object
  days <- names(object)
  
  n <- dim(object)[[1]]
  rownames(object) <- seq_len(n)
  
  # find the index for specified dates within the start.dates provided
  date <- match(as.character(b), days)
  
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





