#' Growing degree-days
#' 
#' Compute Growing degree-days. This a heuristic tool in phenology 
#' that measures heat accumulation and is used to predict plant and animal 
#' development rates. Growing degree-days are calculated by taking the 
#' integral of warmth above a base temperature.
#' 
#' @inheritParams temperature
#' @param base an integer for the minimum temperature for growth (as per the physiology 
#'  of the focal organism)
#' @param degree.days an integer for the accumulated degree-days required by the 
#'  organism (as per the physiology of the focal organism). Optional if 
#'  \var{return.as} = \code{"gdd"}
#' @param equation character to specify the equation to be used, one of \code{"default"},
#' \code{"variant_a"} or \code{"variant_b"}. See details 
#' @param return.as character (one of \code{"ndays"} the default or \code{"gdd"})
#'  to select if the function returns the number of days to reach the accumulated 
#'  \var{degree.days} or the daily values of the GDD
#' @return 
#'  The number of days to reach the accumulated \var{degree.days} or the daily degree-days 
#'   as defined with the argument \var{return.as}
#' @family temperature functions
#' @details 
#' The \code{array} method assumes that \var{object} contains climate data provided 
#'  from a local source; this requires an array with two dimensions, 1st dimension 
#'  contains the day temperature and 2nd dimension the night temperature, 
#'  see help("modis", "climatrends") for an example on input structure.
#' 
#' The \code{default} method and the \code{sf} method assumes that the climate data
#'  will e fetched from a remote (cloud) source that be adjusted using the argument 
#'  \var{data.from}.
#'  
#' The \code{"default"} \var{equation} uses the average of the daily maximum (Tmax) and 
#'  minimum (Tmin) temperatures compared to a \var{base} temperature (Tbase). If Tmin 
#'  is below Tbase there are two variants: 
#'  
#'  \code{"variant_a"}: set Tmean = Tbase if (Tmax + Tmin) / 2 < Tbase
#'  
#'  \code{"variant_b"}: set Tmin = Tbase if Tmin < Tbase
#'
#' Additional arguments:
#' 
#' \code{last.day}: an object (optional to \var{span}) of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD)  for the last day of the time series
#'  
#' \code{span}: an integer (optional to \var{last.day}) or a vector with 
#'  integers (optional if \var{last.day} is given) for the length of 
#'  the time series to be captured
#' 
#' \code{data.from}: character for the source of remote data. Current remote source 
#'  is: 'nasapower'
#' 
#' \code{pars}: character vector for the temperature data to be fetched. If 
#'  \code{data.from} is 'nasapower'. The temperature can be adjusted to 2 m, the default,
#'  c("T2M_MAX", "T2M_MIN") or 10 m c("T10M_MAX", "T10M_MIN") 
#' 
#' @references 
#' Prentice I. C., et al. (1992) Journal of Biogeography, 19(2), 117. 
#' \cr\url{https://doi.org/10.2307/2845499}
#' 
#' @examples
#' # Using local data
#' data("modis", package = "climatrends")
#' 
#' GDD(modis,
#'     day.one = "2013-10-28",
#'     base = 10,
#'     degree.days = 100)
#' 
#' # change the equation to variant_b where 
#' # Tmin = Tbase if (Tmax + Tmin) / 2 < Tbase
#' GDD(modis,
#'     day.one = "2013-10-28",
#'     base = 10,
#'     degree.days = 100,
#'     equation = "variant_b")
#' 
#' # return as degree days
#' GDD(modis,
#'     day.one = "2013-10-28",
#'     base = 10,
#'     degree.days = 100,
#'     return.as = "gdd")
#' 
#' \donttest{
#' # Using remote sources and sf method
#' data("lonlatsf", package = "climatrends")
#' 
#' # compute number of days from "2019-04-01" to reach accumated GDD 
#' # for Acer platanoides that begins to flowering at 45 GDD
#' # set equation to variant_a where Tmean = Tbase if (Tmax + Tmin) / 2 < Tbase
#' GDD(lonlatsf,
#'     day.one = "2019-04-01",
#'     last.day = "2019-06-30",
#'     base = 5,
#'     degree.days = 45,
#'     equation = "variant_a")
#' }
#' @export
GDD <- function(object, ..., base = 10)
{
  UseMethod("GDD")
}

#' @rdname GDD
#' @method GDD default
#' @export
GDD.default <- function(object, day.one, base = 10, 
                        degree.days = NULL,
                        equation = "default",
                        return.as = "ndays", ...){
  
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
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  result <- .gdd(day, night, base, degree.days, equation, return.as)
  
  return(result)
}


#' @rdname GDD
#' @method GDD array
#' @export
GDD.array <- function(object, day.one, base = 10, 
                      degree.days = NULL, 
                      equation = "default", 
                      return.as = "ndays", ...){
  
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
  
  result <- .gdd(day, night, base, degree.days, equation, return.as)
  
  return(result)
}


#' @rdname GDD
#' @method GDD sf
#' @export
GDD.sf <- function(object, day.one, base = 10, 
                   degree.days = NULL, 
                   equation = "default",
                   return.as = "ndays", 
                   as.sf = TRUE, ...){
  
  dots <- list(...)
  pars <- dots[["pars"]]
  
  day.one <- as.data.frame(day.one)[, 1]
  
  if (is.null(pars)) {
    pars <- c("T2M_MAX", "T2M_MIN")
  }
  
  dat <- get_timeseries(object, day.one, pars = pars, ...)
  
  day <- dat[[pars[[1]]]]
  
  night <- dat[[pars[[2]]]]
  
  result <- .gdd(day, night, base, degree.days, equation, return.as)
  
  if (isTRUE(as.sf)) {
    result <- suppressWarnings(sf::st_bind_cols(object, result))
  }
  
  return(result)
}



#' @rdname GDD
#' @method GDD clima_ls
#' @export
GDD.clima_ls <- function(object, 
                         base = 10, 
                         degree.days = NULL, 
                         equation = "default",
                         return.as = "ndays", ...){
  
  day <- object[[1]]
  night <- object[[2]]
  
  result <- .gdd(day, night, base, degree.days, equation, return.as)
  
  return(result)
  
}

.gdd <- function(day, night, base, degree.days, equation = "default", return.as = "ndays"){
  
  if (all(return.as == "ndays", is.null(degree.days))) {
    stop("argument degree.days is missing with no default \n")
  }
  
  temp <- cbind(day, tmin = night$value)
  
  temp <- split(temp, temp$id)
  
  Y <- lapply(temp, function(x){
    
    tmax <- x$value
    tmin <- x$tmin
    
    
    if (isTRUE(equation == "default")) {
      
      y <- ((tmax + tmin) / 2) - base
    
    }
    
    if (isTRUE(equation == "variant_a")) {
      # adjust Tmean if Tmean < base
      tmean <- (tmax + tmin) / 2
      
      tadj <- ifelse(tmean < base, base, tmean)
      
      y <- tadj - base
      
    }
    
    if (isTRUE(equation == "variant_b")) {
      # set Tmin = base if Tmin < base
      tadj <- ifelse(tmin < base, base, tmin)
      
      y <- ((tmax + tadj) / 2) - base
      
    }
    
    if (isTRUE(return.as == "ndays")) {
      
      y <- y[!is.na(y)]
      
      # sum temperature values until reach the defined degree days
      for (d in seq_along(y)) {
        
        i <- d
        
        if (sum(y[1:d], na.rm = TRUE) >= degree.days) {break}
      }
      
      return(i)
      
    }
    
    if (isTRUE(return.as == "gdd")) {
      
      y <- data.frame(id = x$id,
                      date = x$date,
                      gdd = y)
      
      return(y)
      
    }
  
  })
  
  result <- do.call("rbind", Y)
  
  if (isTRUE(return.as == "ndays")) {
    
    result <- data.frame(GDD = result, stringsAsFactors = FALSE)
  
  }
  
  rownames(result) <- seq_along(result[,1])
  
  class(result) <- union("clima_df", class(result))
  
  return(result)
  
}
