#' Is a Date object
#' @param object an object to have its class tested
#' @return a logical value where TRUE indicates that the 
#' object is of class "Date"
#' @examples
#' x <- as.Date(c(1000, 2000), origin = "1970-01-01")
#' .is_Date(x)
#' @noRd
.is_Date <- function(object) {
  
  c("Date") %in% class(object)

}

#' Round to the nearest base value
#' @param x a vector of class numeric
#' @param a number for the base value to round
#' @return a vector of class numeric with rounded number to its near base
#' @examples 
#' x <- c(1.6,0.3, 0.2, 2)
#' .round5(x, 0.5)
#' @noRd
.round5 <- function(x, base.value) {
  
  base.value * round( x / base.value )
  
}

#' Nearest neighbour
#' Check the nearest point using the Eucledian method
#' @param xy1 a numeric vector indicating one single coordinate lonlat
#' @param xy2 a data.frame with coordinates lonlat
#' @return the index in nrow 'xy2' for the nearest point to 'xy1'
#' @examples
#' lonlat1 <- c(11.572197, 57.57921)
#' 
#' lonlat2 <- data.frame(lon = runif(10, 11, 12),
#'                       lat = runif(10, 55, 58))
#' 
#' nn <- .nearest(lonlat1, lonlat2)
#' 
#' lonlat2[nn, ]
#' @noRd
.nearest <- function(xy1, xy2) {
  
  x1 <- xy1[1]
  y1 <- xy1[2]
  
  x2 <- xy2[,1]
  y2 <- xy2[,2]
  
  x <- (x1 - x2)^2
  
  y <- (y1 - y2)^2
  
  xy <- sqrt(x + y)
  
  index_xy <- which.min(xy)
  
  return(index_xy)
  
}

#' Get lonlat from a sf object
#' 
#' @param object an object of class sf and geometry "POINT" or "POLYGON"
#' @return a matrix with longitude and latitude and that order
#'  for sf "POLYGON" a centroid within the polygon is returned
#' @examples 
#' library("sf")
#' set.seed(123)
#' lonlat <- data.frame(lon = runif(2, 11, 12),
#'                      lat = runif(2, 55, 58))
#' lonlat <- st_as_sf(lonlat, coords = c("lon","lat"))
#' 
#' .lonlat_from_sf(lonlat)
#' 
#' @noRd
.lonlat_from_sf <- function(object){
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package 'sf' is required for objects of class 'sf'")
  }
  
  geom <- sf::st_geometry(object)
  
  geo <- as.character(sf::st_geometry_type(geom))
  
  if (all(geo == "POINT")) {
    
    xy <- sf::st_coordinates(geom)[, c("X", "Y"), drop = FALSE]
    
  } else if (all(geo %in% c("POLYGON", "MULTIPOLYGON"))) {
    
    # centroid of each polygon, computed on the geometry only so other
    # columns of the object are not mixed with the coordinates
    xy <- sf::st_coordinates(sf::st_centroid(geom))[, c("X", "Y"), drop = FALSE]
    
  } else {
    
    stop("The sf geometry type is not supported. ",
         "Please provide a sf object of geometry type ",
         "'POINT' or 'POLYGON'\n")
    
  }
  
  nr <- dim(xy)[[1]]
  
  lonlat <- matrix(as.numeric(xy),
                   nrow = nr,
                   ncol = 2, 
                   dimnames = list(seq_len(nr), c("lon","lat")))
  
  return(lonlat)
}


#' Coerce to 'Date'
#' Coerce a vector of characters or integers into class 'Date'
#' @param x a vector
#' @examples 
#' # valid entries
#' .coerce2Date(c(819, 9811))
#' 
#' .coerce2Date(c("1970-01-01", "2020-09-01"))
#' 
#' .coerce2Date(as.Date(12, origin = "1970-01-01"))
#' 
#' # some errors
#' .coerce2Date(letters[1:10])
#' 
#' .coerce2Date(c("1970-01-01", "2020-90-01"))
#' 
#' @noRd
.coerce2Date <- function(x) {
  
  if (is.character(x)) {
      
      x <- as.Date(x, format = "%Y-%m-%d")

  }
  
  if (any(is.integer(x), is.numeric(x))) {
    
    x <- as.Date(x, origin = "1970-01-01")
  
  }
  
  if (isFALSE(.is_Date(x))) {
      stop("No visible method to coerce",
           " given dates to as.Date \n")
  }
  
  if (any(is.na(x))) {
    stop("Visible method to coerce given",
         " dates to as.Date returning NAs \n")
  }
  
  return(x)
  
}

