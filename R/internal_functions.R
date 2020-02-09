#' Is a tibble object
#' @param object an object to have its class tested
#' @return a logical value where TRUE indicates that the 
#' object is of class "tbl_df"
#' @examples
#' .is_tibble(airquality)
#' @noRd
.is_tibble <- function(object) {
  
  c("tbl_df") %in% class(object)

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
