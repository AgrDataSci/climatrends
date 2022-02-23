#' Cumulative sum of rainfall days
#' 
#' Returns a vector with the cumulative sum of 
#'  the maximum length of wet spell (MLWS)
#'  
#' @param x a numeric vector
#' @return a vector with the cumulative sum of MLWS
#' @examples 
#' 
#' # Example 1
#' 
#' rain <- c(0,0.2,1.4,6.1,1.4,5.1,1.5,1.6,0.1,0,7,6,4,6,1.1,1.2,1.5,0)
#' 
#' cumrain(rain)
#' 
#' # should return this vector
#' # raincum <- c(0,0,1,2,3,4,5,6,6,6,6,6,6,6,6,6,7,7)
#' 
#' # Example 2
#' 
#' rain2 <- c(1,0,1,1,0,0,1,1,1,0,0,1,1)
#' 
#' cumrain(rain2)
#' 
#' # should return this 
#' # raincum2 <- c(1,1,1,2,2,2,2,2,3,3,3,3,3)
#' 
#' @export
cumrain <- function(x){
  
  a <- ifelse(x >= 1, 1, 0)
  for(i in 2:length(a))  a[i] <- ifelse(a[i] == 1, sum(a[i-1], a[i]), 0)
  cummax(a)
  
}

#' Cumulative sum of dry days
#' 
#' Returns a vector with the cumulative sum of 
#'  the maximum length of dry spell (MLDS)
#'  
#' @param x a numeric vector
#' @return a vector with the cumulative sum of MLDS
#' @examples 
#' 
#' rain <- c(0,0.2,0.4,0.1,0.4,5.1,1.5,1.6,0.1,0,7,6,4,6,0.1,1.2,0.5,0)
#' 
#' cumdrought(rain)
#' 
#' @export
cumdrought <- function(x){
  
  a <- ifelse(x < 1, 1, 0)
  for(i in 2:length(a))  a[i] <- ifelse(a[i] == 1, sum(a[i-1], a[i]), 0)
  cummax(a)
  
}

