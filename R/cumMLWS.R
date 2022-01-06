#' Cumulative sum of rainfall
#' 
#' Returns a vector with the cumulative sum of 
#'  the maximum length of wet spell (MLWS)
#'  
#' @param object a numeric vector
#' @return a vector with the cumulative sum of MLWS
#' @examples 
#' 
#' # Example 1
#' 
#' rain <- c(0,0.2,1.4,6.1,1.4,5.1,1.5,1.6,0.1,0,7,6,4,6,1.1,1.2,1.5,0)
#' 
#' cumMLWS(rain)
#' 
#' # should return this vector
#' # raincum <- c(0,0,1,2,3,4,5,6,6,6,6,6,6,6,6,6,7,7)
#' 
#' # Example 2
#' 
#' rain2 <- c(1,0,1,1,0,0,1,1,1,0,0,1,1)
#' 
#' cumMLWS(rain2)
#' 
#' # should return this 
#' # raincum2 <- c(1,1,1,2,2,2,2,2,3,3,3,3,3)
#' 
#' @export
cumMLWS <- function(object){
  
  # values higher equal or higher than 1 are coded as 1
  # lower than 1 are coded as 0
  r <- ifelse(object >= 1, 1, 0)
  
  # get the list with length encoding showing 
  # the values with 0s and 1s to build the vector
  len <- rle(r)
  
  # now start the vector by making the first 
  # cumulative values
  result <- integer()
  
  # check if the first sequence is with 1s or 0s
  zero <- which(len$values == 0)[1]
  one  <- which(len$values == 1)[1]
  
  # if it starts with 0 then 
  # start by a sequence with 0s until 
  # the 1s starts and then do a 
  # cumulative sequence until the third sequence starts
  if(zero == 1) {
    result <- rep(0, len$lengths[1])
    result <- c(result, seq_len(len$lengths[2]))
  }
  
  # if starts with 1 then add the cumulative 
  # and then repeats the last value until 
  # the third sequence starts
  if(one == 1) {
    result <- seq_len(len$lengths[1])
    result <- c(result, 
                rep(result[length(result)], seq_len(len$lengths[2])))
  }
  
  result
  
  # if only two sequences then return the result 
  if (length(len$lengths) == 2) {
    return(result)
  }
  
  # or continue with the sequence
  # remove the first two sequences
  len <- lapply(len, function(x){
    x <- x[3:length(x)]
  })
  
  
  for(i in seq_along(len$lengths)){
    
    if (len$values[i] == 0) {
      result <- c(result, rep(result[length(result)], len$lengths[i]))
    }
    
    if (len$values[i] == 1 & len$lengths[i] > result[length(result)]) {
      result <- c(result, rep(result[length(result)], len$lengths[i] -1))
      result <- c(result, len$lengths[i])
    }
    
    if (len$values[i] == 1 & len$lengths[i] < result[length(result)]) {
      result <- c(result, rep(result[length(result)], len$lengths[i]))
    }
    
  }
  
  return(result)
  
}



