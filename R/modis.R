#' Example from input data using local sources
#' 
#' Input example from MODIS and CHIRPS data. See details
#' 
#' modis: array with two layers 1) day temperature and 2) night temperature. 
#'  An excerpt of MODIS (MYD11A1) data to represent an example 
#'  of the input data in \code{temperature()} when a local data is provided 
#'  instead of using remote sources (NASA POWER).
#' 
#' chirp: matrix with rainfall temperature from CHIRPS data set. An excerpt to 
#'  represent an example of the imput data in \code{rainfall()} when a local data
#'  is provided instead of using remote sources.
#'  
#' Both sources were extracted using function \code{raster::extract()}. Rows 
#'  represents the coordinates for the given lonlat provided in 
#'  \code{raster::extract()} and cols represents the dates from the observed 
#'  rain/temperature.
#' 
#' @name modis
#' @rdname modis
#' @docType data
#' @aliases chirp
#' @aliases modis
#' @keywords datasets
#' @source 
#' 
#' Funk, C. et al. (2015). The climate hazards infrared precipitation 
#' with stations—a new environmental record for monitoring extremes. 
#' Scientific Data, 2, 150066. \url{https://doi.org/10.1038/sdata.2015.66}
#' 
#' Wan Z, Hook S, Hulley G (2015) MYD11A1 MODIS/Aqua Land Surface 
#' Temperature/Emissivity 8-Day L3 Global 1km
#' SIN Grid V006 \url{http://dx.doi.org/10.5067/MODIS/MYD11A2.006}.
#' 
#' @format 
#' an array with two layers (modis) and matrix (chirp)
c("modis", "chirp")

