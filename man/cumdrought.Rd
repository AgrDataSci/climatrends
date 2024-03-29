% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cumrain.R
\name{cumdrought}
\alias{cumdrought}
\title{Cumulative sum of dry days}
\usage{
cumdrought(x)
}
\arguments{
\item{x}{a numeric vector}
}
\value{
a vector with the cumulative sum of MLDS
}
\description{
Returns a vector with the cumulative sum of 
 the maximum length of dry spell (MLDS)
}
\examples{

rain <- c(0,0.2,0.4,0.1,0.4,5.1,1.5,1.6,0.1,0,7,6,4,6,0.1,1.2,0.5,0)

cumdrought(rain)

}
