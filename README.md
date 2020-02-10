
# climatrends

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/climatrends)](https://cran.r-project.org/package=climatrends)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/agrobioinfoservices/climatrends?branch=master&svg=true)](https://ci.appveyor.com/project/kauedesousa/climatrends)
[![Build Status](https://travis-ci.org/agrobioinfoservices/climatrends.svg?branch=master)](https://travis-ci.org/agrobioinfoservices/climatrends)
[![codecov](https://codecov.io/gh/agrobioinfoservices/climatrends/master.svg)](https://codecov.io/github/agrobioinfoservices/climatrends?branch=master)
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

# *climatrends*: Precipitation and temperature extremes for climate variability analysis <img align="right" src="man/figures/logo.png">

## Overview

The **climatrends** package provides the toolkit to compute precipitation and temperature extremes for climate models. The indices produced here can be used as explanatory variables for crop modellinng, trends in climate change and to assess the interactions of plants and animals with the environment.

## Installation

The development version can be installed via

``` r
library("devtools")
devtools::install_github("agrobioinfoservices/climatrends", build_vignettes = TRUE)
```

## Example

The function `temperature()` has as the basic input data an `object` of class `data.frame` with geographic information (lonlat) or an `array` with day and night temperature (when provided locally) and a vector of class `Date` for the first day that will be taken into account for the indices. The duration from where the environmental indices will be computed is defined by the argument `span` which can be a single integer that takes into account a single timespan for all tricot experiments or a vector indicating the timespan for each point.

Here we generate some random points within the Innlandet county in Norway from May-2015:


```r
set.seed(6)
lonlat <- data.frame(lon = runif(5, 8.3, 12),
                     lat = runif(5, 60, 62.3))

date <- as.Date("2015-05-01", format = "%Y-%m-%d")

temp <- temperature(lonlat, day.one = date, span = 50)

head(temp)
#> # A tibble: 5 x 8
#>   maxDT  minDT maxNT  minNT   DTR    SU    TR   CFD
#>   <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  12.8 -0.580  3.61 -10.8   7.36     0     0    13
#> 2  13.4  0.66   3.84  -7.6   7.02     0     0    12
#> 3  10.7 -1.48   2.43 -11.3   7.12     0     0    40
#> 4  14.1  1.53   3.94  -6.85  8.44     0     0    12
#> 5  19.6  5.82   8.24  -2.4   8.23     0     0     1
```

## Going further

The full functionality of **climatrends** is illustrated in the package vignette. The vignette can be found on the [package website](https://agrobioinfoservices.github.io/climatrends/) or from within `R` once the package has been installed, e.g. via

``` r
vignette("Overview", package = "climatrends")
```

## Meta

  - Please [report any issues or bugs](https://github.com/agrobioinfoservices/climatrends/issues).

  - License: MIT

  - Get citation information for *climatrends* in R by typing `citation(package = "climatrends")`.

  - You are welcome to contribute to the *climatrends* project. Please read our [contribution guide lines](CONTRIBUTING.md).

  - Please note that the *climatrends* project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in the *climatrends* project you agree to abide by its terms.
