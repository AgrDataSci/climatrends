
# climatrends

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/climatrends)](https://cran.r-project.org/package=climatrends)
[![cran checks](https://cranchecks.info/badges/worst/climatrends)](https://cran.r-project.org/web/checks/check_results_climatrends.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/climatrends)](https://cran.r-project.org/package=climatrends)
[![Build Status](https://travis-ci.org/agrobioinfoservices/climatrends.svg?branch=master)](https://travis-ci.org/agrobioinfoservices/climatrends)
[![codecov](https://codecov.io/gh/agrobioinfoservices/climatrends/master.svg)](https://codecov.io/github/agrobioinfoservices/climatrends?branch=master)
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

# *climatrends*: Precipitation and temperature indices for climate variability analysis <img align="right" src="man/figures/logo.png">

## Overview

The **climatrends** package provides methods to compute precipitation and temperature indices for climate models in ecology. The indices produced here can be used as explanatory variables for crop modelling, trends in climate change and to assess the interactions of plants and animals with the environment.

## Package website

<https://agrobioinfoservices.github.io/climatrends/>

## Installation

The package may be installed from CRAN via

``` r
install.packages("climatrends")
```

The development version can be installed via

``` r
library("remotes")
install_github("agrobioinfoservices/climatrends", build_vignettes = TRUE)
```

## Example

The default method for the function `temperature()` has as the basic input an `object` of class `data.frame` (or any other that can be coerced to a data.frame) with the longitude and latitude in geographic coordinates and a vector of class `Date` (or any other that can be coerced to `Date`) for the first day that will be taken into account for the indices. The duration from where the temperature indices will be computed is defined either by the argument `last.day` or `span`.

Here we generate some random points within the Innlandet county in Norway from May-2015:


```r
library("climatrends")

set.seed(7291)
lonlat <- data.frame(lon = runif(5, 8.3, 12),
                     lat = runif(5, 60, 62.3))

temp <- temperature(lonlat, day.one = "2015-05-01", last.day = "2015-06-30")

temp

   maxDT minDT maxNT  minNT   DTR    SU    TR   CFD  WSDI  CSDI  T10p  T90p
   <dbl> <dbl> <dbl>  <dbl> <int> <int> <int> <int> <int> <int> <dbl> <dbl>
1: 18.28  2.33  6.79  -5.46     8     0     0    12     5     3 -3.45 15.20
2: 17.49  2.45  6.79  -4.76     8     0     0    12     5     2 -3.18 14.69
3: 19.03  3.58 10.70  -4.58     8     0     0     4     4     4 -1.29 15.71
4: 18.22 -1.69  6.55 -12.42     7     0     0    28     4     4 -4.75 12.59
5: 19.81  4.78 11.48  -3.01     8     0     0     3     4     3  0.73 17.26

```

The indices can be splitted for in intervals for series analysis. Here we get the temperature indices for the same area with intervals of 7 days after `day.one`.

```r
temp2 <- temperature(lonlat,
                     day.one = "2015-05-01", 
                     last.day = "2015-06-30", 
                     timeseries = TRUE, 
                     intervals = 7)

        id       date index  value
     <int>     <date> <chr>  <dbl>
1:       1 2015-05-01 maxDT   7.60
2:       1 2015-05-01 minDT   3.01
3:       1 2015-05-01 maxNT   1.47
4:       1 2015-05-01 minNT  -4.18
5:       1 2015-05-01   DTR   5.85
---                               
476:     5 2015-06-19   CFD   0.00
477:     5 2015-06-19  WSDI   1.00
478:     5 2015-06-19  CSDI   1.00
479:     5 2015-06-19  T10p   6.07
480:     5 2015-06-19  T90p  17.93
```

## Going further

The full functionality of **climatrends** is illustrated in the package vignette. The vignette can be found on the [package website](https://agrobioinfoservices.github.io/climatrends/) or from within `R` once the package has been installed, e.g. via

``` r
vignette("Overview", package = "climatrends")
```

## Meta

  - Package [website](https://agrobioinfoservices.github.io/climatrends/)
  
  - Please [report any issues or bugs](https://github.com/agrobioinfoservices/climatrends/issues).

  - License: [MIT](https://opensource.org/licenses/MIT)

  - Get citation information for *climatrends* in R by typing `citation(package = "climatrends")`.

  - You are welcome to contribute to the *climatrends* project. Please read our [contribution guide lines](CONTRIBUTING.md).

  - Please note that the *climatrends* project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in the *climatrends* project you agree to abide by its terms.
