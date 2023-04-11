
# climatrends

<!-- badges: start -->
[![JOSS status](https://joss.theoj.org/papers/03d54683d5c1d7759519070442ef4500/status.svg)](https://joss.theoj.org/papers/03d54683d5c1d7759519070442ef4500)
[![CRAN status](https://www.r-pkg.org/badges/version/climatrends)](https://cran.r-project.org/package=climatrends)
[![cran checks](https://badges.cranchecks.info/worst/climatrends.svg)](https://cran.r-project.org/web/checks/check_results_climatrends.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/climatrends)](https://cran.r-project.org/package=climatrends)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![DOI](https://zenodo.org/badge/239103060.svg)](https://zenodo.org/badge/latestdoi/239103060)
<!-- badges: end -->

# *climatrends*: Climate Variability Indices for Ecological Modelling <img align="right" src="man/figures/logo.png">

## Overview

The **climatrends** package provides methods to compute precipitation and temperature indices for climate models in ecology. The indices produced here can be used as explanatory variables for ecological modelling, crop modelling, and to assess trends in climate change.


## Statement of need

Reproducibility, the ability to repeat the analysis, and Replicability, the ability to repeat an experiment are key to perform collaborative scientific research. This is still a gap in most of the studies in agriculture and ecology. `climatrends` addresses this specific issue. The package originates from a set of scripts to compute climate indices in our previous studies. Building up on the interest in expanding the analysis to other regions and to enable reproducible and replicable studies we developed `climatrends`. Most of the package functions take into account the heterogeneity of testing sites (locations), dates and seasons, a common characteristic of decentralized agricultural trials. Further development was made to enable time series analysis with fixed periods of time and locations. The package `climatrends` computes temperature, precipitation, crop growing and crop stress indices that were validated by previous studies on climatology and crop science. Currently `climatrends` is part of the CRAN Task View in Agriculture (https://cran.r-project.org/web/views/Agriculture.html).

## Package website

<https://agrdatasci.github.io/climatrends/>

## Installation

The package may be installed from CRAN via

``` r
install.packages("climatrends")
```

The development version can be installed via

``` r
library("remotes")
install_github("agrdatasci/climatrends", build_vignettes = TRUE)
```

## Example

The default method for the function `temperature()` has as the basic input one numeric `vector` with the maximum temperature and one numeric `vector` with the minimum temperature:

```r
library("climatrends")

data("innlandet", package = "climatrends")

temperature(innlandet$tmax, innlandet$tmin)

   maxDT  minDT maxNT  minNT   DTR    SU    TR   CFD  WSDI  CSDI   T10p  T90p
   <dbl>  <dbl> <dbl>  <dbl> <int> <int> <int> <int> <int> <int>  <dbl> <dbl>
1: 15.13 -14.86  6.77 -19.25     6     0     0   115     4     5 -15.81  9.09

```

The indices can be splitted in intervals for series analysis. Here we get the temperature indices with intervals of 30 days.

```r

temperature(innlandet$tmax, innlandet$tmin,
            dates = innlandet$day, 
            timeseries = TRUE, 
            intervals = 30)

       id       date index  value
    <int>     <date> <chr>  <dbl>
1:      1 2019-01-01 maxDT  -0.15
2:      1 2019-01-01 minDT -14.86
3:      1 2019-01-01 maxNT  -3.41
4:      1 2019-01-01 minNT -18.67
5:      1 2019-01-01   DTR   4.35
---                              
68:     1 2019-05-31   CFD   3.00
69:     1 2019-05-31  WSDI   2.00
70:     1 2019-05-31  CSDI   3.00
71:     1 2019-05-31  T10p   0.20
72:     1 2019-05-31  T90p  11.14
```

## Going further

The full functionality of **climatrends** is illustrated in the package vignette. The vignette can be found on the [package website](https://agrdatasci.github.io/climatrends/) or from within `R` once the package has been installed, e.g. via

``` r
vignette("Overview", package = "climatrends")
```

## Meta

  - Package [website](https://agrdatasci.github.io/climatrends/)
  
  - Please [report any issues or bugs](https://github.com/agrdatasci/climatrends/issues).

  - License: [MIT](https://opensource.org/licenses/MIT)

  - Get citation information for *climatrends* in R by typing `citation(package = "climatrends")`.

  - You are welcome to contribute to the *climatrends* project. Please read our [contribution guide lines](CONTRIBUTING.md).

  - Please note that the *climatrends* project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in the *climatrends* project you agree to abide by its terms.
