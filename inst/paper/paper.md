---
title: 'Climate variability indices for ecological and crop models in R: the `climatrends` package'
tags:
- climate data
- climatology
- earth science
- precipitation data
- R
- reproducibility
- weather data
authors:
  - name: Kauê de Sousa
    orcid: 0000-0002-7571-7845
    affiliation: "1, 2"
  - name: Jacob van Etten
    orcid: 0000-0001-7554-2558
    affiliation: 2
  - name: Magne Neby
    orcid: 0000-0003-2098-7650
    affiliation: "1"
  - name: Svein Ø. Solberg
    orcid: 0000-0002-4491-4483
    affiliation: 1
affiliations:
  - name: Department of Agricultural Sciences, Inland Norway University of Applied Sciences, Hamar, Norway
    index: 1
  - name: Digital Inclusion, Bioversity International, Montpellier, France
    index: 2
citation_author: de Sousa et. al.
year: 2023
bibliography: paper.bib
output: rticles::joss_article
journal: JOSS
---

# Summary

Abiotic factors play an important role in most ecological and crop systems that depend on certain levels of temperature, light and precipitation to initiate important physiological events [@PlantEcology]. Understanding how these factors drive the physiological processes is a key approach to provide recommendations for adaptation and biodiversity conservation in applied ecology studies. The package `climatrends` aims to provide the methods in R [@RCoreTeam] to compute precipitation and temperature indices that serve as input for climate and crop models [@vanEtten2019; @Kehel2016], trends in climate change [@Aguilar2005; @deSousa2018] and applied ecology [@Prentice1992; @YLiu2018].

# Statement of need

Reproducibility, the ability to repeat the analysis, and replicability, the ability to repeat an experiment [@Stevens2017], are key to perform collaborative scientific research [@Powers2019; @Munafo2017]. It allows scientists to re-perform analysis after a long hiatus and the peers to validate analysis and get new insights using original or new data. This is still a gap in most of the studies in agriculture and ecology. `climatrends` addresses this specific issue. The package originates from a set of scripts to compute climate indices in our previous studies [@deSousa2018; @vanEtten2019]. Building up on the interest in expanding the analysis to other regions and to enable reproducible and replicable studies among different research groups within the CGIAR (https://www.cgiar.org) and partner institutions we developed `climatrends`. Most of the package functions take into account the heterogeneity of testing sites (locations), dates and seasons, a common characteristic of decentralized agricultural trials [@vanEtten2019]. Further development was made to enable time series analysis with fixed periods of time and locations [@deSousa2018]. The package `climatrends` computes temperature, precipitation, crop growing and crop stress indices that were validated by previous studies on climatology and crop science [@Kehel2016; @Aguilar2005; @Challinor2016]. The indices are described in the package's documentation. `climatrends` was released on CRAN in February 2020 and was the first package to provide methods for agroclimatic indices in R, being able to deal with site heterogeneity and time series analysis. `climatrends` is also unique in its integration with API Client data like NASA POWER [@Sparks2018], CHIRPS [@Funk2015; @chirps] and AgERA5 [@Hersbach2020; @Brown2023]. Other packages like `pollen` [@pollen] and `cropgrowdays` [@cropgrowdays] are limited to growing degree days, whereas the package `agroclim` [@agroclim] provides temperature and precipitation indices but is limited to zoning areas. Currently `climatrends` belongs to the CRAN Task View in Agriculture (https://cran.r-project.org/web/views/Agriculture.html) and is complementary to `agroclim` and `cropgrowdays`.

The package contains eight main functions (Table 1), with a default method for numeric 'vector' and additional methods implemented via the package `methods` [@RCoreTeam] for classes 'matrix' (or array), 'data.frame', and 'sf' (of geometry POINT or POLYGON) [@sf]. The last two methods are designed to fetch data using API Client, currently from the packages `nasapower` [@Sparks2018] and `chirps` [@chirps]. 

Table 1: Main functions available in climatrends.

| **Function**  | **Definition**  |
| --------- |----------------------|
| cumdrought() | Returns a vector with the cumulative sum of the maximum length of dry spell (MLDS) |
| cumrain() | Returns a vector with the cumulative sum of the maximum length of wet spell (MLWS) |
| crop_sensitive() | Compute crop sensitive indices |
| ETo() | Reference evapotranspiration using the Blaney-Criddle method|
| GDD() | Compute growing degree-days|
| late_frost() | Compute the occurrence of late-spring frost|
| rainfall() | Precipitation indices|
| temperature() | Temperature indices|

# Application: a case study with common bean on-farm trials 

During five growing seasons (from 2015 to 2017) in Nicaragua, van Etten *et al.* [-@vanEtten2019] conducted a crowdsourced on-farm trial experiment following the tricot approach [@vanEtten2019tricot] testing 11 common bean varieties (*Phaseolus vulgaris* L.) as incomplete blocks of three randomly allocated to 842 blocks. A Plackett–Luce model was used to analyse the data. The model estimates the *worth*, the probability of each variety to outperform all the others in the set [@Turner2020; @Luce1959; @Plackett1975]. Here we reproduce part of this analysis on the calculation and application of the climate indices. The approach here is slightly different because it considers the growing-degree days from planting date to maturity and add new indices to illustrate the package implementation. The data is available as `data("commonbean", package = "climatrends")`.

We estimate the crop phenological stages based on the growing degree-days using the function `GDD()`. For common bean, we define 900 degree-days, from planting date to maturity [@deMedeiros2016]. The input data is an array with the MODIS temperature data [@Wan2015], the vector with planting dates (`cbean$planting_date`), the required amount of degree-days passed to the argument `degree.days` and the character string 'ndays' specifying that the function must return the values as number of days. `GDD()` computes the degree-days for the time series and return the length of the vector where the accumulated gdd reached the pre-defined threshold of 900 degree-days.

The degree-days spanned from 54 to 100 days as shown in Fig. 1a. For simplicity we take the average per season and use this vector to compute the temperature indices. 

```r
library("climatrends")
library("PlackettLuce")
library("tidyverse")

data("commonbean", package = "climatrends")

cbean <- commonbean[[1]]
modis <- commonbean[[2]]

# number of days required to accumulate gdd from planting date to maturity
gdd <- GDD(modis, 
           day.one = cbean$planting_date, 
           degree.days = 900, 
           return.as = "ndays")

# gdd to the cbean data and take the average gdd per season
cbean %<>%  
  mutate(gdd = gdd$gdd) %>% 
  group_by(season) %>% 
  mutate(gdd_season = as.integer(mean(gdd)))
```

To compute the temperature indices we use the function `temperature()`. In van Etten [-@vanEtten2019], a forward variable selection was applied to retain the most representative covariates based on the deviance reduction. This analysis retained the maximum night temperature (maxNT) as the most representative covariate. To illustrate how the Plackett-Luce trees can grow in complexity as we add more indices, we included the summer days (SU, number of days with maximum day temperature > 30 $^\circ C$) together with maxNT.

```r
# temperature indices from planting date to the 
# number of days required to accumulate the gdd in each season
temp <- temperature(modis, 
                    day.one = cbean$planting_date, 
                    span = cbean$gdd_season)

cbean <- cbind(cbean, temp)

# fit a Plackett-Luce tree
plt <- pltree(G ~ maxNT + SU, data = cbean, minsize = 50)

```

Seasonal distribution of maxNT captured for each incomplete block in this experiment is shown in Fig. 1b. The data has a bimodal distribution which is reflected in the splitting value (18.7 $^\circ C$) for the Plackett-Luce trees in Fig. 1c. The upper node splits with 49 summer days (SU). We can interpret these results as that the on-farm performance of common bean varieties is driven by heat accumulation of diurnal temperature above 30 $^\circ C$ (in this case >70% of the growing days) and warmer nights (> 18.7 $^\circ C$).

\begin{figure}

{\centering \includegraphics[width=0.8\linewidth]{cbean}}

\caption{Fig. 1. Application of climatrends functions to support the analysis of decentralized on-farm trial data. (A) Days required to reach 900 growing-degree days from planting date calculated using the function GDD(). (B) Maximum night temperature (°C) distributed across seasons computed using the function temperature(). (C) Plackett-Luce Tree showing the probability that a given variety outperforms the other varieties (axys X) in three different nodes splitted with the summer days (day temperature > 30 °C) and maximum night temperature (°C). Note: the first season (primera, Pr) spans from May to August, the second (postrera, Po) from September to October, and the third (apante, Ap) from November to January.}\label{fig:fig_cbean}
\end{figure}

# Further development

The package can support the integration with other datasets as they become available in `R` via API client packages. In the future, new indices related to the physiology of crops could be implemented. To explore the latest functionalities of `climatrends`, please check the package's updates at CRAN (https://cran.r-project.org/package=climatrends).

# Acknowledgements

This work was supported by The Nordic Joint Committee for Agricultural and Food Research (grant 202100-2817). Additional support was provided by the projects Accelerated Varietal Improvement and Seed Systems in Africa (AVISA, INV-009649) and 1000FARMS (INV-031561) supported by the Bill & Melinda Gates Foundation. The views expressed in this document cannot be taken to reflect the official opinions of these organizations.

# References

<div id="refs"></div>

