---
title: 'Climate variability indices for ecological and crop models in R: the `climatrends` package'
tags:
- climate data
- climatology
- earth science
- evapotranspiration
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
#output:
#  pdf_document:
#    keep_tex: false
#header-includes: \usepackage{caption} \captionsetup[figure]{labelformat=empty}
---

# Summary

Abiotic factors play an important role in most ecological and crop systems that depend on certain levels of temperature, light and precipitation to initiate important physiological events [@PlantEcology]. Understanding how these factors drive the physiological processes is a key approach to provide recommendations for adaptation and biodiversity conservation in applied ecology studies. The package `climatrends` aims to provide the methods in R [@RCoreTeam] to compute precipitation and temperature indices that serve as input for climate and crop models [@vanEtten2019; @Kehel2016], trends in climate change [@Aguilar2005; @deSousa2018] and applied ecology [@Prentice1992; @YLiu2018].

# Statement of need

Six main functions are provided (Table 1), with a default method for numeric 'vector' and additional methods implemented via the package `methods` [@RCoreTeam] for classes 'matrix' (or array), 'data.frame', and 'sf' (of geometry POINT or POLYGON) [@sf]. The last two methods are designed to fetch data from cloud sources, currently from the packages `nasapower` [@Sparks2018] and `chirps` [@chirps]. 

Table 1: Main functions available in climatrends.

| **Function**  | **Definition**  |
| --------- |----------------------|
| crop_sensitive() | Compute crop sensitive indices |
| ETo() | Reference evapotranspiration using the Blaney-Criddle method|
| GDD() | Compute growing degree-days|
| late_frost() | Compute the occurrence of late-spring frost|
| rainfall() | Precipitation indices|
| temperature() | Temperature indices|

These functions started as a set of scripts to compute indices from on-farm testing sites following a citizen science approach [@vanEtten2019]. Aiming to capture the environmental variation across different sites, which can differ as each on-farm trial generally have a different starting day and duration, the arguments `day.one` and `span` are vectorised and may be used to indicate the starting date for each data-point and the duration of the timespan to be considered for the computation of the indices. For time series analysis, fixed periods can be adjusted with the argument `last.day` linked to the argument `day.one`. 

The package `climatrends` computes 12 temperature indices and 10 precipitation indices that were suggested by previous research on climatology and crop science [@Kehel2016; @Aguilar2005]. The indices computed by the functions `temperature()` and `rainfall()` are described in Table 2.

Table 2: Temperature and precipitation indices available in climatrends.

|**Index** | **Definition** | **Unit** |
|------|------------|-------------------|
|maxDT | Maximun day temperature | °C|
|minDT | Minimum day temperature | °C|
|maxNT | Maximun night temperature | °C|
|minNT | Minimum night temperature | °C|
|DTR | Diurnal temperature range | °C|
|SU | Summer days t > 30 °C | days|
|TR | Tropical nights t > 25 °C | days|
|CFD | Consecutive frosty days t < 0 °C | days|
|WSDI | Maximum warm spell duration | days|
|CSDI | Maximum cold spell duration | days|
|T10p | The 10th percentile of night temperature | °C|
|T90p | The 90th percentile of day temperature | °C|
|MLDS | Maximum length of consecutive dry day rain < 1 mm | days|
|MLWS | Maximum length of consecutive wet day rain >= 1 mm | days|
|R10mm | Heavy precipitation days 10 >= rain < 20 mm | days|
|R20mm | Very heavy precipitation days rain >= 20 | days|
|Rx1day | Maximum 1-day precipitation | mm |
|Rx5day | Maximum 5-day precipitation | mm |
|R95p | Total precipitation when rain > 95th percentile | mm |
|R99p | Total precipitation when rain > 99th percentile | mm |
|Rtotal | Total precipitation in wet days, rain >= 1 mm | mm |
|SDII | Simple daily intensity index | mm/days |


# Examples

## Common bean 

During five growing seasons (from 2015 to 2017) in Nicaragua, van Etten *et al.* [-@vanEtten2019] conducted a crowdsourced on-farm trial experiment following the tricot approach [@vanEtten2019tricot] testing 11 common bean varieties (*Phaseolus vulgaris* L.) as incomplete blocks of three randomly allocated to 842 blocks. A Plackett–Luce model was used to analyse the data, this model estimates for each variety the probability that it wins, outperforming all other varieties in the set [@Turner2020]. Here we reproduce part of this analysis on the calculation and application of the climate indices. The approach here is slightly different because it considers the growing-degree days from planting date to maturity and add new indices to illustrate the package implementation. The data is available as `data("commonbean", package = "climatrends")`.

We estimate the crop phenological stages based on the growing degree-days using the function `GDD()`. For common bean, we define 900 degree-days, from planting date to maturity [@deMedeiros2016]. The input data is an array with the MODIS temperature data [@Wan2015], the vector with planting dates (`cbean$planting_date`), the required amount of degree-days passed to the argument `degree.days` and the character string 'ndays' specifying that the function must return the values as number of days. `GDD()` calls internally the function `get_timeseries()` which will search the given dates provided using the argument `day.one` within the column names in the array and concatenate the values for each row. Then `GDD()` computes the degree-days for the time series and return the length of the vector where the accumulated gdd reached the pre-defined threshold of 900 degree-days.

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

To compute the temperature indices we use the array with temperature data, the vector with planting dates, and the seasonal averaged degree-days passed as a vector using the argument `span`. The function `temperature()` concatenates the data from the given `day.one` to the given `span` and computes the indices for each row.

In van Etten [-@vanEtten2019], a forward variable selection was applied to retain the most representative covariates based on the deviance reduction. This analysis retained the maximum night temperature (maxNT) as the most representative covariate. To illustrate how the Plackett-Luce trees can grow in complexity as we add more indices, we included the summer days (SU, number of days with maximum day temperature > 30 $^\circ C$) together with maxNT.


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

\pagebreak 

## Trends in climate variability in Norway and Sweden 

We randomly selected 100 points in hexagonal within the coordinates 7$^\circ$ and 17$^\circ$ W, and 59 $^\circ$ and 63 $^\circ$ N, that comprises Norway and Sweden before the Arctic Circle. We compute the temperature indices from 2000-01-01 to 2019-12-31 using the function `temperature()` with the method for objects of class 'sf'. The temperature data is fetched from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program (<https://power.larc.nasa.gov/>), using the R package `nasapower` [@Sparks2018].


```r
library("climatrends")
library("sf")
library("nasapower")

# create a polygon within the coordinates 7, 17, 59, 63
e <- matrix(c(7, 59, 17, 59, 17, 63,
              7, 63, 7, 59), 
            nrow = 5, ncol = 2, byrow = TRUE)

e <- st_polygon(list(e))

# sample 100 points in the hexagonal type
p <- st_sample(e, 100, type = "hexagonal")
p <- st_as_sf(p, crs = 4326)

# compute the temperature indices using the random points 
temp <- temperature(p, day.one = "2000-01-01", last.day = "2019-12-31", 
                    timeseries = TRUE, intervals = 365)
```

We then select the indices CSDI (cold spell duration of night temperature), WSDI (warm spell duration of day temperature), and their associated indices the T10p (the 10th percentile of night temperature) and T90p (the 90th percentile of day temperature), in Figure 2. Plots are generated with `ggplot2` [@ggplot2] and `patchwork` [@patchwork].

The trends show a decrease in the cold spell duration (number of consecutive cold nights bellow the 10th percentile) and warm spell duration (number of consecutive warm days above the 90th percentile). However, the values of the percentiles show an increase over the time series. The T10p index shows a decrease around the year of 2010, but again rises up to the a value around the -10 $^\circ$C, meaning that the cold nights are becoming a bit warmer over the time. The T90p index also shows an increase in the temperature across the sampled area, with the average 90th percentile rising from ~ 16 $^\circ$C to ~ 18 $^\circ$C over the time series. 

\begin{figure}

{\centering \includegraphics[width=0.8\linewidth]{nordic} 

}

\caption{Fig. 2. Trends in temperature indices across Southern Norway and Sweden from 2000 to 2019. CSDI, maximum cold spell duration, consecutive nights with temperature < 10th percentile. WSDI, maximum warm spell duration, consecutive days with temperature > 90th percentile. T10p, the 10th percentile of night temperature. T90p, the 90th percentile of day temperature. Red line indicates the historical mean of each index in the time series. Blue line indicates the smoothed trends in each index using the 'loess' method.}\label{fig:fig_nordic}
\end{figure}

# Further development

The package can support the integration with other datasets as they become available in `R` via API client packages. In the future, new indices related to the physiology of crops could be implemented. To explore the latest functionalities of `climatrends`, please check the package's updates at CRAN (https://cran.r-project.org/package=climatrends).

# Acknowledgements

This work was supported by The Nordic Joint Committee for Agricultural and Food Research (grant num. 202100-2817). We thank Julian Ramirez-Villegas and Marcel Schrijvers-Gonlag for the useful insights and suggestions that helped in the development of this study.

# References

<div id="refs"></div>

