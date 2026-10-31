climatrends 1.3.0 (2026-10-31)
=========================

This release fixes the NASA POWER requests for `sf` objects and several
indices. Some results change. Check the items under CHANGES IN RESULTS
before comparing with analyses run on earlier versions.

### CHANGES IN RESULTS

* `rainfall()` computes SDII as the rain in wet days (>= 1 mm) divided by
  the number of wet days, as in Aguilar et al. (2005). Earlier versions
  divided the total rain, including days below 1 mm, by the number of wet
  days, which overestimated SDII.
* `temperature()` returns DTR (diurnal temperature range) as a decimal
  number. Earlier versions truncated it to an integer when
  `timeseries = FALSE`.
* `crop_sensitive()` reports `hts_*`, `hse_*` and `lethal_*` as the
  proportion of days with data. Earlier versions divided by all days,
  including days with missing data. The documentation now says proportion
  (0 to 1) instead of percentage.
* `crop_sensitive()` assigns `cdi_mean` to the daily mean temperature and
  `cdi_max` to the daily maximum temperature (#19, #20). The two were
  swapped before.
* The `array` methods of `GDD()`, `ETo()` and `late_frost()` include the
  last day of the data when neither `span` nor `last.day` is given. Earlier
  versions dropped it.

### BUG FIXES

* Data from NASA POWER are requested one parameter at a time and in
  windows of at most 365 days, as the regional API requires. Methods that
  fetch two parameters at once, such as `temperature()`, `GDD()`, `ETo()`,
  `crop_sensitive()` and `late_frost()` for `sf` objects, failed before.
  Values are matched to dates and grid cells by name, so a parameter whose
  name is part of another (for example `T2M` and `T2M_MAX`) is no longer
  mixed up. Missing values coded as -999 are returned as `NA`.
* `rainfall()` for `sf` objects uses "PRECTOTCORR" by default. The previous
  default, "PRECTOT", is no longer offered by NASA POWER.
* Coordinates of `sf` objects are read with 'sf' by namespace, so 'sf' no
  longer needs to be attached. Polygon centroids are computed on the
  geometry only; other columns of the object were mixed into the
  coordinates before.
* `GDD()` with `equation = "c"` stops with a clear message when
  `tbase_max` is missing. The documentation of equation "c" now says that
  tmax is capped at `tbase_max`.
* `rainfall()` and `temperature()` with `timeseries = TRUE` stop with a
  clear message when `intervals` is missing or longer than the series.
* `cumrain()` and `cumdrought()` return a vector of the same length as the
  input when it has a single value.
* `.st_span()` requests NASA POWER data up to the last day actually needed
  when sites have different start dates and spans.

### DOCUMENTATION

* Corrected typos in the help pages and vignettes, the description of
  tropical nights (minimum temperature), of equation "c" in `GDD()`, of the
  `hse` and `hse_ms` indices, and of the `*.threshold` arguments. The
  Overview vignette passes `tbase` to `late_frost()` (it passed `base`,
  which was ignored). The README example uses the right `dates` column.

### TESTS

* Tests now run under `R CMD check` (they were excluded in
  `.Rbuildignore`), with new known-answer tests that do not need the
  internet.
* Added an R CMD check workflow for Linux, macOS and Windows, and
  `AGENTS.md`, a guide for AI agents working on the package.

climatrends 1.1 (2025-11-19)
=========================

* Fixes an issue in `temperature.data.frame()` and `ETo.data.frame()` in requesting data from nasapower as the API cannot handle more than one parameter (TMAX, TMIN)

climatrends 0.4 (2022-09-26)
=========================

* Fix issues in CRAN cmd checks 

climatrends 0.2 (2022-02-14)
=========================

* Fix an issue in computing large areas with nasapower
* Add paper

climatrends 0.1.7 (2020-05-22)
=========================

* Add suggested CITATION
* Fix documentation typos
* Fix a bug in computing equation 'c' in `GDD()`
* Improve speed of functions `GDD()` and `late_frost()`
* package 'sf' is a suggestion not dependency

climatrends 0.1.6 (2020-05-19)
=========================

### NEW FEATURES

* New function `crop_sensitive()` to compute crop sensitive indices.
* New function `late_frost()` to compute late spring frost.
* Export function `get_timeseries()`
* Enable future modular expansion of data sources using API clients by creating specific functions that are called with the argument `data.from` in `get_timeseries()`
* Enable argument `last.day`, optional to `span`, when indices are retrieved with focus on time series analysis
* Enable data retrieve from large areas in 'nasapower' by clustering the lonlat points with `stats::hclust()`
* Methods for objects of classes 'vector' (default), 'matrix', 'array', 'data.frame' and 'sf'. The last two with focus in retrieve data from external sources.
* Remove dependency from 'tibble' which was basically to provide a 'cool' printing method
* Implement a `print()` method for objects that inherits the class 'clima_df'
* Calls from "nasapower" can be adjusted with argument `pars`, mainly for `temperature()`, `GDD()` and `ETo()`. Details in the function documentation
* New `temperature()` indices provided. Details in the function documentation
* Function `GDD()` applies two equations adjustments for cold zones and one for warm zones with the argument `equation`. Details in the function documentation
* Function `GDD()` now allows to return the raw daily gdd or number of days to reach the accumulated gdd (the default in previous versions) using the argument `return.as`

### CHANGES IN BEHAVIOUR

* New default method in all functions which uses a vector as main input
* Change order of `GDD()` arguments. Previous versions used `GDD(object, day.one = NULL, degree.days = NULL, base = 10, span = 150, ...)`. From now on `GDD()` default behaviour is `GDD(object, ..., base = 10)`, with additional arguments are passed through `...` in each defined method. The function return by default the accumulated degree-days across the series.

climatrends 0.1.0 (2020-02-20)
=========================
* Add examples of data set from local sources `modis()` and `chirp()`
* Add details for the utilisation of data set daylight to compute the percentage of daylight based on lat and month

climatrends 0.0.3 (2020-02-11)
=========================
* Fix mis-spelled words identified in CRAN checks
* Update references in function(s) description
* Update `R` to >= v3.5.0

climatrends 0.0.2 (2020-02-10)
=========================

* Remove non-ASCII characters from Description file
* Submit to CRAN

climatrends 0.0.1 (2020-02-08)
=========================

* Migrated functions from **gosset**