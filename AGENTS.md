# Instructions for AI agents

climatrends computes climate indices for ecological and crop modelling: temperature and rainfall indices, growing degree-days, crop-sensitive heat indices, late frost and reference evapotranspiration. It works on local data (vectors, matrices, arrays) or fetches daily data from NASA POWER for coordinates. In tricot trials it is used to describe the environment of each plot between planting and harvest. Find out whether the user wants to **compute indices for their data** or to **change the package**.

## Before you write any code

1. Check the installed version with `packageVersion("climatrends")` and read the help of every function you use. Arguments change between versions, so do not rely on memory.
2. Most arguments go through `...` (for example `span`, `last.day`, `intervals`, `pars`, `equation`, `return.as`, thresholds). Read the Details section of the help page to find them.
3. For methods and evidence on tricot, read the knowledge base at https://github.com/AgrDataSci/tricot-knowledge-base (start with its `AGENTS.md`).

## Which function for which task

| Task | Function |
|---|---|
| Temperature indices (max and min day and night temperature, DTR, summer days, tropical nights, frost, warm and cold spells) | `temperature()` |
| Rainfall indices (dry and wet spells, heavy rain days, Rx1day, Rx5day, R95p, R99p, total, SDII) | `rainfall()` |
| Growing degree-days, daily, accumulated, or days to reach a sum | `GDD()` |
| Heat stress and crop duration indices around a phenological stage | `crop_sensitive()` |
| Late spring frost events | `late_frost()` |
| Reference evapotranspiration (Blaney-Criddle) | `ETo()` |
| Raw daily series from NASA POWER or from local matrices | `get_timeseries()` |
| Running count of wet or dry days | `cumrain()`, `cumdrought()` |

Each function has methods for numeric vectors (local data), `matrix` or `array` (local data with dates as column names), `data.frame` of longitude and latitude, and `sf` objects (both fetch NASA POWER). Rainfall from CHIRPS is fetched with the chirps package and passed as a matrix.

## Definitions to state when reporting results

- Thresholds follow the package documentation, not always ETCCDI. For example, summer days use tmax > 30 °C and tropical nights tmin > 25 °C.
- Percentile indices (R95p, R99p, WSDI, CSDI, T10p, T90p) use the percentiles of the series itself, not of a reference period.
- `GDD(return.as = "ndays")` returns the number of days whose accumulated GDD does not exceed `degree.days`.
- NASA POWER is a gridded product (about 0.5 degrees). Say so when values describe a plot.

## Personal data

Trial coordinates can identify farmers. Never put real coordinates of participants in examples, tests or issues. Use the example data in the package or simulated points.

## Changing the package

- Use `=` for assignment in new code. Use base R where possible.
- Document with roxygen and run `devtools::document()`. Never edit files in `man/` by hand.
- Every fix needs a test in `tests/testthat/` that fails before the fix and passes after it. Prefer known-answer tests on small vectors; tests that call NASA POWER must use `skip_on_cran()` and `skip_if_offline()`. Run `devtools::test()` and `devtools::check()`, and report what was actually run.
- Record changes in `NEWS.md`, and say when results change.
- gosset and ClimMob-analysis (https://github.com/AgrDataSci/ClimMob-analysis) use this package. Check them when changing what a function returns.
- Work on a branch and open a pull request. Do not merge without review and passing checks.

## Where to find more

- Reference and vignettes: https://agrdatasci.github.io/climatrends/
- Issues: https://github.com/AgrDataSci/climatrends/issues
