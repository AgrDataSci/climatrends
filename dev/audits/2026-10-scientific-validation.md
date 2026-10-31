# climatrends modernization: initial issues and scientific audit

**Date:** 2026-10-31. **Baseline:** `master` commit `d0297df130fa16f5744d9b6ba18360ab6b0ad999`; package DESCRIPTION version 1.2. **Status:** source/issue review only. No R numerical validation, R CMD check or live API tests have been performed. This is a separate review from the existing contributor's fix.

## GitHub issues

**Currently open:** [#19](https://github.com/AgrDataSci/climatrends/issues/19), an apparent reversed input pairing for `cdi_mean` and `cdi_max` in `crop_sensitive.sf()`. [PR #20](https://github.com/AgrDataSci/climatrends/pull/20) is open and explicitly fixes it. This audit does not duplicate the PR or alter its ownership.

The closed issues include:
- [#18](https://github.com/AgrDataSci/climatrends/issues/18): `get_power` missing in an old vignette. Regression test import and installation compatibility.
- [#16](https://github.com/AgrDataSci/climatrends/issues/16): development-install failure related to `nasapower`. PR #17 previously addressed it.
- [#13](https://github.com/AgrDataSci/climatrends/issues/13): NASA POWER HTTP 422 for date ranges over 366 days. Test that large remote time windows are split reliably, especially across leap years.
- [#3](https://github.com/AgrDataSci/climatrends/issues/3): multi-region NASA POWER sampling. Test longitude/latitude grouping and result-to-site alignment.
- [#1](https://github.com/AgrDataSci/climatrends/issues/1): time-series interval support in rainfall and temperature.
- [#2](https://github.com/AgrDataSci/climatrends/issues/2): requests for additional scientifically justified indices, closed 2026-10-31.
- [#4](https://github.com/AgrDataSci/climatrends/issues/4): CHIRPS integration request, closed 2026-10-31.
- #5-#15 included JOSS editorial and packaging review requests; these do not all represent outstanding defects.

**Important:** a closed issue is not proof the current development and released versions pass its original regression case.

## Initial code-level observations

1. The master `R/crop_sensitive.R` source assigns `.cdi(max, NULL)` to `cdi_mean` and `.cdi(max,min)` to `cdi_max` in the `sf` method. The helper computes a temperature mean when given both max/min. This confirms the mismatch with the documented meanings **in that method**. Review all methods and add independent numerical test vectors before merging #20.
2. `R/get_timeseries.R` builds remote data adapter names with `paste0('.', data.from)` and `do.call()`. Confirm accepted data source names and errors; prevent unsupported or accidental invocation.
3. `.st_span()` calculates remote request bounds from `max(b) + max(span - 1)`; inspect whether the correct end is `max(b + span - 1)` when sites have different planting dates and spans. Overshooting may be inefficient or exceed API limits; unit-test exact bounds.
4. `.st_ts()` temporarily substitutes `-9999` for missing climate observations. Check where this sentinel is restored and whether statistics can accidentally incorporate it, especially where `na.rm` is used.
5. Existing tests include `test-GDD.R`, `test-rainfall.R`, `test-temperature.R` and `test-ETo.R`. The baseline repository tree does not show a `test-crop_sensitive.R` or `test-late_frost.R`. Add offline fixture tests, not only network-dependent examples.
6. `DESCRIPTION` is 1.2 but `NEWS.md` begins with 1.1 (2025-11-19). Align versioning and release notes only after validating behavior.

## Priorities and validation matrix

| Priority | Area | Required validation |
|---|---|---|
| P0 | #19 and PR #20, CDI | Check each input method and threshold; known-answer Tmax/Tmean examples and compare old/new outputs |
| P0 | GDD and ETo | Published equations, thresholds, units, temperature extremes, missingness and date windows |
| P0 | rainfall and temperature | Exact index definitions, annual/seasonal windows, leap years, partial observations, missing-data thresholds |
| P1 | remote NASA POWER | Offline mocked requests, multi-year 366-day chunking, mixed coordinates, API rate/error handling |
| P1 | CHIRPS and external-source boundaries | Confirm actual implementation vs issue #4 closure; avoid duplicating source retrieval from `chirps` |
| P1 | `get_timeseries` | Missing data, site alignment, spans and end dates |
| P2 | late frost and crop stress | Phenological windows, biologically meaningful temperature thresholds and equivalence tests |
| P2 | maintenance | Test coverage, R CMD check across supported R versions, installation and explicit NEWS for 1.2 |

## Suggested change procedure

1. Review and test [PR #20](https://github.com/AgrDataSci/climatrends/pull/20) in its existing branch; **do not merge or replace without numerical checks**.
2. Create deterministic, offline known-answer tests for every exported method and climate index.
3. Distinguish genuine defects from scientifically different definitions (e.g. GDD equation variants).
4. Make limited, backward-compatible changes with failing tests first and record when scientific outputs change.
5. Run `R CMD check`, CRAN comparison and downstream `gosset` / ClimMob workflow tests.
6. Keep this audit PR draft. Only mark it ready once tests and review are complete.

All newly written R examples should use `=` for assignment.
