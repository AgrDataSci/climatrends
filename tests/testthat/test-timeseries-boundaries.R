testthat::test_that("staggered windows use the actual last requested day", {
  starts = as.Date(c("2020-01-01", "2020-01-20"))
  x = climatrends:::.st_span(day.one = starts, span = c(40L, 2L))
  testthat::expect_equal(x$dates[1], as.Date("2020-01-01"))
  testthat::expect_equal(x$dates[2], as.Date("2020-02-09"))
  testthat::expect_equal(x$maxend, max(x$end))
  testthat::expect_equal(x$span, c(40L, 2L))
})

testthat::test_that("days.before does not truncate requested dates", {
  x = climatrends:::.st_span(day.one = as.Date("2020-03-01"),
                            span = 3L, days.before = 2L)
  testthat::expect_equal(x$dates, as.Date(c("2020-02-28", "2020-03-03")))
  testthat::expect_equal(x$span, 5L)
})

testthat::test_that("regional NASA POWER values match a point request", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("power.larc.nasa.gov")
  testthat::skip_if_not_installed("nasapower")
  xy = data.frame(lon = c(-84.1, -83.6, 36.8), lat = c(9.9, 10.2, -1.3))
  dates = c("2019-03-01", "2019-03-10")
  r = climatrends:::.nasapower(dates = dates, lonlat = xy,
                               pars = c("T2M_MAX", "PRECTOTCORR"))
  testthat::expect_equal(names(r), c("T2M_MAX", "PRECTOTCORR"))
  testthat::expect_equal(dim(r$T2M_MAX), c(3L, 10L))
  testthat::expect_equal(names(r$T2M_MAX)[1], "2019-03-01")
  for (k in c(1, 3)) {
    pt = as.data.frame(nasapower::get_power(community = "ag",
                                            lonlat = c(xy$lon[k], xy$lat[k]),
                                            pars = "T2M_MAX",
                                            dates = dates,
                                            temporal_api = "daily"))
    testthat::expect_equal(as.numeric(r$T2M_MAX[k, ]), pt$T2M_MAX,
                           tolerance = 0.05)
  }
})
