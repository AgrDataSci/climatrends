# Known-answer tests that do not need the internet

test_that("SDII uses rain in wet days only", {
  x = c(0.5, 0.5, 2, 4, 0, 6)
  r = rainfall(x)
  expect_equal(r$SDII, (2 + 4 + 6) / 3)
  expect_equal(r$Rtotal, 12)
  expect_equal(r$MLWS, 2L)
  expect_equal(r$MLDS, 2L)
})

test_that("DTR is the mean diurnal range, not truncated", {
  tmax = c(20.5, 21.5, 22.5)
  tmin = c(10, 10, 10)
  r = temperature(tmax, tmin)
  expect_equal(r$DTR, mean(tmax - tmin))
})

test_that("timeseries needs valid intervals", {
  x = runif(10)
  expect_error(rainfall(x, timeseries = TRUE), "intervals")
  expect_error(rainfall(x, timeseries = TRUE, intervals = 20), "longer")
  expect_error(temperature(x + 20, x, timeseries = TRUE, intervals = 20), "longer")
})

test_that("GDD equations give the documented values", {
  tmax = c(12, 8, 30)
  tmin = c(2, 1, 20)
  # default: mean - tbase, negatives set to 0
  expect_equal(GDD(tmax, tmin, tbase = 10, return.as = "daily")$gdd,
               c(0, 0, 15))
  # b: tmin and tmax raised to tbase
  expect_equal(GDD(tmax, tmin, tbase = 10, equation = "b",
                   return.as = "daily")$gdd, c(1, 0, 15))
  # c: tmax capped at tbase_max
  expect_equal(GDD(tmax, tmin, tbase = 10, equation = "c", tbase_max = 25,
                   return.as = "daily")$gdd, c(1, 0, 12.5))
  expect_error(GDD(tmax, tmin, tbase = 10, equation = "c"), "tbase_max")
  # accumulated
  expect_equal(GDD(tmax, tmin, tbase = 10, equation = "b")$gdd, c(1, 1, 16))
})

test_that("crop_sensitive proportions ignore missing days", {
  tmax = c(40, 40, NA, 30)
  tmin = c(30, 30, NA, 20)
  r = crop_sensitive(tmax, tmin, hts_max.threshold = 35)
  expect_equal(r$hts_max_35, 2 / 3)
})

test_that("crop duration index uses the right temperature", {
  tmax = c(32, 34, 36, 38)
  tmin = c(12, 14, 16, 18)
  r = crop_sensitive(tmax, tmin, cdi_mean.threshold = 23,
                     cdi_max.threshold = 29)
  expect_equal(r$cdi_mean_23, max(mean((tmax + tmin) / 2) - 23, 0))
  expect_equal(r$cdi_max_29, max(mean(tmax) - 29, 0))
})

test_that("cumrain and cumdrought keep the input length", {
  expect_equal(cumrain(5), 1)
  expect_equal(cumdrought(0), 1)
  rain = c(0,0.2,1.4,6.1,1.4,5.1,1.5,1.6,0.1,0,7,6,4,6,1.1,1.2,1.5,0)
  expect_equal(cumrain(rain), c(0,0,1,2,3,4,5,6,6,6,6,6,6,6,6,6,7,7))
})

test_that("array methods use the last day when no span is given", {
  data("temp_dat", package = "climatrends")
  days = dimnames(temp_dat)[[2]]
  first = days[length(days) - 2]
  g = GDD(temp_dat[1:2, , , drop = FALSE], day.one = first, tbase = 0,
          return.as = "daily")
  # two sites, the last three days each
  expect_equal(nrow(g), 6L)
})

test_that("sf points and polygons give the right coordinates", {
  skip_if_not_installed("sf")
  pts = data.frame(lon = c(10.5, 11.25), lat = c(60.1, 59.8), value = c(100, 200))
  p = sf::st_as_sf(pts, coords = c("lon", "lat"))
  xy = climatrends:::.lonlat_from_sf(p)
  expect_equal(unname(xy[, 1]), c(10.5, 11.25))
  expect_equal(unname(xy[, 2]), c(60.1, 59.8))
  
  sq = sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))))
  pol = sf::st_sf(value = 5, geometry = sf::st_sfc(sq))
  xy = climatrends:::.lonlat_from_sf(pol)
  expect_equal(unname(as.vector(xy)), c(1, 1))
})
