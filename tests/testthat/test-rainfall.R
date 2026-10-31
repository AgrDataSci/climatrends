context("test-rainfall")
library("nasapower")
library("sf")
# load("tests/test_data.rda")
load("../test_data.rda")

test_that("rain local ok", {
  
  r <- rainfall(rain, day.one = "2013-10-27", span = 15)
  
  # all indices except SDII match the stored values
  keep <- names(r) != "SDII"
  expect_true(all(as.data.frame(r)[keep] == as.data.frame(rain_local_ok)[keep],
                  na.rm = TRUE))
  
  # SDII is the rain in wet days (>= 1 mm) divided by the number of wet days
  idx <- match("2013-10-27", dimnames(rain)[[2]]) + 0:14
  sdii <- apply(rain[, idx, drop = FALSE], 1, function(x) {
    wet <- x[!is.na(x) & x >= 1]
    if (length(wet) == 0) 0 else sum(wet) / length(wet)
  })
  expect_equal(as.vector(r$SDII), as.vector(sdii), tolerance = 1e-8)

})


test_that("nasapower and sf ok", {
  
  skip_on_cran()
  skip_if_offline("power.larc.nasa.gov")

  ll <- data.frame(lon = lonlat[,1],
                   lat = lonlat[,2])
  
  ll <- sf::st_as_sf(ll, coords = c("lon","lat"))
  
  r <- rainfall(ll,
                day.one = "2013-01-01",
                last.day = "2013-01-10",
                as.sf = FALSE)
  
  # the stored values were computed with the old default "PRECTOT", so
  # compare with the indices computed from the same PRECTOTCORR series
  ts <- get_timeseries(ll, day.one = "2013-01-01", last.day = "2013-01-10",
                       pars = "PRECTOTCORR")[[1]]
  expected <- do.call("rbind", lapply(split(ts$value, ts$id), function(x) {
    as.data.frame(rainfall(x))
  }))
  rownames(expected) <- NULL
  r <- as.data.frame(r)
  rownames(r) <- NULL
  
  expect_equal(nrow(r), nrow(ll))
  expect_equal(r, expected)
  expect_true(all(r$Rtotal >= 0))
  
})


test_that("larger span", {
  
  expect_error(
    rainfall(rain_dat,
             day.one = "2013-10-27",
             span = 16)
  )
  
})


test_that("non valid date", {
  
  expect_error(
    rainfall(rain_dat,
             day.one = "2013-02-31",
             span = 11)
  )
  
})

