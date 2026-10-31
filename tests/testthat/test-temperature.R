context("test-temperature")
library("nasapower")
library("sf")
# load("tests/test_data.rda")
load("../test_data.rda")

# Test if the function is computing the right values
test_that("local ok", {
  
  x <- temperature(temp, day.one = "2013-10-27", span = 15)
  
  # all indices except DTR match the stored values
  keep <- names(x) != "DTR"
  expect_true(all(as.data.frame(x)[keep] == as.data.frame(temp_local_ok)[keep], 
                  na.rm = TRUE))
  
  # DTR is the mean of tmax - tmin, not truncated to an integer
  idx <- match("2013-10-27", dimnames(temp)[[2]]) + 0:14
  dtr <- rowMeans(temp[, idx, 1] - temp[, idx, 2], na.rm = TRUE)
  expect_equal(as.vector(x$DTR), as.vector(dtr), tolerance = 1e-8)
  expect_type(x$DTR, "double")

})

# also test if timeseries gives the correct values
test_that("timeseries", {
  
  x <- temperature(temp, 
                   day.one = "2013-10-27", 
                   last.day = "2013-11-10", 
                   timeseries = TRUE, 
                   intervals = 7)
  
  istrue <- all(x == temp_local_ts_ok)
  
  expect_true(istrue)

})


realv <- c(17.13, 17.43, 17.50)
test_that("sf method works", {
  
  skip_on_cran()
  
  xy <- as.data.frame(lonlat[1:3,])
  xy <- sf::st_as_sf(xy, coords = c("V1","V2"))
  
  r <- temperature(object = xy, 
                   day.one = "2013-10-28",
                   span = 25,
                   pars = c("T10M_MAX","T10M_MIN"))
  
  expect_equal(round(r$maxDT, 2), realv, tolerance = 0.02)
})


realv <- c(15.77, 17.13, 10.37, 16.42, 17.43, 10.4, 16.49, 17.5, 10.22)
test_that("sf method with timeseries", {
  skip_on_cran()
  
  xy <- as.data.frame(lonlat[1:3,])
  xy <- sf::st_as_sf(xy, coords = c("V1","V2"))
  
  r <- temperature(object = xy, 
                   day.one = "2013-10-28",
                   last.day = "2013-11-22",
                   timeseries = TRUE,
                   intervals = 8,
                   pars = c("T10M_MAX","T10M_MIN"))
  
  expect_equal(round(r$value[r$index == "maxDT"], 2), realv, tolerance = 0.02)

})

