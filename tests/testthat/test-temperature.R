context("test-temperature")
library("nasapower")
library("sf")
# load("tests/test_data.rda")
load("../test_data.rda")


maxDT <- c(33.5, 34.9, 32, 27.5, 32.8, 27.4, 32.6, 31.6, 32.3, 28.5)
minNT <- c(8.2, 9.1, 8.2, 1.5, 5.7, 6, 5.7, 4.6, 4.3, 2.8)

# Test if the function is computing the right values
# for maximum day temperature
test_that("day equal", {
  x <- temperature(object = temp, 
                   day.one = d,
                   span = 8)
  
  dt <- all.equal(maxDT, x$maxDT)
  
  expect_equal(dt, TRUE)
})

# and for minimum day temperature
test_that("night equal", {
  x <- temperature(object = temp, 
                   day.one = d,
                   span = 8)
  
  nt <- all.equal(minNT, x$minNT)
  
  expect_equal(nt, TRUE)
})

# also test if timeseries is working
# and for minimum day temperature
test_that("timeseries", {
  
  x <- temperature(object = temp, 
                   day.one = d[1,],
                   span = 10,
                   timeseries = TRUE,
                   intervals = 5)
  
  maxDT <- c(33.5, 33.5, 34.9, 34.9, 32, 32, 27.5, 
             27.1, 32.8, 32.3, 27.6, 27.1, 32.8, 
             32.3, 31.9, 31.1, 32.6, 31.7, 28.7, 28.2)
  
  x <- as.vector(t(x[x$index=="maxDT", "value"]))
  
  dt <- all.equal(maxDT, x)
  
  expect_equal(dt, TRUE)
})

test_that("nasapower works default", {
  skip_on_cran()
  r <- temperature(object = lonlat, 
                   day.one = d,
                   span = 25)
  
  r <- as.vector(apply(r, 1, is.na))
  
  r <- sum(r) == 0
  
  expect_equal(r, TRUE)
})


realv <- c(17.13, 17.43, 17.50)
test_that("sf method works", {
  skip_on_cran()
  
  xy <- as.data.frame(lonlat[1:3,])
  xy <- st_as_sf(xy, coords = c("V1","V2"))
  day <- as.data.frame(d[1:3, ])
  
  r <- temperature(object = xy, 
                   day.one = day,
                   span = 25,
                   pars = c("T10M_MAX","T10M_MIN"))
  
  r <- round(r$maxDT, 2)
  
  r <- all(r == realv)
  
  expect_true(r)
})


realv <- c(15.77, 17.13, 10.37, 16.42, 17.43, 10.4, 16.49, 17.5, 10.22)
test_that("sf method with timeseries", {
  skip_on_cran()
  
  xy <- as.data.frame(lonlat[1:3,])
  xy <- st_as_sf(xy, coords = c("V1","V2"))
  day <- as.data.frame(d[1:3, ])
  
  r <- temperature(object = xy, 
                   day.one = day,
                   span = 25,
                   timeseries = TRUE,
                   intervals = 8,
                   pars = c("T10M_MAX","T10M_MIN"))
  
  r <- round(r$value[r$index == "maxDT"], 2)
  
  r <- all(r == realv)
  
  expect_true(r)
})

