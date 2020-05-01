context("test-temperature")
library("nasapower")
library("sf")
# load("tests/test_data.rda")
load("../test_data.rda")


#save(rain, rain_local_ok, d, lonlat, temp, rain_nasapower_ok, temp_local_ok, temp_local_ts_ok, file = "tests/test_data.rda")


# Test if the function is computing the right values
# for maximum day temperature
test_that("local ok", {
  
  x <- temperature(temp, day.one = "2013-10-27", span = 15)
  
  istrue <- all(x == temp_local_ok)
  
  expect_true(istrue)

})

# also test if timeseries is working
# and for minimum day temperature
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

