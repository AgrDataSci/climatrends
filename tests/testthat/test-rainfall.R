context("test-rainfall")
library("nasapower")
library("sf")
# load("tests/test_data.rda")
load("../test_data.rda")

test_that("rain local ok", {
  
  r <- rainfall(rain, day.one = "2013-10-27", span = 15)

  istrue <- all(r == rain_local_ok)
  
  expect_true(istrue)

})


test_that("nasapower and sf ok", {
  
  skip_on_cran()

  ll <- data.frame(lon = lonlat[,1],
                   lat = lonlat[,2])
  
  ll <- sf::st_as_sf(ll, coords = c("lon","lat"))
  
  r <- rainfall(ll,
                day.one = "2013-01-01",
                last.day = "2013-01-10",
                as.sf = FALSE)

  istrue <- all(r == rain_nasapower_ok)

  expect_true(istrue)
  
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

