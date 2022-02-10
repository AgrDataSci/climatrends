context("test-ETo")
library("nasapower")
library("sf")
# load("tests/test_data.rda")
load("../test_data.rda")


e <- c(4.752, 4.898, 4.66, 3.948, 4.536,
       4.231, 4.535, 4.39, 4.412, 4.124)
test_that("give correct values", {
  
  ev <- ETo(temp,
        day.one = d, 
        span = 10,
        lat = rep(0, 10))
    
  
  ev <- round(ev[[1]], 3)
  
  ev <- all(e == ev)
  
  expect_true(ev)
  
})

rv <- c(2.861, 2.891, 2.879)
test_that("nasapower with default method", {
  skip_on_cran()
  
  ll <- as.data.frame(lonlat[1:3, ])
  
  r <- ETo(object = ll,
           day.one = "2013-10-28",
           last.day = "2013-11-17",
           lat = ll[,2])
  
  r <- round(r$ETo, 3)
  
  r <- all(r == rv)
  
  expect_true(r)
  
})

rv <- c(2.922, 2.953)
test_that("accept and return a sf", {
  skip_on_cran()
  
  ll <- as.data.frame(lonlat[1:2, ])
  ll <- st_as_sf(ll, coords = c("V1","V2"))
  
  r <- ETo(object = ll,
           day.one = "2013-10-28",
           span = 20, 
           as.sf = TRUE,
           pars = c("T10M_MAX", "T10M_MIN"))
  
  is_sf <- "sf" %in% class(r)
  
  r <- round(r$ETo, 3)
  
  r <- all(r == rv)

  expect_equal(r, is_sf)
  
})


test_that("error, more than 2 colunms default method", {
  xy <- as.data.frame(cbind(lonlat, lonlat))
  expect_error(
    ETo(xy,
        day.one = d, 
        span = 10,
        lat = rep(0, 10))
  )
})

