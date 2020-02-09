context("test-ETo")
library("nasapower")
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

rv <- c(2.892, 2.921, 2.907)

test_that("nasapower works", {
  skip_on_cran()
  
  ll <- lonlat[1:3, ]
  
  dd <- d[1,]
  
  r <- ETo(object = ll,
           day.one = dd,
           span = 20,
           lat = ll[,2])
  
  r <- round(r[[1]], 3)
  
  r <- all(r == rv)
  
  expect_true(r)
})

test_that("error, non Date object in day.one", {
  expect_error(
    ETo(temp,
        day.one = c(1:10), 
        span = 10,
        lat = rep(0, 10))
    )
})


test_that("accept a tibble", {
  
  coord <- as.data.frame(lonlat)
  coord <- tibble::as_tibble(coord)
  
  ev <- ETo(temp,
           day.one = d,
           span = 10,
           lat = rep(0, 10))
  
  ev <- round(ev[[1]], 3)
  
  ev <- all(e == ev)
  
  expect_true(ev)
  
  
})
