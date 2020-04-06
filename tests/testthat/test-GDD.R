context("test-GDD")
library("nasapower")
# load("tests/test_data.rda")
load("../test_data.rda")

realv <- c(5,4,5,11,5,7,5,6,6,8)
test_that("right values array method", {
  dg <- GDD(object = temp,
            day.one = d, 
            degree.days = 45,
            base = 10,
            span = 12)
  
  dg <- all.equal(realv, dg[, 1])
  
  expect_true(dg)
  
})

realv <- c(7,6,6)
test_that("nasapower works", {
  skip_on_cran()
  dg <- GDD(object = lonlat[1:3, ],
            day.one = d[1:3, ],
            degree.days = 30, 
            base = 5,
            span = 20)
  
  dg <- all(dg == realv)
  
  expect_true(dg)
})
