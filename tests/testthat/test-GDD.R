context("test-GDD")
library("nasapower")
library("sf")
# load("tests/test_data.rda")
load("../test_data.rda")

realv <- c(5,4,5,11,5,7,5,6,6,8)
test_that("right values array method", {
  
  dg <- GDD(object = temp,
            day.one = d, 
            degree.days = 45,
            base = 10,
            span = 12)
  
  dg <- all(realv == dg[, 1])
  
  expect_true(dg)
  
})

realv <- c(7,6,6)
test_that("nasapower works", {
  skip_on_cran()
  
  xy <- as.data.frame(lonlat[1:3,])
  xy <- st_as_sf(xy, coords = c("V1","V2"))
  
  dg <- GDD(object = xy,
            day.one = "2013-10-28",
            last.day = "2013-11-27",
            degree.days = 30, 
            base = 5)
  
  dg <- all(dg$GDD == realv)
  
  expect_true(dg)

})
