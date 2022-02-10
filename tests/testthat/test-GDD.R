context("test-GDD")
library("nasapower")
library("sf")
# load("tests/test_data.rda")
load("../test_data.rda")

realv <- c(4,3,4,10,4,6,4,5,5,7)
test_that("right values array method", {
  
  dg <- GDD(object = temp,
            day.one = d, 
            degree.days = 45,
            tbase = 10,
            return.as = "ndays")
  
  dg <- all(realv == dg[, 1])
  
  expect_true(dg)
  
})

realv <- c(11, 11, 11, 11, 11, 
           12, 12, 12, 12, 12)
# return daily gdd
test_that("return gdd", {
  
  gdd <- GDD(object = temp[1:2, , ],
             day.one = d[1:2,], 
             span = 5,
             tbase = 10,
             return.as = "daily")$gdd
  
  gdd <- round(gdd, 0)
  
  istrue <- all(gdd == realv)
  
  expect_true(istrue)
})


realv <- c(7,6,6)
test_that("nasapower works", {
  skip_on_cran()
  
  xy <- as.data.frame(lonlat[1:3,])
  xy <- sf::st_as_sf(xy, coords = c("V1","V2"))
  
  dg <- GDD(object = xy,
            day.one = "2013-10-28",
            last.day = "2013-11-27",
            degree.days = 30, 
            tbase = 5,
            return.as = "ndays")
  
  dg <- all(dg$GDD == realv)
  
  expect_true(dg)

})


test_that("missing degree.days return.as ndays", {
  
  expect_error(
    GDD(object = temp,
        day.one = d, 
        tbase = 10,
        return.as = "ndays")
  )
  
})
