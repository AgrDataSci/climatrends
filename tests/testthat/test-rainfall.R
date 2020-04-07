context("test-rainfall")
library("nasapower")
# load("tests/test_data.rda")
load("../test_data.rda")

MLDS <- c(rep(9, 2), rep(10, 8))
MLWS <- c(rep(1, 2), rep(0, 8))

test_that("dry equal", {
  r <- rainfall(object = rain,
                day.one = d,
                span = 10)

  ds <- all.equal(MLDS, r$MLDS)
  ws <- all.equal(MLWS, r$MLWS)
  true <- all(ds, ws)
  expect_true(true)
})

realv <- c(7, 7, 4, 10, 10, 10, 1, 1, 1, 0, 0, 0, 11.24, 12.14,
           11.39, 33.86, 34.79, 32.83, 20.35, 20.91, 21.28, 11.24, 
           12.14, 11.39, 84.17, 82.24, 79.53, 6.01, 5.87, 5.3)
test_that("nasapower works", {
  skip_on_cran()
  
  xy <- as.data.frame(lonlat[1:3, ])
  
  r <- rainfall(object = xy,
                day.one = d[1:3,],
                span = 25)

  r <- round(unlist(r), 2)

  true <- all(r == realv)

  expect_true(true)
})


