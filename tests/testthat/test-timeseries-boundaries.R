testthat::test_that("staggered windows use the actual last requested day", {
  starts = as.Date(c("2020-01-01", "2020-01-20"))
  x = climatrends:::.st_span(day.one = starts, span = c(40L, 2L))
  testthat::expect_equal(x$dates[1], as.Date("2020-01-01"))
  testthat::expect_equal(x$dates[2], as.Date("2020-02-09"))
  testthat::expect_equal(x$maxend, max(x$end))
  testthat::expect_equal(x$span, c(40L, 2L))
})

testthat::test_that("days.before does not truncate requested dates", {
  x = climatrends:::.st_span(day.one = as.Date("2020-03-01"),
                            span = 3L, days.before = 2L)
  testthat::expect_equal(x$dates, as.Date(c("2020-02-28", "2020-03-03")))
  testthat::expect_equal(x$span, 5L)
})
