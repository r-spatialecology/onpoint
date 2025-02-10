# context("test-Oest")

test_that("Oest returns fv", {

  input_pattern <- spatstat.random::runifpoint(n = 100)

  result <- Oest(x = input_pattern)

  expect_true(spatstat.geom::is.fv(x = result))

})

test_that("Oest returns error", {

  expect_error(Oest(x = c(1, 2, 3)), regexp = "Please provide ppp.")

})
