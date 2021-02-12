context("test-center_l_function")

test_that("center_l_function works for point pattern", {

  input_pattern <- spatstat.core::runifpoint(n = 100)

  result <- center_l_function(input_pattern, correction = "Ripley")

  expect_true(spatstat.geom::is.fv(result))
})

test_that("center_l_function works for fv object", {

  input_pattern <- spatstat.core::runifpoint(n = 100)

  lest <- spatstat.core::Lest(input_pattern)

  result <- center_l_function(lest, correction = "Ripley")

  expect_true(spatstat.geom::is.fv(result))
})

test_that("center_l_function returns error", {

  expect_error(center_l_function(x = c(1, 2, 3)),
               regexp = "Please provide either ppp or fv object.")
})

