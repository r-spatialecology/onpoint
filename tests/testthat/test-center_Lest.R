# context("test-center_Lest")

test_that("center_Lest works for point pattern", {

  input_pattern <- spatstat.random::runifpoint(n = 100)

  result <- center_Lest(input_pattern, correction = "Ripley")

  expect_true(spatstat.geom::is.fv(result))
})

test_that("center_Lest works for fv object", {

  input_pattern <- spatstat.random::runifpoint(n = 100)

  lest <- spatstat.explore::Lest(input_pattern)

  result <- center_Lest(lest, correction = "Ripley")

  expect_true(spatstat.geom::is.fv(result))
})

test_that("center_Lest returns error", {

  expect_error(center_Lest(x = c(1, 2, 3)),
               regexp = "Please provide either ppp or fv object.")
})
