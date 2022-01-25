context("test-estimate_o_ring")

test_that("estimate_o_ring returns fv", {

  input_pattern <- spatstat.random::runifpoint(n = 100)

  result <- estimate_o_ring(x = input_pattern)

  expect_true(spatstat.geom::is.fv(x = result))
})

test_that("estimate_o_ring returns error", {

  expect_error(estimate_o_ring(x = c(1, 2, 3)),
               regexp = "Please provide ppp.")
})
