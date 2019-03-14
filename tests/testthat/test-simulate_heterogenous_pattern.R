context("test-simulate_heterogenous_pattern")

test_that("simulate_heterogenous_pattern returns ppp", {

  input_pattern <- spatstat::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)

  result <- simulate_heterogenous_pattern(input_pattern, nsim = 9)

  expect_length(result, n = 9)
  expect_true(all(sapply(result, class) == "ppp"))
})

test_that("simulate_heterogenous_pattern returns error", {

  expect_error(simulate_heterogenous_pattern(c(1, 2, 3 ), nsim = 9),
               regexp = "Please provide ppp object.")

})
