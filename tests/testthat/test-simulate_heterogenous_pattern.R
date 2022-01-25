context("test-simulate_heterogenous_pattern")

test_that("simulate_heterogenous_pattern returns ppp", {

  input_pattern <- spatstat.random::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)

  result <- simulate_heterogenous_pattern(input_pattern, nsim = 9)

  n <- vapply(result, function(x) x$n, FUN.VALUE = numeric(1))

  expect_length(result, n = 9)
  expect_true(all(sapply(result, class) == "ppp"))
  expect_true(!all(n == input_pattern$n))
})

test_that("fix_n is working for simulate_heterogenous_pattern", {

  input_pattern <- spatstat.random::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)

  result <- simulate_heterogenous_pattern(input_pattern, nsim = 9, fix_n = TRUE)

  n <- vapply(result, function(x) x$n, FUN.VALUE = numeric(1))

  expect_length(result, n = 9)
  expect_true(all(sapply(result, class) == "ppp"))
  expect_true(all(n == input_pattern$n))
})

test_that("simulate_heterogenous_pattern returns error", {

  expect_error(simulate_heterogenous_pattern(c(1, 2, 3 ), nsim = 9),
               regexp = "Please provide ppp object.")

})
