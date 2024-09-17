# context("test-rheteroppp")

test_that("rheteroppp returns ppp", {

  input_pattern <- spatstat.random::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)

  result <- rheteroppp(input_pattern, nsim = 9)

  n <- vapply(result, function(x) x$n, FUN.VALUE = numeric(1))

  expect_length(result, n = 9)
  expect_true(all(sapply(result, class) == "ppp"))
  expect_true(!all(n == input_pattern$n))
})

test_that("fix_n is working for rheteroppp", {

  input_pattern <- spatstat.random::rpoispp(lambda = function(x , y) {100 * exp(-3 * x)}, nsim = 1)

  result <- rheteroppp(input_pattern, nsim = 9, fix_n = TRUE)

  n <- vapply(result, function(x) x$n, FUN.VALUE = numeric(1))

  expect_length(result, n = 9)
  expect_true(all(sapply(result, class) == "ppp"))
  expect_true(all(n == input_pattern$n))
})

test_that("rheteroppp returns error", {

  expect_error(rheteroppp(c(1, 2, 3 ), nsim = 9),
               regexp = "Please provide ppp object.")

})
