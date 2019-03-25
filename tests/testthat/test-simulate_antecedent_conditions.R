context("test-simulate_antecedent_conditions")

pattern_a <- spatstat::runifpoint(n = 20)
spatstat::marks(pattern_a) <- "a"

pattern_b <- spatstat::runifpoint(n = 100)
spatstat::marks(pattern_b) <- "b"

pattern <- spatstat::superimpose(pattern_a, pattern_b)

test_that("simulate_antecedent_conditions returns nsim raster", {

  null_model <- simulate_antecedent_conditions(x = pattern,
                                               i = "b", j = "a",
                                               nsim = 19)

  expect_length(null_model, n = 19)
})

test_that("simulate_antecedent_conditions does not randomize pattern j", {

  null_model <- simulate_antecedent_conditions(x = pattern,
                                               i = "b", j = "a",
                                               nsim = 19)

  pattern_j <- spatstat::subset.ppp(null_model[[1]], marks == "a")

  check <- all(spatstat::coords(pattern_j) == spatstat::coords(spatstat::subset.ppp(pattern,
                                                                                    marks == "a")))

  expect_true(check)
})

test_that("simulate_antecedent_conditions randomizes pattern i", {

  null_model <- simulate_antecedent_conditions(x = pattern,
                                               i = "b", j = "a",
                                               nsim = 19)

  pattern_i <- spatstat::subset.ppp(null_model[[1]], marks == "b")

  check <- all(spatstat::coords(pattern_i) != spatstat::coords(spatstat::subset.ppp(pattern,
                                                                                    marks == "b")))

  expect_true(check)
})

test_that("simulate_antecedent_conditions returns error", {

  pattern <- spatstat::runifpoint(n = 100)

  expect_error(simulate_antecedent_conditions(x = pattern),
               regexp = "Please provide marked point pattern.")

})
