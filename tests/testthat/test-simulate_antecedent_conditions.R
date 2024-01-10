# context("test-simulate_antecedent_conditions")

pattern_a <- spatstat.random::runifpoint(n = 20)
spatstat.geom::marks(pattern_a) <- "a"

pattern_b <- spatstat.random::runifpoint(n = 100)
spatstat.geom::marks(pattern_b) <- "b"

pattern <- spatstat.geom::superimpose(pattern_a, pattern_b)

test_that("simulate_antecedent_conditions returns nsim", {

  null_model <- simulate_antecedent_conditions(x = pattern,
                                               i = "b", j = "a",
                                               nsim = 19)

  expect_length(null_model, n = 19)
})

test_that("simulate_antecedent_conditions does not randomize pattern j", {

  null_model <- simulate_antecedent_conditions(x = pattern,
                                               i = "b", j = "a",
                                               nsim = 19)

  pattern_j <- spatstat.geom::subset.ppp(null_model[[1]], marks == "a")

  check <- all(spatstat.geom::coords(pattern_j) ==
                 spatstat.geom::coords(spatstat.geom::subset.ppp(pattern, marks == "a")))

  expect_true(check)
})

test_that("simulate_antecedent_conditions randomizes pattern i", {

  null_model <- simulate_antecedent_conditions(x = pattern,
                                               i = "b", j = "a",
                                               nsim = 19)

  pattern_i <- spatstat.geom::subset.ppp(null_model[[1]], marks == "b")

  check <- all(spatstat.geom::coords(pattern_i) !=
                 spatstat.geom::coords(spatstat.geom::subset.ppp(pattern, marks == "b")))

  expect_true(check)
})

test_that("simulate_antecedent_conditions uses heterogenous process", {

  null_model <- simulate_antecedent_conditions(x = pattern,
                                               i = "b", j = "a",
                                               nsim = 19,
                                               heterogenous = TRUE)

  expect_length(null_model, n = 19)
})

test_that("simulate_antecedent_conditions returns error", {

  pattern_unmarked <- spatstat.random::runifpoint(n = 100)

  expect_error(simulate_antecedent_conditions(x = pattern_unmarked),
               regexp = "Please provide marked point pattern.")

  expect_error(simulate_antecedent_conditions(x = pattern, i = "a", j = "c"),
               regexp = "i and j must be marks of x.")

  spatstat.geom::marks(pattern[1]) <- "c"

  expect_error(simulate_antecedent_conditions(x = pattern, i = "a", j = "b"),
               regexp = "Currently only bivariate point patterns are supported.")
})
