context("test-summarise_envelope")

set.seed(42)
csr_pattern <- spatstat::runifpoint(n = 100)
cluster_pattern <- spatstat::rThomas(kappa = 15, scale = 0.05, mu = 5)
regular_pattern <- spatstat::rHardcore(beta = 200, R = 0.05)

csr_envelope <- spatstat::envelope(csr_pattern, fun = "pcf", nsim = 199,
                                   funargs = list(divisor = "d",
                                                  correction = "Ripley",
                                                  stoyan = 0.25),
                                   verbose = FALSE)

cluster_envelope <- spatstat::envelope(cluster_pattern, fun = "pcf", nsim = 199,
                                       funargs = list(divisor = "d",
                                                      correction = "Ripley",
                                                      stoyan = 0.25),
                                       verbose = FALSE)

regular_envelope <- spatstat::envelope(regular_pattern, fun = "pcf", nsim = 199,
                                       funargs = list(divisor = "d",
                                                      correction = "Ripley",
                                                      stoyan = 0.25),
                                       verbose = FALSE)

test_that("summarise_envelope returns at two seperated and the total value", {

  result_csr <- summarise_envelope(csr_envelope)
  result_cluster <- summarise_envelope(cluster_envelope)
  result_regular <- summarise_envelope(regular_envelope)

  expect_true(result_csr$result_total == 0)
  expect_true(result_cluster$result_total > result_regular$result_total)

  expect_true(all(c(result_csr$result_above, result_cluster$result_above, result_regular$result_above) >= 0))
  expect_true(all(c(result_csr$result_below, result_cluster$result_below, result_regular$result_below) <= 0))
})

test_that("summarise_envelope runs for data.frame", {

  result_env <- summarise_envelope(cluster_envelope)
  result_df <- summarise_envelope(as.data.frame(cluster_envelope))

  expect_equal(object = result_env$result_total, expected = result_df$result_total)
})


test_that("summarise_envelope returns errors", {

  expect_error(summarise_envelope(1:10),
               regexp = "Please provide envelope object or data frame.",
               fixed = TRUE)

  expect_error(summarise_envelope(data.frame(x = 1:10, y = 10:1)),
               regexp = "Data frame must have columns: 'r', 'obs', 'theo', 'lo', 'hi'.",
               fixed = TRUE)
})
