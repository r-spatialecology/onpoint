context("test-summarize_envelope")

set.seed(42)
csr_pattern <- spatstat.random::runifpoint(n = 100)
cluster_pattern <- spatstat.random::rThomas(kappa = 15, scale = 0.05, mu = 5)
regular_pattern <- spatstat.random::rHardcore(beta = 200, R = 0.05)

csr_envelope <- spatstat.core::envelope(csr_pattern, fun = "pcf", nsim = 199,
                                        funargs = list(divisor = "d",
                                                       correction = "Ripley",
                                                       stoyan = 0.25),
                                        verbose = FALSE)

cluster_envelope <- spatstat.core::envelope(cluster_pattern, fun = "pcf", nsim = 199,
                                            funargs = list(divisor = "d",
                                                           correction = "Ripley",
                                                           stoyan = 0.25),
                                            verbose = FALSE)

regular_envelope <- spatstat.core::envelope(regular_pattern, fun = "pcf", nsim = 199,
                                            funargs = list(divisor = "d",
                                                           correction = "Ripley",
                                                           stoyan = 0.25),
                                            verbose = FALSE)

test_that("summarize_envelope returns at two seperated and the total value", {

  result_csr <- summarize_envelope(csr_envelope)
  result_cluster <- summarize_envelope(cluster_envelope)
  result_regular <- summarize_envelope(regular_envelope)

  expect_true(result_csr$result_total == 0)
  expect_true(result_cluster$result_total > result_regular$result_total)

  expect_true(all(c(result_csr$result_above, result_cluster$result_above, result_regular$result_above) >= 0))
  expect_true(all(c(result_csr$result_below, result_cluster$result_below, result_regular$result_below) <= 0))
})

test_that("summarize_envelope runs for data.frame", {

  result_env <- summarize_envelope(cluster_envelope)
  result_df <- summarize_envelope(as.data.frame(cluster_envelope))

  expect_equal(object = result_env$result_total, expected = result_df$result_total)
})

test_that("summarize_envelope runs for exceptions", {

  single_above <- spatstat.core::as.data.frame.fv(csr_envelope)
  single_below <- spatstat.core::as.data.frame.fv(csr_envelope)

  single_above[nrow(single_above), "obs"] <- single_above[nrow(single_above), "hi"] * 1.5
  single_below[nrow(single_below), "obs"] <- single_below[nrow(single_below), "lo"] * 0.5

  result_above <- summarize_envelope(single_above)
  result_below <- summarize_envelope(single_below)

  expect_true(result_above$result_above != 0)
  expect_true(result_below$result_below != 0)
})

test_that("summarize_envelope returns errors", {

  expect_error(summarize_envelope(1:10),
               regexp = "Please provide envelope object or data frame.",
               fixed = TRUE)

  expect_error(summarize_envelope(data.frame(x = 1:10, y = 10:1)),
               regexp = "Data frame must have columns: 'r', 'obs', 'theo', 'lo', 'hi'.",
               fixed = TRUE)
})
