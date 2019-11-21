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

test_that("summarise_envelope returns at two seperated values if seperated = TRUE", {

  result_csr <- summarise_envelope(csr_envelope, seperated = TRUE)
  result_cluster <- summarise_envelope(cluster_envelope, seperated = TRUE)
  result_regular <- summarise_envelope(regular_envelope, seperated = TRUE)

  expect_length(result_csr, n = 2)
  expect_length(result_cluster, n = 2)
  expect_length(result_regular, n = 2)

  expect_true(all(c(result_csr[1], result_cluster[1], result_regular[1]) >= 0))
  expect_true(all(c(result_csr[2], result_cluster[2], result_regular[2]) <= 0))
})

test_that("summarise_envelope returns one value if seperated = FALSE", {

  result_csr <- summarise_envelope(csr_envelope, seperated = FALSE)
  result_cluster <- summarise_envelope(cluster_envelope, seperated = FALSE)
  result_regular <- summarise_envelope(regular_envelope, seperated = FALSE)

  expect_length(result_csr, n = 1)
  expect_length(result_cluster, n = 1)
  expect_length(result_regular, n = 1)
})
