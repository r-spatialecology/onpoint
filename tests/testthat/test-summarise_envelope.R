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

test_that("summarise_envelope returns value", {

  result_csr <- summarise_envelope(csr_envelope)
  result_cluster <- summarise_envelope(cluster_envelope)
  result_regular <- summarise_envelope(regular_envelope)

  expect_lte(object = result_regular, expected = result_csr)
  expect_gte(object = result_cluster, expected = result_csr)
})
