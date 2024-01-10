# context("test-plot")

set.seed(42)
csr_pattern <- spatstat.random::runifpoint(n = 100)
cluster_pattern <- spatstat.random::rThomas(kappa = 15, scale = 0.05, mu = 5)
regular_pattern <- spatstat.random::rHardcore(beta = 200, R = 0.05)

csr_envelope <- spatstat.explore::envelope(csr_pattern, fun = "pcf", nsim = 199,
                                        funargs = list(divisor = "d",
                                                       correction = "Ripley",
                                                       stoyan = 0.25),
                                        verbose = FALSE)

cluster_envelope <- spatstat.explore::envelope(cluster_pattern, fun = "pcf", nsim = 199,
                                            funargs = list(divisor = "d",
                                                           correction = "Ripley",
                                                           stoyan = 0.25),
                                            verbose = FALSE)

regular_envelope <- spatstat.explore::envelope(regular_pattern, fun = "pcf", nsim = 199,
                                            funargs = list(divisor = "d",
                                                           correction = "Ripley",
                                                           stoyan = 0.25),
                                            verbose = FALSE)

result_csr <- summarize_envelope(csr_envelope)
result_cluster <- summarize_envelope(cluster_envelope)
result_regular <- summarize_envelope(regular_envelope)

test_that("plot returns a ggplot object", {

  gg_csr <- plot(result_csr)
  gg_cluster <- plot(result_cluster)
  gg_regular <- plot(result_regular)

  expect_s3_class(gg_csr, class = "ggplot")
  expect_s3_class(gg_cluster, class = "ggplot")
  expect_s3_class(gg_regular, class = "ggplot")
})

test_that("plot returns warning of only 1 colour is provided", {

  expect_warning(object = plot(result_csr, col = "green"),
                 regexp = "Please provide two colours for the polygons. Setting to default.")
})
