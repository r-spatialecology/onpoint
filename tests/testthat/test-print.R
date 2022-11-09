context("test-print")

set.seed(42)
csr_pattern <- spatstat.random::runifpoint(n = 100)

csr_envelope <- spatstat.explore::envelope(csr_pattern, fun = "pcf", nsim = 199,
                                        funargs = list(divisor = "d",
                                                       correction = "Ripley",
                                                       stoyan = 0.25),
                                        verbose = FALSE)

result_csr <- summarize_envelope(csr_envelope)

testthat::test_that("print.env_summarised works", {

  testthat::expect_output(print(result_csr))
  testthat::expect_output(print(result_csr, return_area = TRUE))
})
