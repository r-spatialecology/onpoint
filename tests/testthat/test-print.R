context("test-print")

set.seed(42)
csr_pattern <- spatstat.core::runifpoint(n = 100)

csr_envelope <- spatstat.core::envelope(csr_pattern, fun = "pcf", nsim = 199,
                                        funargs = list(divisor = "d",
                                                       correction = "Ripley",
                                                       stoyan = 0.25),
                                        verbose = FALSE)

result_csr <- summarise_envelope(csr_envelope)

testthat::test_that("print.env_summarised works", {

  testthat::expect_output(print(result_csr))
  testthat::expect_output(print(result_csr, return_area = TRUE))
})
