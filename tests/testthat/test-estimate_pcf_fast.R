testthat::context("test-estimate_pcf_fast")

testthat::test_that("estimate_pcf returns spatstat.fv object", {

  pattern <- spatstat.core::runifpoint(n = 100)

  pcf_est <- estimate_pcf_fast(pattern)

  testthat::expect_is(pcf_est, "fv")
})
