# testthat::context("test-estimate_pcf_fast")

testthat::test_that("estimate_pcf returns spatstat.fv object", {

  pattern <- spatstat.random::runifpoint(n = 100)

  pcf_est <- estimate_pcf_fast(pattern)

  testthat::expect_s3_class(pcf_est, "fv")
})
