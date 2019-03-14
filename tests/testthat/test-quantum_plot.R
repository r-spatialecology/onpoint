context("test-quantum_plot")

test_that("quantum_plot returns ggplot", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  csr_envelope <- spatstat::envelope(pattern,
                                     fun = spatstat::pcf, nsim = 9,
                                     funargs = list(divisor = "d",
                                                    correction = "Ripley"))

  plot <- quantum_plot(csr_envelope, ylab = "g(r)")

  expect_is(plot, "ggplot")
  expect_true(plot$labels$y == "g(r)")
  expect_length(plot$layers, n = 4)
})

test_that("quantum_plot returns ggplot (only quantum)", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  csr_envelope <- spatstat::envelope(pattern,
                                     fun = spatstat::pcf, nsim = 9,
                                     funargs = list(divisor = "d",
                                                    correction = "Ripley"))

  plot <- quantum_plot(csr_envelope, ylab = "g(r)", full_fun = FALSE)

  expect_is(plot, "ggplot")
  expect_length(plot$layers, n = 1)
})

test_that("quantum_plot creates labels if not provided", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  csr_envelope <- spatstat::envelope(pattern,
                                     fun = spatstat::pcf, nsim = 9,
                                     funargs = list(divisor = "d",
                                                    correction = "Ripley"))

  expect_warning(quantum_plot(csr_envelope,
                              labels = c("clustering", "segregation")),
                 regexp = "Not enough labels provided - using 'clustering', 'randomness' and 'segregation'.")
})

test_that("quantum_plot returns error", {

  pattern <- spatstat::rThomas(kappa = 50, scale = 0.025, mu = 5)

  expect_error(quantum_plot(pattern, ylab = "g(r)",
                            labels = c("clustering", "segregation")),
               regexp = "Please provide envelope object or dataframe.")
})
